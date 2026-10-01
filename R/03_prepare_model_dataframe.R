# ============================================================
# 02_prepare_covariates.R
# ENVIRONMENTAL + TRAVEL + CONTINENTAL COVARIATE EXTRACTION
# UGANDA VECTOR ABUNDANCE MODELLING
# ============================================================

source("functions/helpers.R")

library(tidyverse)
library(terra)
library(geodata)

# ------------------------------------------------------------
# 1. FILE PATHS
# ------------------------------------------------------------

input <- "data/processed/ento_clean.csv"
env_file <- "data/raw/covariates2(1).tif"
travel_file <- "data/raw/rescale_travel.tif"
continental_file <- "data/raw/continental/uganda_preds_p.tif"
cache <- "data/raw/.geodata"
out <- "data/processed/model_data_environment.csv"

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  cache,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. READ CLEANED ENTOMOLOGICAL DATA
# ------------------------------------------------------------

if (!file.exists(input)) {
  stop(
    "Cleaned entomological dataset not found: ",
    input,
    "\nRun R/01_clean_data.R first."
  )
}

d <- read_csv(
  input,
  show_col_types = FALSE
)

check_required(
  d,
  c(
    "observation_id",
    "longitude",
    "latitude",
    "an_gambiae_total",
    "site_id",
    "site_month_id",
    "effort"
  )
)

# ------------------------------------------------------------
# 3. ENVIRONMENTAL RASTER
# ------------------------------------------------------------
# Band 1 = tmax
# Band 2 = tmin
# Band 3 = precipitation

if (!file.exists(env_file)) {
  stop(
    "Environmental raster not found: ",
    env_file
  )
}

env <- rast(env_file)

if (nlyr(env) < 3) {
  stop(
    "Environmental raster must contain at least 3 bands."
  )
}

env <- env[[1:3]]

names(env) <- c(
  "tmax",
  "tmin",
  "precip"
)

# ------------------------------------------------------------
# 4. VALID OBSERVATION COORDINATES
# ------------------------------------------------------------

valid <- d |>
  filter(
    is.finite(longitude),
    is.finite(latitude)
  )

if (!nrow(valid)) {
  stop(
    "No valid working coordinates available for extraction."
  )
}

pts <- vect(
  valid,
  geom = c(
    "longitude",
    "latitude"
  ),
  crs = "EPSG:4326"
)

# ------------------------------------------------------------
# 5. ENVIRONMENTAL COVARIATES
# ------------------------------------------------------------

env_values <- terra::extract(
  env,
  pts,
  ID = FALSE
) |>
  as_tibble()

model_data <- d |>
  left_join(
    bind_cols(
      valid |>
        select(observation_id),
      env_values
    ),
    by = "observation_id"
  )

# ------------------------------------------------------------
# 6. TRAVEL ACCESSIBILITY
# ------------------------------------------------------------
#
# Travel time to cities is converted to an accessibility index:
#
#   1 = highest accessibility
#   0 = lowest accessibility
#
# The raster is created automatically if it does not exist.
# It is then aligned to the environmental modelling grid.
# ------------------------------------------------------------

if (!file.exists(travel_file)) {
  
  message(
    "Travel raster not found. Preparing travel accessibility..."
  )
  
  uga <- geodata::gadm(
    "UGA",
    level = 0,
    path = cache
  )
  
  travel <- geodata::travel_time(
    to = "city",
    size = 1,
    path = cache
  )
  
  # Ensure Uganda boundary uses the same CRS
  # as the environmental raster.
  uga <- terra::project(
    uga,
    env
  )
  
  travel <- travel |>
    crop(env[[1]]) |>
    mask(uga) |>
    resample(
      env[[1]],
      method = "bilinear"
    )
  
  max_travel <- global(
    travel,
    "max",
    na.rm = TRUE
  )[1, 1]
  
  if (!is.finite(max_travel) || max_travel <= 0) {
    stop(
      "Travel raster has no positive finite values."
    )
  }
  
  travel <- 1 - (
    travel / max_travel
  )
  
  names(travel) <- "travel"
  
  writeRaster(
    travel,
    travel_file,
    overwrite = TRUE
  )
  
  message(
    "Saved travel-accessibility raster: ",
    travel_file
  )
  
} else {
  
  message(
    "Using existing travel raster: ",
    travel_file
  )
  
  travel <- rast(
    travel_file
  ) |>
    resample(
      env[[1]],
      method = "bilinear"
    )
  
  names(travel) <- "travel"
}

# ------------------------------------------------------------
# EXTRACT TRAVEL ACCESSIBILITY
# ------------------------------------------------------------

travel_values <- terra::extract(
  travel,
  pts,
  ID = FALSE
)[[1]]

model_data <- model_data |>
  select(
    -any_of("travel")
  ) |>
  left_join(
    tibble(
      observation_id = valid$observation_id,
      travel = travel_values
    ),
    by = "observation_id"
  )

# ------------------------------------------------------------
# 7. CONTINENTAL AN. GAMBIAE OFFSET FOR M3
# ------------------------------------------------------------
#
# Band 4 = An. gambiae continental prediction surface.
#
# Only the Gambiae surface is used as the M3 ecological offset.
#
# The continental raster is externally supplied and is never
# fabricated by this workflow.
# ------------------------------------------------------------

if (!file.exists(continental_file)) {
  
  model_data$log_continental_offset <- NA_real_
  
  warning(
    "Continental raster not found: ",
    continental_file,
    "\nM1 and M2 can be prepared, but M3 requires this raster."
  )
  
} else {
  
  continental <- rast(
    continental_file
  )
  
  if (nlyr(continental) < 4) {
    stop(
      "Continental raster must contain at least 4 bands."
    )
  }
  
  # ----------------------------------------------------------
  # An. gambiae = Band 4
  # ----------------------------------------------------------
  
  continental_gambiae <- continental[[4]]
  
  names(
    continental_gambiae
  ) <- "continental_gambiae"
  
  # ----------------------------------------------------------
  # Align to environmental modelling grid
  # ----------------------------------------------------------
  
  continental_gambiae <- resample(
    continental_gambiae,
    env[[1]],
    method = "bilinear"
  )
  
  # ----------------------------------------------------------
  # Ensure positive finite values for log transformation
  # ----------------------------------------------------------
  
  continental_gambiae <- ifel(
    continental_gambiae > 0 &
      is.finite(continental_gambiae),
    continental_gambiae,
    1e-6
  )
  
  # ----------------------------------------------------------
  # Log-transform for use as M3 offset
  # ----------------------------------------------------------
  
  log_offset <- log(
    continental_gambiae
  )
  
  names(
    log_offset
  ) <- "log_continental_offset"
  
  # ----------------------------------------------------------
  # Extract offset at observation locations
  # ----------------------------------------------------------
  
  offset_values <- terra::extract(
    log_offset,
    pts,
    ID = FALSE
  )[[1]]
  
  model_data <- model_data |>
    select(
      -any_of("log_continental_offset")
    ) |>
    left_join(
      tibble(
        observation_id = valid$observation_id,
        log_continental_offset = offset_values
      ),
      by = "observation_id"
    )
  
  # ----------------------------------------------------------
  # Save continental Gambiae surfaces
  # ----------------------------------------------------------
  
  writeRaster(
    continental_gambiae,
    "data/processed/continental_gambiae_1km.tif",
    overwrite = TRUE
  )
  
  writeRaster(
    log_offset,
    "data/processed/log_continental_offset_1km.tif",
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# 8. SAMPLING INTENSITY AND TEMPORAL TREND
# ------------------------------------------------------------

model_data <- model_data |>
  mutate(
    log_effort = log1p(effort),
    year_c = year - mean(
      year,
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------
# 9. COVARIATE AVAILABILITY SUMMARY
# ------------------------------------------------------------

qa <- model_data |>
  summarise(
    observations = n(),
    
    valid_gps =
      sum(
        is.finite(longitude) &
          is.finite(latitude)
      ),
    
    tmax =
      sum(!is.na(tmax)),
    
    tmin =
      sum(!is.na(tmin)),
    
    precip =
      sum(!is.na(precip)),
    
    travel =
      sum(!is.na(travel)),
    
    log_effort =
      sum(!is.na(log_effort)),
    
    year_c =
      sum(!is.na(year_c)),
    
    continental_offset =
      sum(!is.na(log_continental_offset))
  )

print(qa)

# ------------------------------------------------------------
# 10. SAVE MODELLING DATA AND RASTERS
# ------------------------------------------------------------

write_csv(
  model_data,
  out,
  na = ""
)

writeRaster(
  env,
  "data/processed/environment_1km.tif",
  overwrite = TRUE
)

message(
  "Saved modelling dataset: ",
  out
)

# ------------------------------------------------------------
# 11. M3 READINESS CHECK
# ------------------------------------------------------------

n_continental <- sum(
  !is.na(
    model_data$log_continental_offset
  )
)

if (n_continental == 0) {
  
  warning(
    "M3 is NOT ready: no continental An. gambiae offset ",
    "values were extracted. Supply ",
    continental_file,
    " and rerun R/02_prepare_covariates.R."
  )
  
} else {
  
  message(
    "M3 An. gambiae continental offset prepared for ",
    n_continental,
    " observations."
  )
}

# ============================================================
# END OF SCRIPT
# ============================================================