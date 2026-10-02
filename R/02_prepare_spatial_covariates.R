

# 02_prepare_spatial_covariates.R

options(timeout = 600)

suppressPackageStartupMessages({
  library(tidyverse)
  library(terra)
  library(geodata)
})

# 1. FILES AND DIRECTORIES ----------------------------------

input <- "data/processed/ento_clean.csv"
continental_source <- "data/raw/continental/uganda_preds_p.tif"

out_dir <- "data/processed"
bioclim_dir <- "data/raw/bioclim"
cache <- "data/raw/.geodata"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bioclim_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache, recursive = TRUE, showWarnings = FALSE)

uganda_extent <- ext(29.5, 35.1, -1.5, 4.3)

# 2. READ ENTOMOLOGICAL DATA -------------------------------

d <- read_csv(input, show_col_types = FALSE) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    year = as.numeric(year)
  )

cat("Input observations:", nrow(d), "\n")

# 3. PREPARE BIOCLIM DATA -----------------------------------

bio <- geodata::worldclim_country(
  country = "Uganda",
  var = "bio",
  res = 0.5,
  version = "2.1",
  path = bioclim_dir
)

env <- crop(
  bio[[c(1, 5, 6, 12, 13, 14)]],
  uganda_extent
)

names(env) <- c(
  "bio1", "bio5", "bio6",
  "bio12", "bio13", "bio14"
)

writeRaster(
  env,
  file.path(out_dir, "environment_1km.tif"),
  overwrite = TRUE
)

cat("BIOCLIM raster prepared.\n")

# 4. PREPARE TRAVEL ACCESSIBILITY ---------------------------

uga <- geodata::gadm(
  country = "UGA",
  level = 0,
  path = cache
)

travel_raw <- geodata::travel_time(
  to = "city",
  size = 5,
  path = cache
)

uga_travel <- project(uga, crs(travel_raw))

travel_uga <- crop(travel_raw, uga_travel)
travel_uga <- mask(travel_uga, uga_travel)

travel <- project(
  travel_uga,
  env[[1]],
  method = "bilinear"
)

max_travel <- global(
  travel,
  "max",
  na.rm = TRUE
)[1, 1]

travel <- clamp(
  1 - (travel / max_travel),
  lower = 0,
  upper = 1,
  values = TRUE
)

names(travel) <- "travel"

writeRaster(
  travel,
  file.path(out_dir, "travel_accessibility_1km.tif"),
  overwrite = TRUE
)

cat("Travel accessibility raster prepared.\n")

# 5. PREPARE CONTINENTAL AN. GAMBIAE OFFSET -----------------

continental <- project(
  rast(continental_source)[[4]],
  env[[1]],
  method = "bilinear"
)

continental <- ifel(
  !is.finite(continental) | continental <= 0,
  1e-6,
  continental
)

names(continental) <- "continental_gambiae"

log_offset <- log(continental)
names(log_offset) <- "log_continental_offset"

writeRaster(
  continental,
  file.path(out_dir, "continental_gambiae_1km.tif"),
  overwrite = TRUE
)

writeRaster(
  log_offset,
  file.path(out_dir, "log_continental_offset_1km.tif"),
  overwrite = TRUE
)

cat("Continental offset prepared.\n")

# 6. EXTRACT SPATIAL COVARIATES -----------------------------

valid <- d %>%
  filter(
    is.finite(longitude),
    is.finite(latitude),
    between(longitude, 29.5, 35.1),
    between(latitude, -1.5, 4.3)
  )

cat("Observations with valid coordinates:", nrow(valid), "\n")

pts <- vect(
  valid,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

pts <- project(pts, crs(env))

covariates <- c(env, travel, log_offset)

extracted <- terra::extract(
  covariates,
  pts,
  ID = FALSE
)

valid_covariates <- bind_cols(
  valid %>% dplyr::select(observation_id),
  as_tibble(extracted)
)

# 7. MERGE AND PREPARE MODEL DATA ---------------------------

model_data <- d %>%
  left_join(
    valid_covariates,
    by = "observation_id"
  ) %>%
  mutate(
    year_c = year - mean(year, na.rm = TRUE)
  )

# 8. SAVE OUTPUT --------------------------------------------

write_csv(
  model_data,
  file.path(out_dir, "model_data_environment.csv"),
  na = ""
)

cat("\nSpatial covariate preparation completed.\n")
cat("Total observations:", nrow(model_data), "\n")
cat("Observations with valid coordinates:", nrow(valid), "\n")
cat("Output: data/processed/model_data_environment.csv\n")




