

# 03_prepare_model_data.R
# Extract spatial covariates and prepare model datasets

suppressPackageStartupMessages({
  library(tidyverse)
  library(terra)
})

# 1. FILES ---------------------------------------------------

data_file <- "data/processed/ento_clean.csv"
out_dir <- "data/processed"

# 2. LOAD SPATIAL COVARIATES ---------------------------------

monthly_env <- rast(file.path(out_dir, "monthly_climate_uganda_1km.tif"))
environment <- rast(file.path(out_dir, "environment_1km.tif"))
travel <- rast(file.path(out_dir, "travel_accessibility_1km.tif"))
log_offset <- rast(file.path(out_dir, "log_continental_offset_1km.tif"))

# Use monthly climate raster as spatial template
template <- monthly_env[[1]]

# Check monthly climate layers
expected_layers <- c(
  paste0("tmin_", month.abb),
  paste0("tmax_", month.abb),
  paste0("precip_", month.abb)
)

if (!all(expected_layers %in% names(monthly_env))) {
  stop("Monthly climate raster has missing or incorrectly named layers.")
}

# Align supporting rasters with template
travel <- project(travel, template, method = "bilinear")
log_offset <- project(log_offset, template, method = "bilinear")

names(travel) <- "travel"
names(log_offset) <- "log_continental_offset"

# 3. CLEAN OBSERVATIONS --------------------------------------

d <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    an_gambiae_total = as.numeric(an_gambiae_total),
    month_number = as.integer(month_number)
  ) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(an_gambiae_total),
    between(longitude, 29.5, 35.1),
    between(latitude, -1.5, 4.3)
  )

# Convert observations to spatial points
pts <- vect(
  d,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

pts <- project(pts, crs(template))

# Assign raster grid cells
d$cell_number <- cellFromXY(template, crds(pts))

# Remove observations outside raster coverage
d <- d %>%
  filter(!is.na(cell_number))

pts <- pts[!is.na(d$cell_number), ]

cat("Valid observations:", nrow(d), "\n")

# 4. PREPARE COMBINED CLIMATE DATA ---------------------------

# Extract BIO1, BIO5 and BIO12 with supporting covariates
combined_covariates <- c(
  environment,
  travel,
  log_offset
)

combined_values <- terra::extract(
  combined_covariates,
  pts
) %>%
  as_tibble() %>%
  dplyr::select(-ID)

combined_data <- bind_cols(d, combined_values) %>%
  mutate(effort = n(), .by = cell_number)

write_csv(
  combined_data,
  file.path(out_dir, "model_data_environment.csv"),
  na = ""
)

# 5. PREPARE MONTHLY CLIMATE DATA ----------------------------

monthly_values <- vector("list", 12)

for (m in 1:12) {
  
  idx <- which(d$month_number == m)
  
  if (length(idx) == 0) next
  
  month_layers <- monthly_env[[
    c(
      paste0("tmin_", month.abb[m]),
      paste0("tmax_", month.abb[m]),
      paste0("precip_", month.abb[m])
    )
  ]]
  
  names(month_layers) <- c("tmin", "tmax", "precip")
  
  monthly_covariates <- c(
    month_layers,
    travel,
    log_offset
  )
  
  extracted <- terra::extract(
    monthly_covariates,
    pts[idx]
  ) %>%
    as_tibble() %>%
    dplyr::select(-ID)
  
  monthly_values[[m]] <- bind_cols(
    d[idx, ],
    extracted
  )
  
  cat(month.abb[m], ":", length(idx), "observations\n")
}

monthly_data <- bind_rows(monthly_values) %>%
  mutate(
    effort = n(),
    .by = c(cell_number, month_number)
  )

write_csv(
  monthly_data,
  file.path(out_dir, "model_data_monthly_environment.csv"),
  na = ""
)

# 6. OUTPUT SUMMARY ------------------------------------------

cat("\nModel data preparation completed.\n")
cat("Combined climate observations:", nrow(combined_data), "\n")
cat("Monthly climate observations:", nrow(monthly_data), "\n")

cat("\nSaved files:\n")
cat("- model_data_environment.csv\n")
cat("- model_data_monthly_environment.csv\n")













