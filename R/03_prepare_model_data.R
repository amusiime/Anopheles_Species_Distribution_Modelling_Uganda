
# 03_prepare_model_data.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(terra)
  library(lubridate)
})

# 1. LOAD DATA ----------------------------------------------

d <- read_csv(
  "data/processed/ento_clean.csv",
  show_col_types = FALSE
)

env <- rast("data/processed/environment_1km.tif")
travel <- rast("data/processed/travel_accessibility_1km.tif")
continental <- rast("data/processed/log_continental_offset_1km.tif")

# 2. PREPARE COVARIATES -------------------------------------

environment <- env[[c(3, 2, 4)]]
names(environment) <- c("tmin", "tmax", "precip")

covariates <- c(
  environment,
  travel[[1]],
  continental[[1]]
)

names(covariates) <- c(
  "tmin", "tmax", "precip",
  "travel", "log_continental_offset"
)

# 3. CLEAN OBSERVATIONS -------------------------------------

d2 <- d %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    an_gambiae_total = as.numeric(an_gambiae_total)
  ) %>%
  filter(
    is.finite(longitude),
    is.finite(latitude),
    between(longitude, 29.5, 35.1),
    between(latitude, -1.5, 4.3)
  )

# 4. ASSIGN GRID CELLS --------------------------------------
d2$cell_number <- terra::cellFromXY(
  environment[[1]],
  cbind(d2$longitude, d2$latitude)
)

#Extract raster covariates
extracted <- terra::extract(
  covariates,
  cbind(d2$longitude, d2$latitude)
)

# Attach covariates to observation data
d2 <- bind_cols(
  d2,
  as_tibble(extracted))


# Calculate sampling effort by cell_number
d2 <- d2 %>%
  mutate(
    effort = n(),
    .by = c(cell_number))

write_csv(
  d2,
  "data/processed/model_data_observations.csv",
  na = ""
)




