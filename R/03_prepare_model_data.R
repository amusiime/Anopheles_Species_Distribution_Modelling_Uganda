
# 03_prepare_model_data.R

library(tidyverse)
library(terra)
library(lubridate)

# 1. Load data
d <- read_csv("data/processed/ento_clean.csv",
              show_col_types = FALSE)

env <- rast("data/processed/environment_1km.tif")
travel_raw <- rast("data/processed/travel_accessibility_1km.tif")
continental_raw <- rast("data/processed/log_continental_offset_1km.tif")

# 2. Prepare raster covariates
environment <- env[[c(3, 2, 4)]]
names(environment) <- c("tmin", "tmax", "precip")

travel <- project(travel_raw[[1]], environment,
                  method = "bilinear")

continental <- project(continental_raw[[1]], environment,
                       method = "bilinear")

names(travel) <- "travel"
names(continental) <- "log_continental_offset"

covariates <- c(environment, travel, continental)


# 4. Clean observations
d2 <- d %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    an_gambiae_total = as.numeric(an_gambiae_total)
  ) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    between(longitude, 29, 36),
    between(latitude, -2, 5))
  

# 5. Assign observations to 1 km grid cells

points <- vect(
  d2,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)


d2$cell_number <- cellFromXY(environment[[1]], 
                    as.matrix(d[ !is.na(d$latitude),
                       c("longitude","latitude")]))

# Extract environmental and spatial covariates
extracted_covariates <- terra::extract(
  covariates,
  d2[, c("longitude", "latitude")]
)

# Attach extracted covariates to observations
d2 <- bind_cols(
  d2,
  extracted_covariates %>% select(-ID)
)



# Create unique cell_id_month identifier
d2 <- d2 %>%
  mutate(
    cell_number_month = paste(cell_number, month, sep = "_")
  )

# Aggregate by cell_id_month
cell_month_totals <- d2 %>%
  group_by(cell_number_month) %>%
  summarise(
    an_gambiae_total = sum(an_gambiae_total, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    travel = mean(travel, na.rm = TRUE),
    log_continental_offset = first(log_continental_offset),
    n_observations = n(),
    .groups = "drop"
  )

cell_totals <- cell_month_totals %>%
  group_by(cell_number) %>%
  summarise(
    n_cell_months = n(),
    an_gambiae_total = sum(an_gambiae_total, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    travel = first(travel),
    log_continental_offset = first(log_continental_offset),
    .groups = "drop"
  )

# Save cell-month dataset
write_csv(
  cell_month_totals,
  "data/processed/model_data_cell_month_totals.csv"
)

## 9. Save grid-cell dataset
write_csv(
  cell_totals,
  "data/processed/model_data_cell_totals.csv"
)





