
# 04_final_prediction.R

library(terra)

# 1. PREPARE PREDICTORS -------------------------------------

env <- rast("data/processed/environment_1km.tif")
travel <- rast("data/processed/travel_accessibility_1km.tif")

predictors <- c(
  env[[c(3, 2, 4)]],
  travel[[1]]
)

names(predictors) <- c(
  "tmin", "tmax", "precip", "travel"
)

# Reference sampling effort = 1
log_effort <- terra::init(predictors[[1]], 0)
names(log_effort) <- "log_effort"


predictors <- c(predictors, log_effort)

# 2. PREDICT USING FITTED M1 -------------------------------

prediction <- terra::predict(
  predictors,
  M1,
  type = "response",
  na.rm = TRUE
)

names(prediction) <- "an_gambiae_predicted"

# 3. SAVE PREDICTION ----------------------------------------

dir.create(
  "outputs/predictions",
  recursive = TRUE,
  showWarnings = FALSE
)

writeRaster(
  prediction,
  "outputs/predictions/an_gambiae_M1_1km.tif",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

# 4. CREATE MAP USING GGPLOT --------------------------------

library(ggplot2)
library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)
library(viridis)

# Convert prediction raster to data frame
pred_df <- as.data.frame(
  prediction,
  xy = TRUE,
  na.rm = TRUE
)

names(pred_df) <- c("longitude", "latitude", "predicted_abundance")


# Uganda boundary
# Uganda boundary
uganda <- ne_countries(
  country = "Uganda",
  scale = "medium",
  returnclass = "sf"
)

# District boundaries
districts <- st_read(
  "data/raw/Uganda Districts-wgs84.shp",
  quiet = TRUE
) %>%
  st_transform(4326)

# Convert prediction raster to WGS84
prediction_map <- terra::project(prediction, "EPSG:4326")


# Prepare raster data
pred_df <- as.data.frame(
  prediction_map,
  xy = TRUE,
  na.rm = TRUE
)

names(pred_df) <- c("longitude", "latitude", "predicted_abundance")

# Ensure map layers use the same CRS
uganda <- st_transform(uganda, 4326)
districts <- st_transform(districts, 4326)

# Map using actual prediction range
p_actual <- ggplot() +
  geom_raster(
    data = pred_df,
    aes(longitude, latitude, fill = predicted_abundance)
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, 0.3),
    breaks = seq(0, 0.3, by = 0.05),
    oob = scales::squish,
    name = "Predicted\nabundance"
  ) +
  theme_void()

p_actual
  
  