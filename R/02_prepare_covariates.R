

# 02_prepare_covariates.R
# Prepare 1 km WorldClim covariates for Uganda

library(terra)
library(geodata)

out_dir <- "data/raw/worldclim"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Uganda boundary
uganda <- geodata::gadm("UGA", level = 0, path = out_dir)

# Download WorldClim data for Uganda (~1 km)
tmin <- geodata::worldclim_country("Uganda", "tmin", res = 0.5, path = out_dir, version = "2.1")
tmax <- geodata::worldclim_country("Uganda", "tmax", res = 0.5, path = out_dir, version = "2.1")
prec <- geodata::worldclim_country("Uganda", "prec", res = 0.5, path = out_dir, version = "2.1")

# Crop and mask to Uganda
tmin <- mask(crop(tmin, uganda), uganda)
tmax <- mask(crop(tmax, uganda), uganda)
prec <- mask(crop(prec, uganda), uganda)

# Align rasters
tmax <- resample(tmax, tmin, method = "bilinear")
prec <- resample(prec, tmin, method = "bilinear")

# Name monthly layers
names(tmin) <- paste0("tmin_", month.abb)
names(tmax) <- paste0("tmax_", month.abb)
names(prec) <- paste0("precip_", month.abb)

# Save monthly covariates
writeRaster(
  c(tmin, tmax, prec),
  "data/processed/uganda_worldclim_monthly_1km.tif",
  overwrite = TRUE
)

# Calculate annual covariates
annual <- c(
  app(tmin, mean, na.rm = TRUE),
  app(tmax, mean, na.rm = TRUE),
  app(prec, sum, na.rm = TRUE)
)

names(annual) <- c("tmin_mean", "tmax_mean", "precip_annual")

writeRaster(
  annual,
  "data/processed/uganda_worldclim_annual_1km.tif",
  overwrite = TRUE
)









