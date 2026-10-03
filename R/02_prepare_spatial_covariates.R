
# 02_prepare_spatial_covariates.R
# Prepare spatial covariates for Anopheles gambiae abundance modelling

options(timeout = 600)

suppressPackageStartupMessages({
  library(terra)
  library(geodata)
})

# 1. SETTINGS ------------------------------------------------

out_dir <- "data/processed"
climate_dir <- "data/raw/worldclim_monthly"
cache <- "data/raw/.geodata"
continental_file <- "data/raw/continental/uganda_preds_p.tif"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(climate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache, recursive = TRUE, showWarnings = FALSE)

uganda_extent <- ext(29.5, 35.1, -1.5, 4.3)
months <- month.abb

# 2. UGANDA BOUNDARY -----------------------------------------

uga <- geodata::gadm(
  country = "UGA",
  level = 0,
  path = cache
)

# 3. MONTHLY CLIMATE VARIABLES -------------------------------

tmin <- geodata::worldclim_country(
  "Uganda", "tmin", res = 0.5, version = "2.1",
  path = climate_dir
) / 10

tmax <- geodata::worldclim_country(
  "Uganda", "tmax", res = 0.5, version = "2.1",
  path = climate_dir
) / 10

precip <- geodata::worldclim_country(
  "Uganda", "prec", res = 0.5, version = "2.1",
  path = climate_dir
)

# Crop and name monthly layers
tmin <- crop(tmin, uganda_extent)
tmax <- crop(tmax, uganda_extent)
precip <- crop(precip, uganda_extent)

names(tmin) <- paste0("tmin_", months)
names(tmax) <- paste0("tmax_", months)
names(precip) <- paste0("precip_", months)

monthly_climate <- mask(
  c(tmin, tmax, precip),
  project(uga, crs(tmin))
)

writeRaster(
  monthly_climate,
  file.path(out_dir, "monthly_climate_uganda_1km.tif"),
  overwrite = TRUE
)

# 4. COMBINED CLIMATE VARIABLES ------------------------------

bio <- geodata::worldclim_country(
  "Uganda", "bio", res = 0.5, version = "2.1",
  path = climate_dir
)

# BIO1 = Annual mean temperature
# BIO5 = Maximum temperature of warmest month
# BIO12 = Annual precipitation

environment <- crop(bio[[c(1, 5, 12)]], uganda_extent)

names(environment) <- c("bio1", "bio5", "bio12")

environment <- mask(
  environment,
  project(uga, crs(environment))
)

# Align combined climate variables with monthly climate grid
environment <- project(
  environment,
  monthly_climate[[1]],
  method = "bilinear"
)

writeRaster(
  environment,
  file.path(out_dir, "environment_1km.tif"),
  overwrite = TRUE
)

# 5. TRAVEL ACCESSIBILITY ------------------------------------

travel_raw <- geodata::travel_time(
  to = "city",
  size = 5,
  path = cache
)

travel_uga <- mask(
  crop(travel_raw, project(uga, crs(travel_raw))),
  project(uga, crs(travel_raw))
)

# Project and align with climatic var
travel <- project(
  travel_uga,
  monthly_climate[[1]],
  method = "bilinear"
)

#global() calculates 
#a statistical summary across all raster cells

max_travel <- global(travel, "max", na.rm = TRUE)[1, 1]

# clamp Normalise travel time

travel <- clamp(1 - travel / max_travel, 0, 1)
names(travel) <- "travel"

writeRaster(
  travel,
  file.path(out_dir, "travel_accessibility_1km.tif"),
  overwrite = TRUE
)

# 6. CONTINENTAL AN. GAMBIAE OFFSET --------------------------


continental <- project(
  rast(continental_file)[[4]],
  monthly_climate[[1]],
  method = "bilinear"
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












