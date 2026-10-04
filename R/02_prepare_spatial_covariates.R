
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

# Download Uganda national boundary (GADM level 0)
uga <- geodata::gadm(
  country = "UGA",
  level = 0,
  path = cache
)

# Ensure output directory exists
dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. MONTHLY CLIMATE VARIABLES -------------------------------

tmin <- geodata::worldclim_country(
  "Uganda", "tmin",
  version = "2.1",
  path = climate_dir
)

tmax <- geodata::worldclim_country(
  "Uganda", "tmax",
  version = "2.1",
  path = climate_dir
)

precip <- geodata::worldclim_country(
  "Uganda", "prec",
  version = "2.1",
  path = climate_dir
)

# Assign descriptive monthly layer names
months <- tolower(month.abb)

names(tmin) <- paste0("tmin_", months)
names(tmax) <- paste0("tmax_", months)
names(precip) <- paste0("precip_", months)

# Combine monthly climate variables
monthly_climate <- c(
  tmin,
  tmax,
  precip
)

# Mask monthly climate data to Uganda
monthly_climate <- terra::mask(
  monthly_climate,
  uga
)

# Save monthly climate variables
terra::writeRaster(
  monthly_climate,
  file.path(out_dir, "monthly_climate_uganda_1km.tif"),
  overwrite = TRUE
)


# 4. BIOCLIM CLIMATE VARIABLES -------------------------------

# Download WorldClim v2.1 bioclimatic variables
bio <- geodata::worldclim_country(
  "Uganda", "bio",
  version = "2.1",
  path = climate_dir
)


environment <- bio[[c(6, 5, 12)]]

# Assign descriptive layer names
names(environment) <- c(
  "bio6_min_temp",
  "bio5_max_temp",
  "bio12_precip"
)

# Align BIOCLIM variables with the monthly climate grid
environment <- terra::project(
  environment,
  monthly_climate[[1]],
  method = "bilinear"
)

# Mask BIOCLIM variables to Uganda
environment <- terra::mask(
  environment,
  uga
)

# Save environmental variables
terra::writeRaster(
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












