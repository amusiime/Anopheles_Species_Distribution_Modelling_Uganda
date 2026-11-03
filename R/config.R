
# R/config.R
# Shared paths, settings and helper functions

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

set.seed(42)

# 1. Paths 

raw_p  <- \(...) here("data", "raw", ...)
proc_p <- \(...) here("data", "processed", ...)
out_p  <- \(...) here("outputs", ...)

paths <- list(
  # Raw data
  ento_raw      = raw_p("ento_august.csv"),
  districts     = raw_p("Uganda Districts-wgs84.shp"),
  itn_table     = raw_p("itn_data_clean.csv"),
  continental   = raw_p("continental", "uganda_preds_p.tif"),
  worldclim_dir = raw_p("worldclim"),
  env_dir       = raw_p("environmental_space", "uganda_environmental_space"),
  
  # Processed data
  ento_clean      = proc_p("ento_clean.csv"),
  climate_annual  = proc_p("uganda_worldclim_annual_1km.tif"),
  climate_monthly = proc_p("uganda_worldclim_monthly_1km.tif"),
  landcover       = proc_p("uganda_landcover_1km.tif"),
  footprint       = proc_p("uganda_footprint_1km.tif"),
  itn_raster      = proc_p("itn_use_prediction_1km.tif"),
  model_annual    = proc_p("model_data_annual_itn.csv"),
  model_monthly   = proc_p("model_data_monthly_itn.csv"),
  
  # Outputs
  models_annual  = out_p("models", "annual_models.rds"),
  models_monthly = out_p("models", "monthly_models.rds"),
  models         = out_p("models"),
  predictions    = out_p("predictions"),
  figures        = out_p("figures"),
  tables         = out_p("tables")
)

# 2. Model predictors --

climate_vars <- c("tmin", "tmax", "precip")

landcover_classes <- c(
  "crop_other", "irrigated", "tree", "shrubland", "grassland",
  "sparse", "wetland", "mangrove", "urban", "bare", "water"
)

landcover_predictors <- c(
  "irrigated", "tree", "shrubland", "wetland", "urban", "water"
)

env_predictors <- c(
  climate_vars, landcover_predictors, "human_footprint"
)

# 3. Temporal settings 

landcover_years <- 1992:2022
footprint_years <- 2000:2024

pred_year <- list(
  landcover = 2022,
  footprint = 2024,
  itn = 2024
)

# 4. Prediction settings 

continental_layer <- 4

extrapolation <- "clamp"
mask_open_water <- TRUE
map_clip_q <- 0.99

abundance_unit <- c(
  M3 = "An. gambiae per PSC",
  M4 = "An. gambiae per PSC x continental surface"
)

model_labels <- c(
  M1 = "M1: climate",
  M2 = "M2: M1 + land cover + human footprint",
  M3 = "M3: M2 + ITN use",
  M4 = "M4: M3 + continental-model offset"
)

# 5. Helper functions 

check_files <- \(files) {
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Missing files: ", paste(missing, collapse = ", "))
  }
}

make_dir <- \(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

clamp_year <- \(year, years) {
  pmin(pmax(year, min(years)), max(years))
}

# Read climate rasters
read_climate <- \(type = c("annual", "monthly")) {
  type <- match.arg(type)
  
  r <- terra::rast(paths[[paste0("climate_", type)]])
  
  nms <- if (type == "annual") {
    climate_vars
  } else {
    paste0(rep(climate_vars, each = 12), "_", month.abb)
  }
  
  if (terra::nlyr(r) != length(nms)) {
    stop("Unexpected number of climate raster layers.")
  }
  
  names(r) <- nms
  r
}

# Read regional ITN use
read_itn <- \(file = paths$itn_table, which_scenario = "Historical") {
  
  itn <- read_csv(file, show_col_types = FALSE) |>
    filter(scenario == which_scenario) |>
    transmute(
      itn_region = str_to_lower(str_squish(admin_name)),
      year = lubridate::year(as.Date(date)),
      itn_use = as.numeric(use)
    )
  
  if (anyDuplicated(itn[c("itn_region", "year")])) {
    stop("Duplicate ITN values for a region-year.")
  }
  
  itn
}

