
# 02_prepare_covariates.R

# Prepare 1 km environmental covariates for Uganda

source(here::here("R", "config.R"))

library(terra)
library(geodata)
library(tidyverse)

# Input files 

make_dir(paths$worldclim_dir)
make_dir(dirname(paths$climate_annual))

landcover_files <- file.path(
  paths$env_dir,
  paste0("landcover_", landcover_classes, "_1992_2022.tif")
)

footprint_file <- file.path(paths$env_dir, "footprint_2000_2024.tif")

check_files(c(landcover_files, footprint_file))

# Uganda boundary 

uganda <- geodata::gadm(
  "UGA",
  level = 0,
  path = paths$worldclim_dir
)

# Monthly climate 

get_worldclim <- function(var, prefix) {
  
  r <- geodata::worldclim_country(
    "Uganda",
    var = var,
    res = 0.5,
    path = paths$worldclim_dir,
    version = "2.1"
  )
  
  stopifnot(nlyr(r) == 12)
  
  r <- mask(crop(r, uganda), uganda)
  names(r) <- paste0(prefix, "_", month.abb)
  
  r
}

monthly_climate <- c(
  get_worldclim("tmin", "tmin"),
  get_worldclim("tmax", "tmax"),
  get_worldclim("prec", "precip")
)

stopifnot(
  compareGeom(
    monthly_climate[[1]],
    monthly_climate,
    stopOnError = FALSE
  )
)

writeRaster(
  monthly_climate,
  paths$climate_monthly,
  overwrite = TRUE,
  datatype = "FLT4S"
)

# Annual climate 

annual_climate <- c(
  mean(monthly_climate[[1:12]]),
  mean(monthly_climate[[13:24]]),
  sum(monthly_climate[[25:36]])
)

names(annual_climate) <- climate_vars

#FLT4S preserves decimal precision
writeRaster(
  annual_climate,
  paths$climate_annual,
  overwrite = TRUE,
  datatype = "FLT4S"
)

# Common 1 km grid 

template <- monthly_climate[[1]]

# Land cover and human footprint 

#Read multi-layer raster and align to
#modelling grid,and checks that the output geometry matches my template

read_stack <- function(file, prefix, years) {
  
  r <- rast(file)
  
  stopifnot(nlyr(r) == length(years))
  
  names(r) <- paste0(prefix, "_", years)
  
  r <- resample(r, template, method = "average") |>
    mask(uganda)
  
  stopifnot(compareGeom(r, template, stopOnError = FALSE))
  
  r
}

#reads multiple land-cover raster files and 
#processes each using read_stack()

landcover <- map2(
  landcover_files,
  landcover_classes,
  \(file, cl) read_stack(file, cl, landcover_years)
) |>
  purrr::reduce(c)

footprint <- read_stack(
  footprint_file,
  "footprint",
  footprint_years
)

# Save covariates 

writeRaster(
  landcover,
  paths$landcover,
  overwrite = TRUE,
  datatype = "FLT4S"
)

writeRaster(
  footprint,
  paths$footprint,
  overwrite = TRUE,
  datatype = "FLT4S"
)

# Final validation 

stopifnot(
  compareGeom(landcover, template, stopOnError = FALSE),
  compareGeom(footprint, template, stopOnError = FALSE),
  nlyr(landcover) == length(landcover_classes) * length(landcover_years),
  nlyr(footprint) == length(footprint_years)
)




