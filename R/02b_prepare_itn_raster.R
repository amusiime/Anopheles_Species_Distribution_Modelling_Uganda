
# 02b_prepare_itn_raster.R
# Rasterise regional ITN use onto the 1 km grid

source(here::here("R", "config.R"))

library(terra)
library(sf)
library(tidyverse)

check_files(c(paths$districts, paths$itn_table, paths$climate_annual))
make_dir(dirname(paths$itn_raster))

template <- read_climate("annual")[[1]]

# Prediction year=2024 in config.R
itn <- read_itn() |>
  filter(year== pred_year$itn) |>
  select(itn_region, itn_use, year)

districts <- st_read(paths$districts, quiet = TRUE) |>
  filter(
    !is.na(dname2019),
    !is.na(d),
    !st_is_empty(geometry)
  ) |>
  mutate(
    itn_region = str_to_lower(str_squish(Region)),
    itn_region = if_else(itn_region == "kampala", "central", itn_region)
  ) |>
  left_join(itn, by = "itn_region")

stopifnot(!any(is.na(districts$itn_use)))

itn_raster <- districts |>
  st_transform(crs(template)) |>
  vect() |>
  rasterize(template, field = "itn_use", background = NA)

names(itn_raster) <- "itn_use"

writeRaster(
  itn_raster,
  paths$itn_raster,
  overwrite = TRUE,
  datatype = "FLT4S"
)

