
# 03_prepare_model_data.R
# Prepare annual and monthly mosquito abundance datasets

library(tidyverse)
library(terra)

# Settings --------------------------------------------------------------------
effort_mode <- "cell_records"  # Options: "cell_records", "none"

input_file <- "data/processed/ento_clean.csv"
annual_file <- "data/raw/worldclim/uganda_worldclim_annual.tif"
monthly_file <- "data/raw/worldclim/uganda_worldclim_monthly.tif"
continental_file <- "data/raw/continental/uganda_preds_p.tif"

files <- c(input_file, annual_file, monthly_file, continental_file)
if (any(!file.exists(files))) {
  stop("Missing files: ", paste(files[!file.exists(files)], collapse = ", "))
}
if (!effort_mode %in% c("cell_records", "none")) {
  stop("Invalid effort_mode.")
}

# Load data -------------------------------------------------------------------
d <- readr::read_csv(input_file, show_col_types = FALSE)
annual <- terra::rast(annual_file)
monthly <- terra::rast(monthly_file)
continental <- terra::rast(continental_file)

if (terra::nlyr(annual) != 3) stop("Annual raster must have 3 layers.")
if (terra::nlyr(monthly) != 36) stop("Monthly raster must have 36 layers.")
if (terra::nlyr(continental) < 4) stop("Continental raster needs band 4.")

names(annual) <- c("tmin", "tmax", "precip")
template <- annual[[1]]
continental <- continental[[4]]

required <- c(
  "observation_id", "event_date", "longitude", "latitude",
  "an_gambiae_total", "year", "month_number"
)
missing <- setdiff(required, names(d))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

# Validate coordinates and extract annual covariates ---------------------------
d <- d |>
  dplyr::mutate(.row_id = dplyr::row_number()) |>
  dplyr::filter(
    is.finite(longitude), is.finite(latitude),
    between(longitude, -180, 180),
    between(latitude, -90, 90)
  )

if (!nrow(d)) stop("No records have valid coordinates.")

points <- terra::vect(
  dplyr::select(d, longitude, latitude),
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

annual_points <- terra::project(points, terra::crs(template))
continental_points <- terra::project(points, terra::crs(continental))

d$cell_number <- terra::cellFromXY(template, terra::crds(annual_points))

annual_values <- terra::extract(annual, annual_points)
continental_values <- terra::extract(continental, continental_points)

d <- d |>
  dplyr::mutate(
    tmin = annual_values$tmin,
    tmax = annual_values$tmax,
    precip = annual_values$precip,
    continental_gambiae = continental_values[[2]]
  ) |>
  dplyr::filter(
    !is.na(cell_number),
    !is.na(continental_gambiae), continental_gambiae >= 0,
    !is.na(an_gambiae_total), an_gambiae_total >= 0
  )

if (!nrow(d)) stop("No eligible records remain.")

# Effort and offsets -----------------------------------------------------------
d <- d |>
  dplyr::add_count(cell_number, name = "effort") |>
  dplyr::mutate(
    log_effort = if (effort_mode == "cell_records") log(effort) else 0,
    log_continental = log(continental_gambiae),
    month_number = dplyr::coalesce(
      as.integer(month_number),
      match(stringr::str_to_title(as.character(month)), month.abb)
    )
  )

# Save annual dataset ----------------------------------------------------------
model_vars <- c(
  "observation_id", "event_date", "longitude", "latitude",
  "an_gambiae_total", "cell_number", "year", "month_number",
  "log_effort", "log_continental"
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

annual_data <- d |>
  dplyr::select(dplyr::all_of(model_vars), tmin, tmax, precip) |>
  tidyr::drop_na(an_gambiae_total, tmin, tmax, precip,
                 log_effort, log_continental)

readr::write_csv(annual_data, "data/processed/model_data_annual.csv")

# Extract monthly covariates ---------------------------------------------------
monthly_covars <- tibble::tibble(
  .row_id = d$.row_id,
  tmin_month = NA_real_,
  tmax_month = NA_real_,
  precip_month = NA_real_
)

for (m in 1:12) {
  i <- which(d$month_number == m)
  if (!length(i)) next
  
  vals <- terra::extract(
    monthly[[c(m, m + 12, m + 24)]],
    terra::project(points[i, ], terra::crs(monthly))
  )
  
  monthly_covars$tmin_month[i] <- vals[[2]]
  monthly_covars$tmax_month[i] <- vals[[3]]
  monthly_covars$precip_month[i] <- vals[[4]]
}

# Save monthly dataset ---------------------------------------------------------
monthly_data <- d |>
  dplyr::left_join(monthly_covars, by = ".row_id") |>
  dplyr::transmute(
    observation_id, event_date, longitude, latitude,
    an_gambiae_total, cell_number, year, month_number,
    tmin = tmin_month,
    tmax = tmax_month,
    precip = precip_month,
    log_effort, log_continental
  ) |>
  tidyr::drop_na(an_gambiae_total, tmin, tmax, precip,
                 log_effort, log_continental)

readr::write_csv(monthly_data, "data/processed/model_data_monthly.csv")

