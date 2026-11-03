


# 03_prepare_model_data.R
# Prepare annual and monthly vector abundance modelling datasets

source(here::here("R", "config.R"))

library(terra)
library(tidyverse)

# Region mapping 

region_map <- tribble(
  ~region,         ~itn_region,
  "lango",         "northern",
  "karamoja",      "northern",
  "bukedi",        "eastern",
  "busoga",        "eastern",
  "teso",          "eastern",
  "south buganda", "central"
)

# Input files 

check_files(c(
  paths$ento_clean,
  paths$climate_annual,
  paths$climate_monthly,
  paths$landcover,
  paths$footprint,
  paths$continental,
  paths$itn_table
))

make_dir(dirname(paths$model_annual))

# Load data

ento <- read_csv(paths$ento_clean, show_col_types = FALSE) |>
  mutate(region = str_to_lower(str_squish(region))) |>
  filter(
    if_all(
      c(longitude, latitude, an_gambiae_total, year, month_number),
      is.finite
    ),
    an_gambiae_total >= 0,
    month_number %in% 1:12
  )

annual <- read_climate("annual")
monthly <- read_climate("monthly")

landcover <- rast(paths$landcover)
footprint <- rast(paths$footprint)

continental <- rast(paths$continental)[[continental_layer]]

# Align continental raster 
if (!same.crs(continental, annual)) {
  continental <- project(continental, annual, method = "bilinear")
} else if (!compareGeom(continental, annual, stopOnError = FALSE)) {
  continental <- resample(continental, annual, method = "bilinear")
}

# Assign observations to 1 km grid 
points <- vect(
  ento,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
) |>
  project(crs(annual))

ento$cell_number <- cellFromXY(annual[[1]], crds(points))

ento <- ento |>
  filter(!is.na(cell_number))

# Join regional ITN use 

itn <- read_itn()

ento <- ento |>
  left_join(region_map, by = "region") |>
  left_join(itn, by = c("itn_region", "year"))

# Aggregate observations 
aggregate_data <- \(data, groups) {
  data |>
    group_by(across(all_of(groups))) |>
    summarise(
      an_gambiae_total = sum(an_gambiae_total, na.rm = TRUE),
      effort = n(),
      itn_use = if (all(is.na(itn_use))) {
        NA_real_
      } else {
        mean(itn_use, na.rm = TRUE)
      },
      region = names(which.max(table(region))),
      .groups = "drop"
    ) |>
    mutate(log_effort = log(effort))
}

annual_data <- aggregate_data(
  ento,
  c("cell_number", "year")
)

monthly_data <- aggregate_data(
  ento,
  c("cell_number", "year", "month_number")
)

# Grid-cell coordinates 
add_coordinates <- \(data) {
  xy <- xyFromCell(annual[[1]], data$cell_number)
  
  coords <- vect(
    xy,
    type = "points",
    crs = crs(annual)
  ) |>
    project("EPSG:4326") |>
    crds()
  
  bind_cols(
    data,
    tibble(
      longitude = coords[, 1],
      latitude = coords[, 2],
      x = xy[, 1],
      y = xy[, 2]
    )
  )
}

annual_data <- add_coordinates(annual_data)
monthly_data <- add_coordinates(monthly_data)

# Raster extraction 

extract_by <- \(r, data, group, layer_fun, output_names) {
  points <- vect(data, geom = c("x", "y"), crs = crs(annual))
  
  map_dfr(unique(na.omit(data[[group]])), \(v) {
    i <- which(!is.na(data[[group]]) & data[[group]] == v)
    layers <- layer_fun(v)
    
    terra::extract(r[[layers]], points[i, ], ID = FALSE) |>
      as_tibble() |>
      setNames(output_names) |>
      mutate(.row_id = i)
  }) |>
    dplyr::arrange(.data$.row_id) |>
    select(-.row_id)
}

# Annual climate and continental predictions

annual_points <- vect(
  annual_data,
  geom = c("x", "y"),
  crs = crs(annual)
)

annual_data <- bind_cols(
  annual_data,
  terra::extract(annual, annual_points, ID = FALSE) |>
    as_tibble(),
  tibble(
    continental_gambiae = terra::extract(
      continental,
      annual_points,
      ID = FALSE
    )[[1]]
  )
) |>
  filter(
    is.finite(continental_gambiae),
    continental_gambiae > 0
  ) |>
  mutate(log_continental = log(continental_gambiae))

# Year-matched land cover 

annual_data <- bind_cols(
  annual_data,
  extract_by(
    landcover,
    annual_data,
    "year",
    \(y) paste0(
      landcover_classes, "_",
      clamp_year(as.numeric(y), landcover_years)
    ),
    landcover_classes
  )
)

# Year-matched human footprint 

annual_data <- bind_cols(
  annual_data,
  extract_by(
    footprint,
    annual_data,
    "year",
    \(y) paste0(
      "footprint_",
      clamp_year(as.numeric(y), footprint_years)
    ),
    "human_footprint"
  )
)

# Add annual predictors to monthly data 

annual_predictors <- c(
  climate_vars,
  "continental_gambiae",
  "log_continental",
  landcover_classes,
  "human_footprint"
)

monthly_data <- monthly_data |>
  left_join(
    annual_data |>
      select(cell_number, year, all_of(annual_predictors)),
    by = c("cell_number", "year")
  )

# Monthly climate 

monthly_data <- monthly_data |>
  select(-all_of(climate_vars)) |>
  bind_cols(
    extract_by(
      monthly,
      monthly_data,
      "month_number",
      \(m) paste0(climate_vars, "_", month.abb[as.integer(m)]),
      climate_vars
    )
  )

# Select modelling variables 
annual_keep <- c(
  "longitude", "latitude", "cell_number", "year",
  "an_gambiae_total", "effort", "log_effort",
  "log_continental", "region", climate_vars,
  landcover_classes, "human_footprint", "itn_use"
)

monthly_keep <- c(
  "longitude", "latitude", "cell_number", "year", "month_number",
  "an_gambiae_total", "effort", "log_effort",
  "log_continental", "region", climate_vars,
  landcover_classes, "human_footprint", "itn_use"
)

annual_data <- annual_data |>
  select(all_of(annual_keep)) |>
  drop_na()

monthly_data <- monthly_data |>
  select(all_of(monthly_keep)) |>
  drop_na()

# Final validation 
stopifnot(
  all(annual_data$effort > 0),
  all(monthly_data$effort > 0),
  all(annual_data$an_gambiae_total >= 0),
  all(monthly_data$an_gambiae_total >= 0),
  all(annual_data$itn_use >= 0 & annual_data$itn_use <= 1),
  all(monthly_data$itn_use >= 0 & monthly_data$itn_use <= 1)
)

# Save datasets 

write_csv(annual_data, paths$model_annual)
write_csv(monthly_data, paths$model_monthly)

