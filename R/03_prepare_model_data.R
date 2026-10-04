
# 03_prepare_model_data.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(terra)
})

# 1. FILES ---------------------------------------------------

data_dir <- "data/processed"

data_file <- file.path(data_dir, "ento_clean.csv")

monthly_env <- terra::rast(
  file.path(data_dir, "monthly_climate_uganda_1km.tif")
)

bio_env <- terra::rast(
  file.path(data_dir, "environment_1km.tif")
)

travel <- terra::rast(
  file.path(data_dir, "travel_accessibility_1km.tif")
)

log_offset <- terra::rast(
  file.path(data_dir, "log_continental_offset_1km.tif")
)

template <- monthly_env[[1]]


# 2. ALIGN SPATIAL COVARIATES -------------------------------

bio_env <- terra::project(
  bio_env, template, method = "bilinear"
)

travel <- terra::project(
  travel, template, method = "bilinear"
)

log_offset <- terra::project(
  log_offset, template, method = "bilinear"
)

names(travel) <- "travel"
names(log_offset) <- "log_continental_offset"


# 3. CLEAN OBSERVATIONS -------------------------------------

d <- readr::read_csv(
  data_file,
  show_col_types = FALSE
) %>%
  mutate(
    an_gambiae_total = as.numeric(an_gambiae_total),
    month_number = as.integer(month_number)
  ) %>%
  filter(
    !is.na(an_gambiae_total),
    ! is.na(longitude))

pts <- terra::vect(
  d,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
) %>%
  terra::project(terra::crs(template))

cells <- terra::cellFromXY(
  template,
  terra::crds(pts)
)

keep <- !is.na(cells)

d <- d[keep, , drop = FALSE]
pts <- pts[keep, ]
d$cell_number <- cells[keep]


# 4. STATIC ENVIRONMENTAL DATA ------------------------------

static_covariates <- c(
  bio_env,
  travel,
  log_offset
)

static_values <- terra::extract(
  static_covariates,
  pts
) %>%
  as_tibble() %>%
  dplyr::select(-ID)

static_data <- bind_cols(
  d,
  static_values
) %>%
  mutate(
    effort = n(),
    .by = cell_number
  )

readr::write_csv(
  static_data,
  file.path(data_dir, "model_data_environment.csv"),
  na = ""
)


# 5. MONTHLY ENVIRONMENTAL DATA -----------------------------

monthly_values <- vector("list", 12)

for (m in seq_len(12)) {

  idx <- which(d$month_number == m)

  if (length(idx) == 0) next

  # Select monthly climate layers by position
  layer_indices <- c(m, 12 + m, 24 + m)

  month_layers <- monthly_env[[layer_indices]]

  names(month_layers) <- c(
    "tmin",
    "tmax",
    "precip"
  )

  monthly_covariates <- c(
    month_layers,
    travel,
    log_offset
  )

  extracted <- terra::extract(
    monthly_covariates,
    pts[idx]
  ) %>%
    as_tibble() %>%
    dplyr::select(-ID)

  monthly_values[[m]] <- bind_cols(
    d[idx, ],
    extracted
  )

  cat(month.abb[m], ":", length(idx), "observations\n")
}

monthly_data <- bind_rows(monthly_values) %>%
  mutate(
    effort = n(),
    .by = c(cell_number, month_number)
  )

readr::write_csv(
  monthly_data,
  file.path(data_dir, "model_data_monthly_environment.csv"),
  na = ""
)


