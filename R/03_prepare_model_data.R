
# ============================================================
# R/03_prepare_model_data.R
# Uganda Mosquito Abundance Modelling - 1 km Grid
#
# Purpose:
# 1. Read entomological surveillance data and spatial rasters.
# 2. Prepare environmental and spatial covariates.
# 3. Assign observations to 1 km grid cells.
# 4. Extract raster covariates at grid-cell centres.
# 5. Aggregate mosquito counts by cell-month.
# 6. Aggregate mosquito counts by grid cell.
# 7. Calculate sampling effort as unique sampled cell-months.
# 8. Save modelling datasets.
#
# Outputs:
# data/processed/model_data_cell_month_totals.csv
# data/processed/model_data_cell_totals.csv
# ============================================================


# ============================================================
# 0. LOAD PACKAGES
# ============================================================

library(tidyverse)
library(terra)
library(lubridate)


# ============================================================
# 1. DEFINE FILE PATHS
# ============================================================

input_file <- "data/processed/ento_clean.csv"

environment_file <- "data/processed/environment_1km.tif"

travel_file <- "data/processed/travel_accessibility_1km.tif"

continental_file <- "data/processed/log_continental_offset_1km.tif"

output_dir <- "data/processed"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Check that all input files exist

required_files <- c(
  input_file,
  environment_file,
  travel_file,
  continental_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}


# ============================================================
# 2. READ ENTOMOLOGICAL DATA AND RASTERS
# ============================================================

d <- read_csv(
  input_file,
  show_col_types = FALSE
)

env <- rast(environment_file)

travel_raw <- rast(travel_file)

continental_raw <- rast(continental_file)


# Check environmental raster layers

if (nlyr(env) < 4) {
  stop(
    "The environmental raster must contain at least four layers."
  )
}

if (nlyr(travel_raw) < 1 ||
    nlyr(continental_raw) < 1) {
  stop(
    "Travel or continental raster contains no layers."
  )
}


# ============================================================
# 3. PREPARE ENVIRONMENTAL VARIABLES
# ============================================================

# BIOCLIM layer selection:
# Layer 2 = bio5  : Maximum temperature of warmest month
# Layer 3 = bio6  : Minimum temperature of coldest month
# Layer 4 = bio12 : Annual precipitation

environment <- c(
  env[[3]],
  env[[2]],
  env[[4]]
)

names(environment) <- c(
  "tmin",
  "tmax",
  "precip"
)


# ============================================================
# 4. ALIGN SPATIAL RASTERS
# ============================================================

# Align travel accessibility to environmental grid

travel <- project(
  travel_raw[[1]],
  environment,
  method = "bilinear"
)


# Align continental abundance offset to environmental grid

continental <- project(
  continental_raw[[1]],
  environment,
  method = "bilinear"
)


names(travel) <- "travel"

names(continental) <- "log_continental_offset"


# Verify raster alignment

if (!compareGeom(
  environment,
  travel,
  continental,
  stopOnError = FALSE
)) {
  stop(
    "Environmental, travel and continental rasters are not aligned."
  )
}

cat("\nRaster alignment completed.\n")

cat("Environmental grid resolution:\n")

print(res(environment))


# ============================================================
# 5. IDENTIFY MOSQUITO COUNT VARIABLES
# ============================================================

count_vars <- intersect(
  c(
    "an_gambiae_total",
    "an_funestus_total",
    "an_coustani_total",
    "other_anopheles_total"
  ),
  names(d)
)

if (!"an_gambiae_total" %in% count_vars) {
  stop(
    "Required response variable an_gambiae_total is missing."
  )
}


# Check required entomological columns

required_columns <- c(
  "longitude",
  "latitude",
  "year",
  "month"
)

missing_columns <- setdiff(
  required_columns,
  names(d)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ============================================================
# 6. CLEAN ENTOMOLOGICAL DATA
# ============================================================

# Convert month values into numeric months (1-12)

parse_month <- function(x) {
  
  x <- trimws(as.character(x))
  
  numeric_month <- suppressWarnings(
    as.integer(x)
  )
  
  named_month <- match(
    tolower(substr(x, 1, 3)),
    tolower(month.abb)
  )
  
  date_month <- month(
    suppressWarnings(
      parse_date_time(
        x,
        orders = c(
          "ymd",
          "dmy",
          "mdy",
          "ym",
          "my"
        )
      )
    )
  )
  
  case_when(
    between(numeric_month, 1, 12) ~ numeric_month,
    !is.na(named_month) ~ named_month,
    !is.na(date_month) ~ date_month,
    TRUE ~ NA_integer_
  )
}


# Clean coordinates, dates and mosquito counts

d <- d %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    year = as.integer(year),
    month = parse_month(month),
    
    across(
      all_of(count_vars),
      as.numeric
    )
  ) %>%
  filter(
    is.finite(longitude),
    is.finite(latitude),
    !is.na(year),
    !is.na(month),
    between(month, 1, 12),
    
    # Uganda geographic limits used for data screening
    between(longitude, 29, 36),
    between(latitude, -2, 5)
  )


if (nrow(d) == 0) {
  stop(
    "No valid entomological observations remain after cleaning."
  )
}

cat("\nValid entomological observations:", nrow(d), "\n")


# ============================================================
# 7. ASSIGN OBSERVATIONS TO 1 KM GRID CELLS
# ============================================================

# Convert observations into spatial points

points <- vect(
  d,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)


# Transform points to environmental raster CRS

points_env <- project(
  points,
  crs(environment)
)


# Assign each observation to an environmental grid cell

d$cell_number <- cellFromXY(
  environment[[1]],
  crds(points_env)
)


# Remove observations outside raster coverage

d <- d %>%
  filter(
    !is.na(cell_number)
  )


if (nrow(d) == 0) {
  stop(
    "No observations fall within the environmental raster."
  )
}


# Obtain grid-cell centre coordinates

coordinates <- xyFromCell(
  environment[[1]],
  d$cell_number
)

d$longitude <- coordinates[, 1]

d$latitude <- coordinates[, 2]


# Create unique grid-cell identifier

d$cell_id <- paste0(
  "CELL_",
  d$cell_number
)


# Recreate spatial points at grid-cell centres

points <- vect(
  d,
  geom = c("longitude", "latitude"),
  crs = crs(environment)
)


# ============================================================
# 8. EXTRACT ENVIRONMENTAL AND SPATIAL COVARIATES
# ============================================================

covariates <- c(
  environment,
  travel,
  continental
)


# Extract raster values at grid-cell centres

covariate_values <- terra::extract(
  covariates,
  points,
  ID = FALSE
)


# Attach extracted covariates to entomological data

d <- bind_cols(
  d,
  as_tibble(covariate_values)
)


cat("\nCovariate extraction completed.\n")


# ============================================================
# 9. RETAIN COMPLETE MODELLING RECORDS
# ============================================================

# Retain observations with finite values for all predictors.
# Missing mosquito counts are not converted to zero.

d <- d %>%
  filter(
    if_all(
      c(
        tmin,
        tmax,
        precip,
        travel,
        log_continental_offset
      ),
      ~ is.finite(.x)
    )
  )


if (nrow(d) == 0) {
  stop(
    "No observations remain after filtering incomplete covariates."
  )
}

cat(
  "Records with complete modelling covariates:",
  nrow(d),
  "\n"
)


# ============================================================
# 10. CREATE UNIQUE CELL-MONTH IDENTIFIER
# ============================================================

# Each unique combination of grid cell, year and month
# represents one sampled cell-month.

d <- d %>%
  mutate(
    cell_month_id = paste(
      cell_id,
      year,
      month,
      sep = "_"
    )
  )


# ============================================================
# 11. DEFINE SAFE AGGREGATION FUNCTIONS
# ============================================================

# Calculate mean while preserving entirely missing groups

safe_mean <- function(x) {
  
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}


# Sum counts while preserving entirely missing groups

safe_sum <- function(x) {
  
  if (all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}


# ============================================================
# 12. AGGREGATE MOSQUITO COUNTS BY CELL-MONTH
# ============================================================

# Each row represents one grid cell sampled in one month
# of a particular year.

cell_month_totals <- d %>%
  group_by(
    cell_id,
    cell_number,
    longitude,
    latitude,
    year,
    month,
    cell_month_id
  ) %>%
  summarise(
    
    # Total mosquito counts within each cell-month
    
    across(
      all_of(count_vars),
      safe_sum
    ),
    
    # Environmental and spatial covariates
    
    tmin = safe_mean(tmin),
    
    tmax = safe_mean(tmax),
    
    precip = safe_mean(precip),
    
    travel = safe_mean(travel),
    
    log_continental_offset = safe_mean(
      log_continental_offset
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    cell_id,
    year,
    month
  )


# ============================================================
# 13. AGGREGATE DATA BY GRID CELL
# ============================================================

# Each row represents one 1 km grid cell.
#
# n_cell_months = number of unique valid sampled months
# contributing to the grid cell.

cell_totals <- cell_month_totals %>%
  group_by(
    cell_id,
    cell_number
  ) %>%
  summarise(
    
    # Grid-cell centre coordinates
    
    longitude = first(longitude),
    
    latitude = first(latitude),
    
    
    # Sampling effort:
    # Number of unique sampled cell-months
    
    n_cell_months = n_distinct(cell_month_id),
    
    
    # Total mosquito abundance across sampled months
    
    across(
      all_of(count_vars),
      safe_sum
    ),
    
    
    # Mean environmental conditions across sampled months
    
    tmin = safe_mean(tmin),
    
    tmax = safe_mean(tmax),
    
    precip = safe_mean(precip),
    
    travel = safe_mean(travel),
    
    
    # Mean continental log abundance offset
    
    log_continental_offset = safe_mean(
      log_continental_offset
    ),
    
    .groups = "drop"
  ) %>%
  filter(
    n_cell_months > 0,
    is.finite(n_cell_months),
    !is.na(an_gambiae_total),
    is.finite(an_gambiae_total)
  )


# ============================================================
# 14. CHECK FINAL MODELLING DATA
# ============================================================

if (nrow(cell_month_totals) == 0) {
  stop("Cell-month aggregation produced no records.")
}

if (nrow(cell_totals) == 0) {
  stop("Grid-cell aggregation produced no records.")
}


# Check for valid sampling effort

if (any(cell_totals$n_cell_months < 1)) {
  stop("Invalid sampling effort detected.")
}


# Check for negative mosquito counts

if (any(
  cell_totals$an_gambiae_total < 0,
  na.rm = TRUE
)) {
  stop("Negative An. gambiae counts detected.")
}


# ============================================================
# 15. SAVE MODELLING DATASETS
# ============================================================

write_csv(
  cell_month_totals,
  file.path(
    output_dir,
    "model_data_cell_month_totals.csv"
  )
)


write_csv(
  cell_totals,
  file.path(
    output_dir,
    "model_data_cell_totals.csv"
  )
)


# ============================================================
# 16. QUALITY CONTROL SUMMARY
# ============================================================

cat("\n")
cat("====================================================\n")
cat("MODELLING DATA PREPARATION COMPLETED\n")
cat("====================================================\n")


cat(
  "\nCell-month records:",
  nrow(cell_month_totals),
  "\n"
)

cat(
  "Unique grid cells:",
  nrow(cell_totals),
  "\n"
)


cat("\nSampling effort summary:\n")

print(
  summary(cell_totals$n_cell_months)
)


cat("\nAn. gambiae abundance summary:\n")

print(
  summary(cell_totals$an_gambiae_total)
)


cat("\nEnvironmental predictor summary:\n")

print(
  summary(
    cell_totals %>%
      select(
        tmin,
        tmax,
        precip,
        travel,
        log_continental_offset
      )
  )
)


cat("\nMissing values in final modelling variables:\n")

print(
  colSums(
    is.na(
      cell_totals %>%
        select(
          an_gambiae_total,
          n_cell_months,
          tmin,
          tmax,
          precip,
          travel,
          log_continental_offset
        )
    )
  )
)


cat("\nSaved files:\n")

cat(
  file.path(
    output_dir,
    "model_data_cell_month_totals.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "model_data_cell_totals.csv"
  ),
  "\n"
)


cat("\n====================================================\n")
cat("END OF 03_prepare_model_data.R\n")
cat("====================================================\n")



