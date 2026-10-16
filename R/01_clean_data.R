
# 01_clean_data.R
# Uganda malaria vector abundance modelling

library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)

# Paths -----------------------------------------------------------------------
raw_file <- "data/raw/ento_august.csv"
out_file <- "data/processed/ento_clean.csv"

if (!file.exists(raw_file)) stop("Input file not found: ", raw_file)
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# Helper functions ------------------------------------------------------------
check_required <- function(data, vars) {
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
}

parse_gps <- function(x) {
  nums <- str_extract_all(as.character(x), "[-+]?[0-9]*\\.?[0-9]+")
  tibble(
    longitude_gps = map_dbl(nums, ~
                              if (length(.x) >= 2) as.numeric(.x[1]) else NA_real_),
    latitude_gps = map_dbl(nums, ~
                             if (length(.x) >= 2) as.numeric(.x[2]) else NA_real_)
  )
}

row_total <- function(data) {
  values <- as.matrix(data)
  total <- rowSums(values, na.rm = TRUE)
  total[rowSums(!is.na(values)) == 0] <- NA_real_
  total
}

# Read data -------------------------------------------------------------------
raw <- readr::read_csv(
  raw_file,
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  janitor::clean_names()

# Mosquito source-to-output column mapping ------------------------------------
mosquito_map <- c(
  gambiae_fed = "mal_001_er18a_an_gambiae_s_l_fed",
  gambiae_unfed = "mal_001_er18b_an_gambiae_s_l_unfed",
  gambiae_gravid = "mal_001_er18c_an_gambiae_s_l_gravid",
  gambiae_half_gravid = "mal_001_er18d_an_gambiae_s_l_half_gravid",
  funestus_fed = "mal_001_er19a_an_funestus_s_l_fed",
  funestus_unfed = "mal_001_er19b_an_funestus_s_l_unfed",
  funestus_gravid = "mal_001_er19c_an_funestus_s_l_gravid",
  funestus_half_gravid = "mal_001_er19d_an_funestus_s_l_half_gravid",
  coustani_fed = "mal_001_er20a_an_coustani_s_l_fed",
  coustani_unfed = "mal_001_er20b_an_coustani_s_l_unfed",
  coustani_gravid = "mal_001_er20c_an_coustani_s_l_gravid",
  coustani_half_gravid = "mal_001_er20d_an_coustani_s_l_half_gravid",
  other_fed = "mal_001_er21a_other_anopheles_fed",
  other_unfed = "mal_001_er21b_other_anopheles_unfed",
  other_gravid = "mal_001_er21c_other_anopheles_gravid",
  other_half_gravid = "mal_001_er21d_other_anopheles_half_gravid"
)

required <- c(
  "organisation_unit_name_hierarchy", "event_date", "longitude", "latitude",
  "mal_001_er02_gps_co_ordinate", "mal_001_er01_site_code",
  "mal_001_er03_parish", "mal_001_er04_house_type",
  "mal_001_er05_house_number",
  "mal_001_er06_has_site_sprayed_in_the_past_12_months",
  "mal_001_er07_insecticide_sprayed",
  "mal_001_er15_how_many_months_ago",
  "mal_001_er13_no_of_people_who_slept_in_the_house",
  "mal_001_er16_number_of_lli_ns_available",
  "mal_001_er17_type_of_llin", "mal_001_er28_brand_of_llin",
  "mal_001_er30_no_of_people_who_slept_under_llin",
  "mal_001_er29_mosquito_collection_method_psc_ltc",
  unname(mosquito_map)
)
check_required(raw, required)

# Administrative hierarchy and GPS -------------------------------------------
hier <- str_split_fixed(
  raw$organisation_unit_name_hierarchy, "\\s*/\\s*", 6
)

gps <- parse_gps(raw$mal_001_er02_gps_co_ordinate)
lon <- suppressWarnings(as.numeric(raw$longitude))
lat <- suppressWarnings(as.numeric(raw$latitude))

primary_valid <- is.finite(lon) & is.finite(lat) &
  !(lon == 0 & lat == 0)

# Rename source variables ------------------------------------------------------
x <- raw |>
  dplyr::mutate(
    observation_id = dplyr::row_number(),
    region = str_squish(hier[, 2]),
    district = str_squish(hier[, 3]),
    sub_county = str_squish(hier[, 5]),
    health_facility = str_squish(hier[, 6]),
    longitude_raw = lon,
    latitude_raw = lat,
    longitude_gps = gps$longitude_gps,
    latitude_gps = gps$latitude_gps,
    longitude = if_else(primary_valid, lon, longitude_gps),
    latitude = if_else(primary_valid, lat, latitude_gps),
    event_date = suppressWarnings(dmy(event_date)),
    year = year(event_date),
    month_number = month(event_date),
    month = factor(month.abb[month_number],
                   levels = month.abb, ordered = TRUE)
  ) |>
  dplyr::rename(
    site_code = mal_001_er01_site_code,
    parish = mal_001_er03_parish,
    house_type = mal_001_er04_house_type,
    house_number = mal_001_er05_house_number,
    sprayed = mal_001_er06_has_site_sprayed_in_the_past_12_months,
    insecticide = mal_001_er07_insecticide_sprayed,
    months_since_irs = mal_001_er15_how_many_months_ago,
    n_people = mal_001_er13_no_of_people_who_slept_in_the_house,
    n_nets = mal_001_er16_number_of_lli_ns_available,
    llin_type = mal_001_er17_type_of_llin,
    llin_brand = mal_001_er28_brand_of_llin,
    people_under_net = mal_001_er30_no_of_people_who_slept_under_llin,
    collection_method = mal_001_er29_mosquito_collection_method_psc_ltc
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(months_since_irs, n_people, n_nets, people_under_net,
        dplyr::all_of(unname(mosquito_map))),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) |>
  dplyr::rename(
    !!!rlang::set_names(
      rlang::syms(unname(mosquito_map)),
      names(mosquito_map)
    )
  )

# Calculate species abundance totals ------------------------------------------
stages <- c("fed", "unfed", "gravid", "half_gravid")

for (sp in c("gambiae", "funestus", "coustani", "other")) {
  cols <- paste0(sp, "_", stages)
  total_name <- if (sp == "other") {
    "other_anopheles_total"
  } else {
    paste0("an_", sp, "_total")
  }
  x[[total_name]] <- row_total(x[cols])
}

# Derived variables and coordinate validation ---------------------------------
x <- x |>
  dplyr::mutate(
    sprayed = case_when(
      str_to_lower(str_squish(sprayed)) %in% c("yes", "true", "1") ~ "Yes",
      str_to_lower(str_squish(sprayed)) %in% c("no", "false", "0") ~ "No",
      TRUE ~ NA_character_
    ),
    all_anopheles_missing = if_all(
      c(an_gambiae_total, an_funestus_total,
        an_coustani_total, other_anopheles_total),
      is.na
    ),
    anopheles_total = if_else(
      all_anopheles_missing,
      NA_real_,
      rowSums(
        pick(an_gambiae_total, an_funestus_total,
             an_coustani_total, other_anopheles_total),
        na.rm = TRUE
      )
    ),
    invalid_coordinate = !is.na(longitude) & !is.na(latitude) &
      (longitude < 29 | longitude > 36 |
         latitude < -2 | latitude > 5),
    longitude_original = longitude,
    latitude_original = latitude,
    coordinate_source = case_when(
      invalid_coordinate ~ "Invalid_GPS",
      primary_valid ~ "Primary_GPS",
      !is.na(longitude_gps) & !is.na(latitude_gps) ~ "ER02_GPS",
      TRUE ~ "Missing"
    ),
    longitude = if_else(invalid_coordinate, NA_real_, longitude),
    latitude = if_else(invalid_coordinate, NA_real_, latitude),
    coordinate_missing = is.na(longitude) | is.na(latitude),
    llin_use = if_else(n_people > 0, people_under_net / n_people, NA_real_),
    llin_per_person = if_else(n_people > 0, n_nets / n_people, NA_real_),
    site_id = paste(region, district, sub_county, site_code, sep = "_"),
    household_id = paste(site_id, coalesce(house_number, "UNKNOWN"), sep = "_"),
    site_year_id = paste(site_id, year, sep = "_"),
    site_month_id = paste(
      district, sub_county, site_code, year, month_number, sep = "_"
    ),
    location_id = if_else(
      !is.na(longitude) & !is.na(latitude),
      paste(round(longitude, 5), round(latitude, 5), sep = "_"),
      NA_character_
    ),
    negative_abundance = !is.na(an_gambiae_total) & an_gambiae_total < 0
  ) |>
  dplyr::add_count(site_month_id, name = "effort")

# Select final columns ---------------------------------------------------------
clean <- x |>
  dplyr::select(
    observation_id, event_date, year, month_number, month,
    region, district, sub_county, health_facility, site_code,
    site_id, household_id, site_year_id, site_month_id, location_id,
    parish, house_type, house_number, n_people, n_nets, people_under_net,
    llin_use, llin_per_person, llin_type, llin_brand, sprayed,
    insecticide, months_since_irs, collection_method, effort,
    longitude, latitude, longitude_original, latitude_original,
    longitude_raw, latitude_raw, longitude_gps, latitude_gps,
    coordinate_source, coordinate_missing, invalid_coordinate,
    negative_abundance, an_gambiae_total, an_funestus_total,
    an_coustani_total, other_anopheles_total, anopheles_total,
    dplyr::matches("^(gambiae|funestus|coustani|other)_")
  )

# Save cleaned data ------------------------------------------------------------
readr::write_csv(clean, out_file, na = "")


