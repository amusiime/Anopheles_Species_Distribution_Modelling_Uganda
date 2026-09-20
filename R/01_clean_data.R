# ============================================================
# 01_clean_data.R
# MALARIA VECTOR ABUNDANCE MODELLING - UGANDA
# ============================================================

source("functions/helpers.R")

library(tidyverse)
library(janitor)
library(lubridate)

# ------------------------------------------------------------
# 1. FILES
# ------------------------------------------------------------

raw_file <- "data/raw/ento_august.csv"
out_file <- "data/processed/ento_clean.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/qa", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. READ DATA
# ------------------------------------------------------------

raw <- read_csv(
  raw_file,
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  clean_names()

required <- c(
  "organisation_unit_name_hierarchy", "event_date", "longitude", "latitude",
  "mal_001_er02_gps_co_ordinate", "mal_001_er01_site_code",
  "mal_001_er03_parish", "mal_001_er04_house_type", "mal_001_er05_house_number",
  "mal_001_er06_has_site_sprayed_in_the_past_12_months",
  "mal_001_er07_insecticide_sprayed", "mal_001_er15_how_many_months_ago",
  "mal_001_er13_no_of_people_who_slept_in_the_house",
  "mal_001_er16_number_of_lli_ns_available", "mal_001_er17_type_of_llin",
  "mal_001_er28_brand_of_llin", "mal_001_er30_no_of_people_who_slept_under_llin",
  "mal_001_er29_mosquito_collection_method_psc_ltc"
)

check_required(raw, required)

# ------------------------------------------------------------
# 3. ADMINISTRATIVE HIERARCHY AND GPS
# ------------------------------------------------------------

hier <- str_split_fixed(
  raw$organisation_unit_name_hierarchy,
  "\\s*/\\s*", 6
)

gps <- parse_gps(raw$mal_001_er02_gps_co_ordinate)

lon <- suppressWarnings(as.numeric(raw$longitude))
lat <- suppressWarnings(as.numeric(raw$latitude))

primary_valid <- is.finite(lon) & is.finite(lat) &
  !(lon == 0 & lat == 0)

# ------------------------------------------------------------
# 4. CLEAN CORE VARIABLES
# ------------------------------------------------------------

x <- raw |>
  mutate(
    observation_id = row_number(),
    region = str_squish(hier[, 2]),
    district = str_squish(hier[, 3]),
    sub_county = str_squish(hier[, 5]),
    health_facility = str_squish(hier[, 6])
  ) |>
  rename(
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
  mutate(
    across(
      c(months_since_irs, n_people, n_nets, people_under_net),
      ~ suppressWarnings(as.numeric(.x))
    ),
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
  )

# ------------------------------------------------------------
# 5. MOSQUITO ABUNDANCE
# ------------------------------------------------------------

mosquito_vars <- c(
  "mal_001_er18a_an_gambiae_s_l_fed",
  "mal_001_er18b_an_gambiae_s_l_unfed",
  "mal_001_er18c_an_gambiae_s_l_gravid",
  "mal_001_er18d_an_gambiae_s_l_half_gravid",
  "mal_001_er19a_an_funestus_s_l_fed",
  "mal_001_er19b_an_funestus_s_l_unfed",
  "mal_001_er19c_an_funestus_s_l_gravid",
  "mal_001_er19d_an_funestus_s_l_half_gravid",
  "mal_001_er20a_an_coustani_s_l_fed",
  "mal_001_er20b_an_coustani_s_l_unfed",
  "mal_001_er20c_an_coustani_s_l_gravid",
  "mal_001_er20d_an_coustani_s_l_half_gravid",
  "mal_001_er21a_other_anopheles_fed",
  "mal_001_er21b_other_anopheles_unfed",
  "mal_001_er21c_other_anopheles_gravid",
  "mal_001_er21d_other_anopheles_half_gravid"
)

check_required(x, mosquito_vars)

x <- x |>
  mutate(
    across(all_of(mosquito_vars), ~ suppressWarnings(as.numeric(.x)))
  ) |>
  rename(
    gambiae_fed = mal_001_er18a_an_gambiae_s_l_fed,
    gambiae_unfed = mal_001_er18b_an_gambiae_s_l_unfed,
    gambiae_gravid = mal_001_er18c_an_gambiae_s_l_gravid,
    gambiae_half_gravid = mal_001_er18d_an_gambiae_s_l_half_gravid,
    funestus_fed = mal_001_er19a_an_funestus_s_l_fed,
    funestus_unfed = mal_001_er19b_an_funestus_s_l_unfed,
    funestus_gravid = mal_001_er19c_an_funestus_s_l_gravid,
    funestus_half_gravid = mal_001_er19d_an_funestus_s_l_half_gravid,
    coustani_fed = mal_001_er20a_an_coustani_s_l_fed,
    coustani_unfed = mal_001_er20b_an_coustani_s_l_unfed,
    coustani_gravid = mal_001_er20c_an_coustani_s_l_gravid,
    coustani_half_gravid = mal_001_er20d_an_coustani_s_l_half_gravid,
    other_fed = mal_001_er21a_other_anopheles_fed,
    other_unfed = mal_001_er21b_other_anopheles_unfed,
    other_gravid = mal_001_er21c_other_anopheles_gravid,
    other_half_gravid = mal_001_er21d_other_anopheles_half_gravid
  )

gambiae <- grep("^gambiae_", names(x), value = TRUE)
funestus <- grep("^funestus_", names(x), value = TRUE)
coustani <- grep("^coustani_", names(x), value = TRUE)
other <- grep("^other_", names(x), value = TRUE)

x <- x |>
  mutate(
    an_gambiae_total = row_total_preserve_na(pick(all_of(gambiae)), gambiae),
    an_funestus_total = row_total_preserve_na(pick(all_of(funestus)), funestus),
    an_coustani_total = row_total_preserve_na(pick(all_of(coustani)), coustani),
    other_anopheles_total = row_total_preserve_na(pick(all_of(other)), other)
  )

# ------------------------------------------------------------
# 6. DERIVED VARIABLES AND DATA QUALITY
# ------------------------------------------------------------

x <- x |>
  mutate(
    sprayed = case_when(
      str_to_lower(str_squish(sprayed)) %in% c("yes", "true", "1") ~ "Yes",
      str_to_lower(str_squish(sprayed)) %in% c("no", "false", "0") ~ "No",
      TRUE ~ NA_character_
    ),
    
    anopheles_total = rowSums(
      pick(an_gambiae_total, an_funestus_total,
           an_coustani_total, other_anopheles_total),
      na.rm = TRUE
    ),
    
    all_anopheles_missing = if_all(
      c(an_gambiae_total, an_funestus_total,
        an_coustani_total, other_anopheles_total),
      is.na
    ),
    
    anopheles_total = if_else(
      all_anopheles_missing, NA_real_, anopheles_total
    ),
    
    invalid_coordinate =
      !is.na(longitude) & !is.na(latitude) &
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
    
    llin_use = if_else(
      n_people > 0, people_under_net / n_people, NA_real_
    ),
    llin_per_person = if_else(
      n_people > 0, n_nets / n_people, NA_real_
    ),
    
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
    
    negative_abundance =
      !is.na(an_gambiae_total) & an_gambiae_total < 0
  ) |>
  add_count(site_month_id, name = "effort")

# ------------------------------------------------------------
# 7. SAVE CLEAN DATA
# ------------------------------------------------------------

clean <- x |>
  select(
    observation_id, any_of("event"), event_date, year, month_number, month,
    region, district, sub_county, health_facility, site_code, site_id,
    household_id, site_year_id, site_month_id, location_id,
    parish, house_type, house_number,
    n_people, n_nets, people_under_net, llin_use, llin_per_person,
    llin_type, llin_brand, sprayed, insecticide, months_since_irs,
    collection_method, effort,
    longitude, latitude, longitude_original, latitude_original,
    longitude_raw, latitude_raw, longitude_gps, latitude_gps,
    coordinate_source, coordinate_missing, invalid_coordinate,
    negative_abundance,
    an_gambiae_total, an_funestus_total, an_coustani_total,
    other_anopheles_total, anopheles_total,
    matches("^(gambiae|funestus|coustani|other)_")
  )

write_csv(clean, out_file, na = "")

# ------------------------------------------------------------
# 8. QA SUMMARY
# ------------------------------------------------------------

qa <- clean |>
  summarise(
    observations = n(),
    regions = n_distinct(region, na.rm = TRUE),
    districts = n_distinct(district, na.rm = TRUE),
    sub_counties = n_distinct(sub_county, na.rm = TRUE),
    sites = n_distinct(site_id, na.rm = TRUE),
    site_months = n_distinct(site_month_id, na.rm = TRUE),
    valid_gps = sum(!coordinate_missing),
    missing_gps = sum(coordinate_missing),
    invalid_gps = sum(invalid_coordinate, na.rm = TRUE),
    gambiae_missing = sum(is.na(an_gambiae_total)),
    gambiae_zero = sum(an_gambiae_total == 0, na.rm = TRUE),
    gambiae_positive = sum(an_gambiae_total > 0, na.rm = TRUE)
  )

write_csv(qa, "outputs/qa/cleaning_summary.csv")

message("Saved ", out_file, " with ", nrow(clean), " observations.")
