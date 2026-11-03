# 01_clean_data.R
# Clean raw entomology records: dates, administrative units, coordinates,
# LLIN indicators and mosquito totals.

source(here::here("R", "config.R"))

check_files(paths$ento_raw)
make_dir(dirname(paths$ento_clean))

# Helpers -----------------------------------------------------------------------
# Row sum that returns NA (not 0) when every value is missing
row_sum <- function(df) {
  total <- rowSums(df, na.rm = TRUE)
  total[rowSums(!is.na(df)) == 0] <- NA_real_
  total
}

# Parse "lon, lat" strings (assumes longitude is listed first; see README)
parse_gps <- function(x) {
  nums <- str_extract_all(as.character(x), "[-+]?[0-9]*\\.?[0-9]+")
  tibble(
    longitude_gps = map_dbl(nums, \(n) if (length(n) >= 2) as.numeric(n[1]) else NA_real_),
    latitude_gps  = map_dbl(nums, \(n) if (length(n) >= 2) as.numeric(n[2]) else NA_real_)
  )
}

# Loose sanity box around Uganda; also excludes (0, 0)
in_uganda <- function(lon, lat) {
  !is.na(lon) & !is.na(lat) & between(lon, 29, 36) & between(lat, -2, 5)
}

# Source columns --------------------------------------------------------------
# Raw name pattern: mal_001_<code><letter>_<species>_<stage>, e.g.
# mal_001_er18a_an_gambiae_s_l_fed  ->  gambiae_fed
species <- tribble(
  ~short,     ~code,  ~stem,
  "gambiae",  "er18", "an_gambiae_s_l",
  "funestus", "er19", "an_funestus_s_l",
  "coustani", "er20", "an_coustani_s_l",
  "other",    "er21", "other_anopheles"
)
stages <- c(fed = "a", unfed = "b", gravid = "c", half_gravid = "d")

mosquito_cols <- species |>
  cross_join(tibble(stage = names(stages), letter = stages)) |>
  mutate(
    new = str_c(short, "_", stage),
    old = str_c("mal_001_", code, letter, "_", stem, "_", stage)
  )
mosquito_map <- set_names(mosquito_cols$old, mosquito_cols$new)  # new = old

renamed <- c(
  site_code        = "mal_001_er01_site_code",
  parish           = "mal_001_er03_parish",
  house_type       = "mal_001_er04_house_type",
  house_number     = "mal_001_er05_house_number",
  sprayed          = "mal_001_er06_has_site_sprayed_in_the_past_12_months",
  insecticide      = "mal_001_er07_insecticide_sprayed",
  n_people         = "mal_001_er13_no_of_people_who_slept_in_the_house",
  months_since_irs = "mal_001_er15_how_many_months_ago",
  n_nets           = "mal_001_er16_number_of_lli_ns_available",
  llin_type        = "mal_001_er17_type_of_llin",
  collection_method = "mal_001_er29_mosquito_collection_method_psc_ltc",
  llin_brand       = "mal_001_er28_brand_of_llin",
  people_under_net = "mal_001_er30_no_of_people_who_slept_under_llin"
)

# Read ------------------------------------------------------------------------
raw <- read_csv(
  paths$ento_raw,
  col_types = cols(.default = col_character()),
  trim_ws = TRUE
) |>
  janitor::clean_names()

required <- c(
  "organisation_unit_name_hierarchy", "event_date", "longitude", "latitude",
  "mal_001_er02_gps_co_ordinate", renamed, mosquito_map
)
missing <- setdiff(required, names(raw))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

# Administrative hierarchy: positions 2, 3, 5 and 6 of the DHIS2 path
hier <- str_split_fixed(raw$organisation_unit_name_hierarchy, "\\s*/\\s*", 6)
colnames(hier) <- c("level_1", "region", "district", "level_4",
                    "sub_county", "health_facility")
hier <- as_tibble(hier) |>
  select(region, district, sub_county, health_facility) |>
  mutate(across(everything(), \(x) na_if(str_squish(x), "")))

# Clean -------------------------------------------------------------------------
clean <- raw |>
  bind_cols(hier, parse_gps(raw$mal_001_er02_gps_co_ordinate)) |>
  rename(all_of(c(renamed, mosquito_map))) |>
  mutate(
    observation_id = row_number(),
    event_date     = dmy(event_date, quiet = TRUE),
    year           = year(event_date),
    month_number   = month(event_date),
    month          = factor(month.abb[month_number], levels = month.abb, ordered = TRUE),
    longitude_raw  = suppressWarnings(as.numeric(longitude)),
    latitude_raw   = suppressWarnings(as.numeric(latitude)),
    across(
      c(months_since_irs, n_people, n_nets, people_under_net, all_of(names(mosquito_map))),
      \(x) suppressWarnings(as.numeric(x))
    ),
    sprayed = case_match(
      str_to_lower(str_squish(sprayed)),
      c("yes", "true", "1") ~ "Yes",
      c("no", "false", "0")  ~ "No",
      .default = NA_character_
    ),
    # Prefer the primary coordinates; fall back to the ER02 GPS field
    primary_ok = in_uganda(longitude_raw, latitude_raw),
    gps_ok     = in_uganda(longitude_gps, latitude_gps),
    coordinate_source = case_when(
      primary_ok ~ "Primary_GPS",
      gps_ok     ~ "ER02_GPS",
      # (0, 0) in the primary columns is a placeholder, not a coordinate
      (!is.na(longitude_raw) & !(longitude_raw == 0 & latitude_raw == 0)) |
        !is.na(longitude_gps) ~ "Invalid_GPS",
      .default = "Missing"
    ),
    longitude = case_when(primary_ok ~ longitude_raw, gps_ok ~ longitude_gps),
    latitude  = case_when(primary_ok ~ latitude_raw,  gps_ok ~ latitude_gps),
    # Mosquito totals
    an_gambiae_total      = row_sum(pick(starts_with("gambiae_"))),
    an_funestus_total     = row_sum(pick(starts_with("funestus_"))),
    an_coustani_total     = row_sum(pick(starts_with("coustani_"))),
    other_anopheles_total = row_sum(pick(starts_with("other_"))),
    anopheles_total = row_sum(pick(
      an_gambiae_total, an_funestus_total, an_coustani_total, other_anopheles_total
    )),
    negative_abundance = coalesce(if_any(all_of(names(mosquito_map)), \(x) x < 0), FALSE),
    # LLIN indicators (NA if implausible: more people under nets than slept)
    llin_use = if_else(
      n_people > 0 & people_under_net <= n_people, people_under_net / n_people, NA_real_
    ),
    llin_per_person = if_else(n_people > 0, n_nets / n_people, NA_real_),
    # Identifiers
    site_id       = paste(region, district, sub_county, site_code, sep = "_"),
    household_id  = paste(site_id, coalesce(house_number, "UNKNOWN"), sep = "_"),
    site_year_id  = paste(site_id, year, sep = "_"),
    site_month_id = paste(site_id, year, month_number, sep = "_")
  ) |>
  add_count(site_month_id, name = "n_site_month_records") |>
  select(
    observation_id, event_date, year, month_number, month,
    region, district, sub_county, health_facility, site_code,
    site_id, household_id, site_year_id, site_month_id, n_site_month_records,
    parish, house_type, house_number, n_people, n_nets, people_under_net,
    llin_use, llin_per_person, llin_type, llin_brand,
    sprayed, insecticide, months_since_irs, collection_method,
    longitude, latitude, longitude_raw, latitude_raw, longitude_gps, latitude_gps,
    coordinate_source, negative_abundance, ends_with("_total"),
    all_of(names(mosquito_map))
  )

# Checks and output -------------------------------------------------------------
if (any(is.na(clean$event_date))) {
  warning(sum(is.na(clean$event_date)), " records have an unparseable event_date.")
}
count(clean, coordinate_source) |> print()

write_csv(clean, paths$ento_clean, na = "")
