
# 01b_data_QA.R: Data quality assessment

library(tidyverse)

x <- read_csv("data/processed/ento_clean.csv", show_col_types = FALSE)

dir.create("outputs/qa", recursive = TRUE, showWarnings = FALSE)

# 1. Coordinate quality
x |>
  mutate(coordinate_status = case_when(
    invalid_coordinate ~ "Invalid",
    is.na(longitude) | is.na(latitude) ~ "Missing",
    TRUE ~ "Valid"
  )) |>
  count(coordinate_status, sort = TRUE) |>
  write_csv("outputs/qa/coordinate_status.csv")

x |> filter(invalid_coordinate) |>
  write_csv("outputs/qa/invalid_coordinates.csv")

x |> filter(is.na(longitude) | is.na(latitude)) |>
  write_csv("outputs/qa/missing_coordinates.csv")

# 2. Extreme mosquito counts
x |>
  filter(an_gambiae_total >= 100) |>
  arrange(desc(an_gambiae_total)) |>
  write_csv("outputs/qa/extreme_gambiae_counts.csv")

# 3. Species count summary
species <- c(
  "an_gambiae_total", "an_funestus_total",
  "an_coustani_total", "other_anopheles_total",
  "anopheles_total"
)

tibble(
  species = species,
  missing = map_int(x[species], ~ sum(is.na(.x))),
  zero = map_int(x[species], ~ sum(.x == 0, na.rm = TRUE)),
  positive = map_int(x[species], ~ sum(.x > 0, na.rm = TRUE))
) |>
  write_csv("outputs/qa/species_missingness.csv")

# 4. Collection method summary
x |>
  count(collection_method, sort = TRUE) |>
  write_csv("outputs/qa/collection_method_summary.csv")

x |>
  group_by(collection_method) |>
  summarise(
    n = n(),
    gambiae_missing = sum(is.na(an_gambiae_total)),
    gambiae_positive = sum(an_gambiae_total > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  write_csv("outputs/qa/gambiae_by_collection_method.csv")

# 5. Site-month summary
site_month <- x |>
  group_by(site_month_id, district, sub_county, site_code,
           year, month_number) |>
  summarise(
    n_observations = n(),
    n_gps = sum(!is.na(longitude) & !is.na(latitude)),
    n_gambiae = sum(!is.na(an_gambiae_total)),
    .groups = "drop"
  )

write_csv(site_month, "outputs/qa/site_month_summary.csv")

site_month |>
  filter(n_gps == 0) |>
  write_csv("outputs/qa/site_months_no_gps.csv")

# 6. Key variable missingness
key <- c(
  "event_date", "longitude", "latitude", "district",
  "sub_county", "site_code", "house_type", "n_people",
  "n_nets", "people_under_net", "sprayed",
  "collection_method", "an_gambiae_total"
)

tibble(
  variable = key,
  missing = map_int(x[key], ~ sum(is.na(.x))),
  percent_missing = map_dbl(x[key], ~ mean(is.na(.x)) * 100)
) |>
  write_csv("outputs/qa/key_variable_missingness.csv")

# 7. Modelling eligibility
x |>
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(an_gambiae_total),
    an_gambiae_total >= 0
  ) |>
  write_csv("outputs/qa/gambiae_modelling_eligible_pre_covariates.csv")

cat("QA completed. Outputs saved in outputs/qa/\n")
