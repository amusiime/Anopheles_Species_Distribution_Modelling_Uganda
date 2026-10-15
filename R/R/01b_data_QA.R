# ============================================================
# 01b_data_QA.R
# DATA QUALITY ASSESSMENT
# ============================================================
library(tidyverse)

x <- readr::read_csv("data/processed/ento_clean.csv", show_col_types = FALSE)
dir.create("outputs/qa", recursive = TRUE, showWarnings = FALSE)

coord_status <- x |> mutate(coordinate_status = case_when(
  invalid_coordinate ~ "Invalid",
  is.na(longitude) | is.na(latitude) ~ "Missing",
  TRUE ~ "Valid"
)) |> count(coordinate_status, sort=TRUE)
write_csv(coord_status, "outputs/qa/coordinate_status.csv")

x |> filter(invalid_coordinate) |> write_csv("outputs/qa/invalid_coordinates.csv")
x |> filter(is.na(longitude) | is.na(latitude)) |> write_csv("outputs/qa/missing_coordinates.csv")

x |> filter(an_gambiae_total >= 100) |> arrange(desc(an_gambiae_total)) |> write_csv("outputs/qa/extreme_gambiae_counts.csv")

species_missingness <- tibble(
  species = c("An. gambiae s.l.","An. funestus s.l.","An. coustani s.l.","Other Anopheles","Total Anopheles"),
  missing = c(sum(is.na(x$an_gambiae_total)),sum(is.na(x$an_funestus_total)),sum(is.na(x$an_coustani_total)),sum(is.na(x$other_anopheles_total)),sum(is.na(x$anopheles_total))),
  zero = c(sum(x$an_gambiae_total==0,na.rm=TRUE),sum(x$an_funestus_total==0,na.rm=TRUE),sum(x$an_coustani_total==0,na.rm=TRUE),sum(x$other_anopheles_total==0,na.rm=TRUE),sum(x$anopheles_total==0,na.rm=TRUE)),
  positive = c(sum(x$an_gambiae_total>0,na.rm=TRUE),sum(x$an_funestus_total>0,na.rm=TRUE),sum(x$an_coustani_total>0,na.rm=TRUE),sum(x$other_anopheles_total>0,na.rm=TRUE),sum(x$anopheles_total>0,na.rm=TRUE))
)
write_csv(species_missingness, "outputs/qa/species_missingness.csv")

x |> count(collection_method, sort=TRUE) |> write_csv("outputs/qa/collection_method_summary.csv")
x |> group_by(collection_method) |> summarise(n=n(), gambiae_missing=sum(is.na(an_gambiae_total)), gambiae_positive=sum(an_gambiae_total>0,na.rm=TRUE), .groups="drop") |> write_csv("outputs/qa/gambiae_by_collection_method.csv")

site_month <- x |> group_by(site_month_id, district, sub_county, site_code, year, month_number) |> summarise(n_observations=n(), n_gps=sum(!is.na(longitude)&!is.na(latitude)), n_gambiae=sum(!is.na(an_gambiae_total)), .groups="drop")
write_csv(site_month, "outputs/qa/site_month_summary.csv")
site_month |> filter(n_gps==0) |> write_csv("outputs/qa/site_months_no_gps.csv")

x |> summarise(min=min(effort,na.rm=TRUE), q1=quantile(effort,.25,na.rm=TRUE), median=median(effort,na.rm=TRUE), mean=mean(effort,na.rm=TRUE), q3=quantile(effort,.75,na.rm=TRUE), max=max(effort,na.rm=TRUE)) |> write_csv("outputs/qa/sampling_effort_summary.csv")

key <- c("event_date","longitude","latitude","district","sub_county","site_code","house_type","n_people","n_nets","people_under_net","sprayed","collection_method","an_gambiae_total","effort")
tibble(variable=key, missing=sapply(x[key], function(z) sum(is.na(z))), percent_missing=sapply(x[key], function(z) mean(is.na(z))*100)) |> write_csv("outputs/qa/key_variable_missingness.csv")

eligible <- x |> filter(!is.na(longitude),!is.na(latitude),!is.na(an_gambiae_total),an_gambiae_total>=0) |> select(observation_id,everything())
write_csv(eligible, "outputs/qa/gambiae_modelling_eligible_pre_covariates.csv")

message("QA completed. Files written to outputs/qa/")


library(sf)
library(dplyr)
library(ggplot2)

uganda_counties <- st_read(
  "data/raw/gadm41_UGA_2.geojson.json",
  quiet = TRUE
)

uganda_counties <- uganda_counties %>%
  select(
    GID_2,
    GID_1,
    COUNTRY,
    NAME_1,
    NAME_2,
    TYPE_2,
    geometry
  ) %>%
  st_transform(4326) %>%
  st_make_valid()

ggplot(uganda_counties) +
  geom_sf(
    fill = "grey90",
    colour = "grey40",
    linewidth = 0.25
  ) +
  labs(
    title = "Uganda Administrative Boundaries",
    subtitle = "Administrative Level 2"
  ) +
  theme_minimal()




uganda_counties <- st_read(
  "data/raw/gadm41_UGA_2.shp"
)

plot(uganda_counties["NAME_1"])


