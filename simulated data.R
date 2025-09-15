
library(tidyverse)

set.seed(2025)  

# Parameters
n_regions <- 15
n_districts <- 3
n_villages <- 3
n_households_per_village <- 20
n_people_sleeping_per_household <- 3:10
n_nets_per_hh <- 1:5
net_types <- c("Permanet","Permanet 3.0","PBO net","Olyset net","Olyset Plus")
house_sprayed <- c("yes","no")
months_sprayed <- 1:12 
female_gambiae <- 0:250
female_funestus <- 0:400
female_constani <- 0:100
has_net <- c("yes","no")
has_the_house_sprayed_before <- c("yes","no")
# bounding box for Uganda (approx)
lon_min <- 29.5
lon_max <- 35.0
lat_min <- -1.5
lat_max <- 4.5

# create household-level dataset with fixed coordinates

hh_coord <- expand_grid(
  region_id = 1:n_regions,
  district_id = 1:n_districts,
  village_id = 1:n_villages,
  household_id = 1:n_households_per_village
) %>%
  mutate(
    id = paste(region_id, district_id, village_id, household_id, sep = "_"),
    lon = runif(n(), lon_min, lon_max),
    lat = runif(n(), lat_min, lat_max)
  ) |> 
  select(id, lon, lat)


# Monthly survey dates
date_range <- seq.Date(from = as.Date("2020-01-01"),
                       to   = as.Date("2024-12-31"),
                       by   = "month")


# create household-level dataset with fixed coordinates
# Household × Month structure

hh_month_attributes <- expand_grid(
  region_id = 1:n_regions,
  district_id = 1:n_districts,
  village_id = 1:n_villages,
  household_id = 1:n_households_per_village,
  date = date_range
) |> 
  mutate(
    id = paste(region_id, district_id, village_id, household_id, sep = "_"),
    year_month = format(date, "%Y-%m"),   # group by month
    people_sleeping = sample(n_people_sleeping_per_household, n(), replace = TRUE),
    nets_owned = sample(n_nets_per_hh, n(), replace = TRUE),
    net_type = sample(net_types, n(), replace = TRUE),
    sprayed = sample(house_sprayed, n(), replace = TRUE, prob = c(0.3,0.7)),
    months_since_spray = ifelse(sprayed == "yes", sample(months_sprayed, n(), replace = TRUE), NA),
    has_net = sample(has_net, n(), replace = TRUE, prob = c(0.8,0.2)),
    sprayed_before = sample(has_the_house_sprayed_before, n(), replace = TRUE)
  )

# add mosquito counts using Poisson distribution

sim_data1 <- hh_month_attributes %>%
  mutate(
    gambiae_count  = rpois(n(), lambda = 15),
    funestus_count = rpois(n(), lambda = 20),
    constani_count = rpois(n(), lambda = 6)
  )


sim_data <-sim_data1|> 
          left_join(hh_coord,by = "id")

write.csv(sim_data, "simulated_data.csv", row.names = FALSE)
  
  
  
  
