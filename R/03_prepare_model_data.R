
library(tidyverse)
library(terra)

# 1. Load data and rasters
d <- read_csv("data/processed/ento_clean.csv", show_col_types = FALSE)
annual <- rast("data/raw/covariates2(1).tif")
monthly <- rast("data/raw/worldclim/uganda_worldclim_monthly.tif")
continental <- rast("data/raw/continental/uganda_preds_p.tif")

names(annual) <- c("tmax", "tmin", "precip")
template <- annual[[1]]

# 2. Assign grid cells and extract annual covariates
pts <- vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326")
pts <- project(pts, crs(template))

d$cell_number <- cellFromXY(template, crds(pts))


d <- bind_cols(d, extract(annual, pts)[, -1, drop = FALSE]) |>
  mutate(
    continental_gambiae = extract(continental[[4]], pts)[, 2]
  ) |>
  filter(!is.na(cell_number), continental_gambiae > 0) |>
  group_by(cell_number) |>
  mutate(effort = n()) |>
  ungroup() |>
  mutate(
    log_effort = log(effort),
    log_continental = log(continental_gambiae)
  )



# 3. Save annual dataset
vars <- c(
  "an_gambiae_total", "cell_number", "year", "month",
  "tmin", "tmax", "precip", "log_effort", "log_continental"
)

d |>
  dplyr::select(all_of(vars)) |>
  drop_na() |>
  write_csv("data/processed/model_data_annual.csv")

# 4. Extract monthly covariates
month_num <- match(str_to_title(as.character(d$month)), month.abb)
month_num[is.na(month_num)] <- as.integer(as.character(d$month[is.na(month_num)]))

d$tmin <- d$tmax <- d$precip <- NA_real_

for (m in 1:12) {
  i <- which(month_num == m)
  if (length(i) == 0) next
  
  v <- extract(monthly[[c(m, m + 12, m + 24)]], pts[i, ])
  d$tmin[i] <- v[[2]]
  d$tmax[i] <- v[[3]]
  d$precip[i] <- v[[4]]
}

# 5. Save monthly dataset
d |>
  dplyr::select(dplyr::all_of(vars)) |>
  tidyr::drop_na() |>
  readr::write_csv("data/processed/model_data_monthly.csv")

