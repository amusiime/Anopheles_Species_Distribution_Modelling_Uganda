
library(tidyverse)
library(terra)
library(geodata)

# 1. Set paths
input <- "data/processed/ento_clean.csv"
out_dir <- "data/raw/worldclim"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Download monthly WorldClim data for Uganda
tmin <- worldclim_country("Uganda", "tmin", res = 0.5, path = out_dir)
tmax <- worldclim_country("Uganda", "tmax", res = 0.5, path = out_dir)
prec <- worldclim_country("Uganda", "prec", res = 0.5, path = out_dir)

names(tmin) <- paste0("tmin_", month.abb)
names(tmax) <- paste0("tmax_", month.abb)
names(prec) <- paste0("prec_", month.abb)

# 3. Save monthly covariates
monthly <- c(tmin, tmax, prec)

writeRaster(
  monthly,
  file.path(out_dir, "uganda_worldclim_monthly.tif"),
  overwrite = TRUE
)

# 4. Prepare and save annual covariates
annual <- c(
  app(tmin, mean, na.rm = TRUE),
  app(tmax, mean, na.rm = TRUE),
  app(prec, sum, na.rm = TRUE)
)

names(annual) <- c("tmin_annual", "tmax_annual", "prec_annual")

writeRaster(
  annual,
  file.path(out_dir, "uganda_worldclim_annual.tif"),
  overwrite = TRUE
)

# 5. Read surveillance data and extract annual covariates
d <- read_csv(input, show_col_types = FALSE)

points <- vect(
  d,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

annual_values <- extract(annual, project(points, crs(annual)))
d <- bind_cols(d, annual_values[, -1])

# 6. Extract monthly covariates for each observation
month_num <- match(str_to_title(as.character(d$month)), month.abb)

d$tmin_monthly <- NA_real_
d$tmax_monthly <- NA_real_
d$prec_monthly <- NA_real_

for (m in 1:12) {
  i <- which(month_num == m)
  
  if (length(i) == 0) next
  
  p <- project(points[i, ], crs(monthly))
  
  values <- extract(
    monthly[[c(m, m + 12, m + 24)]],
    p
  )
  
  d$tmin_monthly[i] <- values[[2]]
  d$tmax_monthly[i] <- values[[3]]
  d$prec_monthly[i] <- values[[4]]
}

# 7. Save covariate dataset for Script 03
write_csv(
  d,
  "data/processed/ento_spatial_covariates.csv"
)



