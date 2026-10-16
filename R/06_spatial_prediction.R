
library(terra)

# Load models
annual_M1  <- readRDS("outputs/models/annual_M1.rds")
annual_M2  <- readRDS("outputs/models/annual_M2.rds")
monthly_M1 <- readRDS("outputs/models/monthly_M1.rds")
monthly_M2 <- readRDS("outputs/models/monthly_M2.rds")

# Load covariates
annual <- rast("data/raw/covariates2(1).tif")
monthly <- rast("data/raw/worldclim/uganda_worldclim_monthly.tif")
continental <- rast("data/raw/continental/uganda_preds_p.tif")

names(annual) <- c("tmax", "tmin", "precip")

dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)

# Annual predictions
annual$log_effort <- 0

writeRaster(
  predict(annual, annual_M1, type = "response", na.rm = TRUE),
  "outputs/predictions/annual_M1.tif",
  overwrite = TRUE
)

continental_annual <- resample(continental[[4]], annual, method = "bilinear")
annual$log_continental <- log(continental_annual)

writeRaster(
  predict(annual, annual_M2, type = "response", na.rm = TRUE),
  "outputs/predictions/annual_M2.tif",
  overwrite = TRUE
)

# Align continental predictions to the monthly grid
template <- monthly[[1]]
continental_monthly <- resample(continental[[4]], template, method = "bilinear")

# Monthly predictions
for (m in 1:12) {
  
  x <- monthly[[c(m, m + 12, m + 24)]]
  names(x) <- c("tmin", "tmax", "precip")
  x <- resample(x, template, method = "bilinear")
  x$log_effort <- 0
  
  # M1
  p1 <- predict(x, monthly_M1, type = "response", na.rm = TRUE)
  
  writeRaster(
    p1,
    paste0("outputs/predictions/monthly_M1_", m, ".tif"),
    overwrite = TRUE
  )
  
  # M2
  x$log_continental <- log(continental_monthly)
  
  p2 <- predict(x, monthly_M2, type = "response", na.rm = TRUE)
  
  writeRaster(
    p2,
    paste0("outputs/predictions/monthly_M2_", m, ".tif"),
    overwrite = TRUE
  )
}


