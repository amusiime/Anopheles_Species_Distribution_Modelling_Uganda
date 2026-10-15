library(terra)

# Load models and rasters
annual_M1  <- readRDS("outputs/models/annual_M1.rds")
annual_M2  <- readRDS("outputs/models/annual_M2.rds")
monthly_M1 <- readRDS("outputs/models/monthly_M1.rds")
monthly_M2 <- readRDS("outputs/models/monthly_M2.rds")

annual <- rast("data/raw/covariates2(1).tif")
monthly <- rast("data/raw/worldclim/uganda_worldclim_monthly.tif")
continental <- rast("data/raw/continental/uganda_preds_p.tif")

names(annual) <- c("tmax", "tmin", "precip")
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)

# Annual predictions
annual$log_effort <- 0
p <- predict(annual, annual_M1, type = "response", na.rm = TRUE)
writeRaster(p, "outputs/predictions/annual_M1.tif", overwrite = TRUE)

continental_annual <- resample(continental[[4]], annual, method = "bilinear")
annual$log_continental <- log(continental_annual)
p <- predict(annual, annual_M2, type = "response", na.rm = TRUE)
writeRaster(p, "outputs/predictions/annual_M2.tif", overwrite = TRUE)

# Monthly predictions
for (m in 1:12) {
  x <- monthly[[c(m, m + 12, m + 24)]]
  names(x) <- c("tmin", "tmax", "precip")
  x$log_effort <- 0
  
  p <- predict(x, monthly_M1, type = "response", na.rm = TRUE)
  writeRaster(p, paste0("outputs/predictions/monthly_M1_", m, ".tif"),
              overwrite = TRUE)
  
  continental_monthly <- resample(continental[[4]], x, method = "bilinear")
  x$log_continental <- log(continental_monthly)
  p <- predict(x, monthly_M2, type = "response", na.rm = TRUE)
  writeRaster(p, paste0("outputs/predictions/monthly_M2_", m, ".tif"),
              overwrite = TRUE)
}
