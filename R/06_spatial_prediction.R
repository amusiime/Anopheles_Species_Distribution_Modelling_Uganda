
# 06_spatial_prediction.R
# National spatial predictions: Anopheles gambiae
# M1: Combined environmental model
# M2: Monthly environmental model

suppressPackageStartupMessages({
  library(terra)
})

# ------------------------------------------------------------
# 1. LOAD MODELS AND CREATE OUTPUT DIRECTORY
# ------------------------------------------------------------

models <- readRDS("outputs/models/abundance_models.rds")

M1 <- models$M1
M2 <- models$M2

dir.create(
  "outputs/predictions",
  recursive = TRUE,
  showWarnings = FALSE
)

# Reference sampling effort = 1
prepare_effort <- function(template) {
  x <- terra::init(template, 0)
  names(x) <- "log_effort"
  x
}

# ------------------------------------------------------------
# 2. LOAD AND ALIGN TRAVEL ACCESSIBILITY
# ------------------------------------------------------------

travel_source <- rast(
  "data/processed/travel_accessibility_1km.tif"
)

# ------------------------------------------------------------
# 3. M1: COMBINED ENVIRONMENTAL PREDICTION
# ------------------------------------------------------------

env <- rast(
  "data/processed/environment_1km.tif"
)

names(env) <- tolower(names(env))

required_bio <- c(
  "bio6_min_temp",
  "bio5_max_temp",
  "bio12_precip"
)

if (!all(required_bio %in% names(env))) {
  stop(
    "M1 requires environmental layers: ",
    paste(required_bio, collapse = ", ")
  )
}

env <- env[[required_bio]]

# Align travel raster with environmental raster
travel_M1 <- project(
  travel_source,
  env[[1]],
  method = "bilinear"
)

names(travel_M1) <- "travel"

# Create reference sampling effort
effort_M1 <- prepare_effort(env[[1]])

# Combine predictors in model order
predictors_M1 <- c(
  env,
  travel_M1,
  effort_M1
)


# Generate annual prediction
prediction_M1 <- terra::predict(
  predictors_M1,
  M1,
  type = "response",
  na.rm = TRUE,
  filename = "outputs/predictions/M1_annual_1km.tif",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

names(prediction_M1) <- "an_gambiae_predicted"


# Monthly

monthly_file <- "data/processed/monthly_climate_uganda_1km.tif"

if (file.exists(monthly_file)) {
  
  monthly_env <- rast(monthly_file)
  names(monthly_env) <- tolower(names(monthly_env))
  
  required_monthly <- unlist(lapply(tolower(month.abb), function(m) {
    paste0(c("tmin_", "tmax_", "precip_"), m)
  }))
  
  monthly_env <- monthly_env[[required_monthly]]
}
  
  # ----------------------------------------------------------
  #PREPARE PREDICTORS
  # ----------------------------------------------------------
  
  travel_M2 <- project(
    travel_source,
    monthly_env[[1]],
    method = "bilinear"
  )
  
  names(travel_M2) <- "travel"
  
  effort_M2 <- prepare_effort(monthly_env[[1]])
  
  monthly_predictions <- vector("list", 12)
  
  
  # ----------------------------------------------------------
  # 3. GENERATE MONTHLY PREDICTIONS
  # ----------------------------------------------------------
  
  for (m in seq_len(12)) {
    
    month <- tolower(month.abb[m])
    
    layers <- paste0(c("tmin_", "tmax_", "precip_"), month)
    
    env_month <- monthly_env[[layers]]
    names(env_month) <- c("tmin", "tmax", "precip")
    
    predictors_M2 <- c(env_month, travel_M2, effort_M2)
    
    monthly_predictions[[m]] <- terra::predict(
      predictors_M2,
      M2,
      type = "response",
      na.rm = TRUE,
      filename = paste0(
        "outputs/predictions/M2_",
        month.abb[m],
        "_1km.tif"
      ),
      overwrite = TRUE,
      wopt = list(gdal = "COMPRESS=LZW")
    )
    
    names(monthly_predictions[[m]]) <- month.abb[m]
  }
  

  # ----------------------------------------------------------
  # SAVE COMBINED MONTHLY PREDICTIONS
  # ----------------------------------------------------------
  
  prediction_M2 <- do.call(c, monthly_predictions)
  
  writeRaster(
    prediction_M2,
    "outputs/predictions/M2_monthly_1km.tif",
    overwrite = TRUE,
    wopt = list(gdal = "COMPRESS=LZW")
  )
  

  
  
  












