
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

cat("\nPreparing M1 annual prediction...\n")

env <- rast(
  "data/processed/environment_1km.tif"
)

names(env) <- tolower(names(env))

required_bio <- c("bio1", "bio5", "bio12")

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

# Check predictor names
required_M1 <- c(
  "bio1", "bio5", "bio12",
  "travel", "log_effort"
)

if (!identical(names(predictors_M1), required_M1)) {
  stop("M1 prediction layers do not match model predictors.")
}

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

cat("M1 annual prediction completed.\n")

# ------------------------------------------------------------
# 4. M2: MONTHLY CLIMATE PREDICTIONS
# ------------------------------------------------------------

monthly_file <- "data/processed/monthly_climate_uganda_1km.tif"

if (!file.exists(monthly_file)) {
  
  cat("\nM2 skipped: monthly climate raster not found.\n")
  
} else {
  
  monthly_env <- rast(monthly_file)
  
  names(monthly_env) <- tolower(names(monthly_env))
  
  # Expected 36 monthly climate layers
  required_monthly <- unlist(
    lapply(
      tolower(month.abb),
      function(month) {
        c(
          paste0("tmin_", month),
          paste0("tmax_", month),
          paste0("precip_", month)
        )
      }
    )
  )
  
  missing_layers <- setdiff(
    required_monthly,
    names(monthly_env)
  )
  
  if (length(missing_layers) > 0) {
    
    cat(
      "\nM2 skipped: monthly climate raster is incomplete.\n"
    )
    
    cat(
      "Missing layers:",
      paste(missing_layers, collapse = ", "),
      "\n"
    )
    
  } else {
    
    cat("\nPreparing M2 monthly predictions...\n")
    
    # Align travel raster with monthly climate raster
    travel_M2 <- project(
      travel_source,
      monthly_env[[1]],
      method = "bilinear"
    )
    
    names(travel_M2) <- "travel"
    
    effort_M2 <- prepare_effort(monthly_env[[1]])
    
    monthly_predictions <- vector("list", 12)
    
    for (m in seq_len(12)) {
      
      month <- tolower(month.abb[m])
      
      cat("Predicting:", month.abb[m], "\n")
      
      layers <- c(
        paste0("tmin_", month),
        paste0("tmax_", month),
        paste0("precip_", month)
      )
      
      env_month <- monthly_env[[layers]]
      
      names(env_month) <- c(
        "tmin",
        "tmax",
        "precip"
      )
      
      predictors_M2 <- c(
        env_month,
        travel_M2,
        effort_M2
      )
      
      # Check predictor names
      required_M2 <- c(
        "tmin", "tmax", "precip",
        "travel", "log_effort"
      )
      
      if (!identical(names(predictors_M2), required_M2)) {
        stop(
          "M2 predictor mismatch for ",
          month.abb[m]
        )
      }
      
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
    
    # Combine all 12 monthly predictions
    prediction_M2 <- do.call(
      c,
      monthly_predictions
    )
    
    writeRaster(
      prediction_M2,
      "outputs/predictions/M2_monthly_1km.tif",
      overwrite = TRUE,
      wopt = list(gdal = "COMPRESS=LZW")
    )
    
    cat("M2 monthly predictions completed.\n")
  }
}

# ------------------------------------------------------------
# 5. REPORT OUTPUTS
# ------------------------------------------------------------

cat("\nSpatial prediction script completed.\n")

cat(
  "\nM1 output:",
  "outputs/predictions/M1_annual_1km.tif\n"
)

if (file.exists(
  "outputs/predictions/M2_monthly_1km.tif"
)) {
  cat(
    "M2 output:",
    "outputs/predictions/M2_monthly_1km.tif\n"
  )
} else {
  cat("M2 output: Not generated (monthly climate data unavailable).\n")
}











