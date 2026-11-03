
# 06_spatial_prediction.R
# Annual and monthly spatial abundance predictions

source(here::here("R", "config.R"))
source(here::here("R", "prediction_inputs.R"))

dir.create(paths$predictions, recursive = TRUE, showWarnings = FALSE)

# Load models and prediction inputs
models <- list(
  annual = readRDS(paths$models_annual),
  monthly = readRDS(paths$models_monthly)
)

model_names <- c("M3", "M4")

ranges <- list(
  annual = read_training_ranges("annual"),
  monthly = read_training_ranges("monthly")
)

inp <- load_prediction_inputs()

# Prediction function
predict_map <- \(model, climate, file, dataset, save_outside = FALSE) {
  
  x <- c(inp$align(climate), inp$static, inp$offsets)
  
  x <- x[[all.vars(delete.response(terms(model)))]]
  
  # Calculate extrapolation leverage
  lev <- leverage_ratio(model, x, ranges[[dataset]])
  
  writeRaster(
    lev,
    sub("\\.tif$", "_leverage_ratio.tif", file),
    overwrite = TRUE
  )
  
  # Mask open water
  if (mask_open_water && "water" %in% names(x)) {
    
    w <- ranges[[dataset]]$max[
      ranges[[dataset]]$variable == "water"
    ]
    
    x <- terra::mask(
      x,
      terra::ifel(x[["water"]] > w, 1, NA),
      inverse = TRUE
    )
  }
  
  # Clamp predictors to training ranges
  if (extrapolation == "clamp") {
    
    cl <- clamp_to_training(x, ranges[[dataset]])
    x <- cl$x
    
    if (save_outside) {
      writeRaster(
        cl$n_outside,
        sub("\\.tif$", "_n_outside_training.tif", file),
        overwrite = TRUE
      )
    }
  }
  
  # Predict abundance
  p <- terra::predict(
    x,
    model,
    fun = \(model, data, ...) {
      predict(
        model,
        newdata = data,
        type = "response",
        ...
      )
    },
    na.rm = TRUE
  )
  
  names(p) <- "prediction"
  
  writeRaster(
    p,
    file,
    overwrite = TRUE
  )
  
  p
}

# Annual predictions
purrr::walk(model_names, \(m) {
  
  predict_map(
    models$annual[[m]],
    inp$annual_climate,
    file.path(
      paths$predictions,
      paste0("annual_", m, ".tif")
    ),
    "annual",
    TRUE
  )
})

# Monthly predictions and annual means
purrr::walk(model_names, \(m) {
  
  # Predict each month
  r <- purrr::map(seq_along(month.abb), \(i) {
    
    climate <- inp$monthly_climate[[paste0(climate_vars, "_", month.abb[i])]]
    
    names(climate) <- climate_vars
    
    predict_map(
      models$monthly[[m]],
      climate,
      file.path(
        paths$predictions,
        sprintf("monthly_%s_%02d.tif", m, i)
      ),
      "monthly"
    )
  }) |>
    terra::rast()
  
  names(r) <- month.abb
  
  # Save monthly predictions
  writeRaster(
    r,
    file.path(
      paths$predictions,
      paste0("monthly_", m, "_all_months.tif")
    ),
    overwrite = TRUE
  )
  
  # Calculate annual mean abundance
  ann <- terra::app(r, mean, na.rm = TRUE)
  
  names(ann) <- "prediction"
  
  f <- file.path(
    paths$predictions,
    paste0("annual_from_monthly_", m, ".tif")
  )
  
  writeRaster(
    ann,
    f,
    overwrite = TRUE
  )
  
  # Calculate maximum monthly leverage
  lev_files <- file.path(
    paths$predictions,
    sprintf("monthly_%s_%02d_leverage_ratio.tif", m, 1:12)
  )
  
  lev <- terra::rast(lev_files) |>
    terra::app(fun = max, na.rm = TRUE)
  
  names(lev) <- "leverage_ratio"
  
  writeRaster(
    lev,
    sub("\\.tif$", "_leverage_ratio.tif", f),
    overwrite = TRUE
  )
})


