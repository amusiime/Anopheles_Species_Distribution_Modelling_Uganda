# ============================================================
# Reusable helper functions
# ============================================================

# Extract GPS coordinates
parse_gps <- function(x) {
  m <- stringr::str_match(as.character(x),
                          "\\[?\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)")
  tibble::tibble(
    longitude_gps = as.numeric(m[, 2]),
    latitude_gps  = as.numeric(m[, 3])
  )
}

# Calculate row totals, preserving all-NA rows
row_total_preserve_na <- function(data, vars) {
  x <- data[vars]
  total <- rowSums(x, na.rm = TRUE)
  total[rowSums(!is.na(x)) == 0] <- NA
  total
}

# Calculate model performance
metric_fun <- function(obs, pred) {
  ok <- !is.na(obs) & !is.na(pred)
  obs <- obs[ok]
  pred <- pred[ok]
  
  tibble::tibble(
    RMSE = if (length(obs)) sqrt(mean((obs - pred)^2)) else NA,
    MAE  = if (length(obs)) mean(abs(obs - pred)) else NA,
    R2   = if (length(obs) > 1) cor(obs, pred)^2 else NA
  )
}

# Create numeric raster
make_numeric_raster <- function(template, value, name) {
  r <- template
  terra::values(r) <- value
  names(r) <- name
  r
}

# Create factor raster using the modal category
make_factor_raster <- function(template, variable, data) {
  x <- droplevels(factor(data[[variable]]))
  if (!length(x) || all(is.na(x))) stop("No valid values: ", variable)
  
  levs <- levels(x)
  modal <- names(which.max(table(x)))
  
  r <- template
  r[] <- match(modal, levs)
  r <- terra::as.factor(r)
  terra::levels(r) <- data.frame(ID = seq_along(levs), category = levs)
  names(r) <- variable
  r
}

# Check required columns
check_required <- function(data, vars) {
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop("Missing: ", paste(missing, collapse = ", "))
}