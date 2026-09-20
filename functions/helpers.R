# Reusable helper functions

parse_gps <- function(x) {
  m <- stringr::str_match(
    as.character(x),
    "\\[?\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*\\]?"
  )
  tibble::tibble(
    longitude_gps = suppressWarnings(as.numeric(m[, 2])),
    latitude_gps  = suppressWarnings(as.numeric(m[, 3]))
  )
}

row_total_preserve_na <- function(data, vars) {
  x <- as.data.frame(data[vars])
  out <- rowSums(x, na.rm = TRUE)
  all_missing <- rowSums(!is.na(x)) == 0
  out[all_missing] <- NA_real_
  out
}

metric_fun <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]
  pred <- pred[ok]
  tibble::tibble(
    RMSE = if (length(obs)) sqrt(mean((obs - pred)^2)) else NA_real_,
    MAE  = if (length(obs)) mean(abs(obs - pred)) else NA_real_,
    R2   = if (length(obs) > 1) cor(obs, pred)^2 else NA_real_
  )
}

make_numeric_raster <- function(template, value, name) {
  r <- template
  terra::values(r) <- value
  names(r) <- name
  r
}

make_factor_raster <- function(template, variable, data) {
  x <- droplevels(factor(data[[variable]]))
  if (!length(x) || all(is.na(x))) stop("No non-missing values for factor: ", variable)
  levs <- levels(x)
  modal <- names(sort(table(x), decreasing = TRUE))[1]
  r <- template
  r[] <- match(modal, levs)
  r <- terra::as.factor(r)
  terra::levels(r) <- data.frame(ID = seq_along(levs), category = levs)
  names(r) <- variable
  r
}

check_required <- function(data, vars) {
  miss <- setdiff(vars, names(data))
  if (length(miss)) stop("Missing variables: ", paste(miss, collapse = ", "))
}
