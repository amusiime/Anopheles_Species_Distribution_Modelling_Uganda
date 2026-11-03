
# 04_fit_models.R
# Fit annual and monthly negative-binomial vector abundance models

source(here::here("R", "config.R"))

# 1. Model specifications -

specs <- list(
  M1 = list(x = climate_vars, offset = "log_effort"),
  M2 = list(x = env_predictors, offset = "log_effort"),
  M3 = list(x = c(env_predictors, "itn_use"), offset = "log_effort"),
  M4 = list(x = c(env_predictors, "itn_use"), offset = "combined_offset")
)

predictors <- specs |>
  purrr::map("x") |>
  unlist() |>
  unique()

# 2. Prepare modelling datasets -

needed <- c(predictors, "log_effort", "log_continental",
            "an_gambiae_total")

datasets <- list(
  annual = read_csv(paths$model_annual, show_col_types = FALSE),
  monthly = read_csv(paths$model_monthly, show_col_types = FALSE)
) |>
  purrr::map(\(d) {
    d |>
      mutate(combined_offset = log_effort + log_continental) |>
      filter(
        if_all(all_of(needed), is.finite),
        effort > 0,
        an_gambiae_total >= 0
      )
  })

# 3. Fit negative-binomial models 

models <- purrr::map(datasets, \(d) {
  purrr::map(specs, \(s) {
    formula <- reformulate(
      c(
        purrr::map_chr(s$x, \(v) {
          sprintf("I((%s - %.12g) / %.12g)",
                  v, mean(d[[v]]), sd(d[[v]]))
        }),
        paste0("offset(", s$offset, ")")
      ),
      response = "an_gambiae_total"
    )
    
    MASS::glm.nb(
      formula,
      data = d,
      control = glm.control(maxit = 200)
    )
  })
})

# 4. Save fitted models 

dir.create(paths$models, recursive = TRUE, showWarnings = FALSE)

saveRDS(models$annual, paths$models_annual)
saveRDS(models$monthly, paths$models_monthly)

# 5. Predictor scaling statistics 

scaling <- purrr::imap_dfr(datasets, \(d, dataset) {
  tibble(
    dataset,
    variable = predictors,
    mean = purrr::map_dbl(predictors, \(v) mean(d[[v]])),
    sd = purrr::map_dbl(predictors, \(v) sd(d[[v]])),
    min = purrr::map_dbl(predictors, \(v) min(d[[v]])),
    p01 = purrr::map_dbl(predictors, \(v) quantile(d[[v]], 0.01)),
    p99 = purrr::map_dbl(predictors, \(v) quantile(d[[v]], 0.99)),
    max = purrr::map_dbl(predictors, \(v) max(d[[v]])),
    prop_zero = purrr::map_dbl(predictors, \(v) mean(d[[v]] == 0))
  )
})

write_csv(scaling, file.path(paths$models, "model_scaling.csv"))

# 6. Model performance --

metrics <- purrr::imap_dfr(models, \(ms, dataset) {
  purrr::imap_dfr(ms, \(m, model) {
    tibble(
      dataset,
      model,
      n = nobs(m),
      AIC = AIC(m),
      theta = m$theta,
      converged = m$converged,
      training_RMSE = sqrt(mean((m$y - fitted(m))^2)),
      training_MAE = mean(abs(m$y - fitted(m)))
    )
  })
})

write_csv(metrics, file.path(paths$models, "model_metrics.csv"))




