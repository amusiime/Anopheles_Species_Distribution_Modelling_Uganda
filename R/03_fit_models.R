# ============================================================
# 03_fit_models.R
# THREE NEGATIVE-BINOMIAL VECTOR ABUNDANCE MODELS - UGANDA
# ============================================================

source("functions/helpers.R")

library(tidyverse)
library(glmmTMB)

# ------------------------------------------------------------
# 1. INPUTS AND OUTPUTS
# ------------------------------------------------------------

input <- "data/processed/model_data_environment.csv"

dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. READ AND PREPARE DATA
# ------------------------------------------------------------

d <- read_csv(input, show_col_types = FALSE) |>
  mutate(
    month = factor(
      month,
      levels = month.abb,
      ordered = TRUE
    ),
    across(
      c(
        house_type,
        sprayed,
        site_id,
        household_id,
        site_year_id,
        site_month_id
      ),
      factor
    )
  )

required_vars <- c(
  "an_gambiae_total",
  "people_under_net",
  "n_nets",
  "sprayed",
  "house_type",
  "month",
  "travel",
  "log_effort",
  "year_c",
  "tmax",
  "tmin",
  "precip",
  "site_id"
)

check_required(d, required_vars)

# ------------------------------------------------------------
# 3. COMMON MODELLING DATASET
# ------------------------------------------------------------
# All three models use the same complete-case observations.
# This makes model comparisons consistent.

d_model <- d |>
  filter(
    an_gambiae_total >= 0,
    complete.cases(across(all_of(required_vars)))
  )

if (!nrow(d_model)) {
  stop(
    "No complete modelling records. Check covariates, ",
    "environmental data, and travel raster."
  )
}

# ------------------------------------------------------------
# 4. VALIDATE M3 CONTINENTAL AN. GAMBIAE OFFSET
# ------------------------------------------------------------
# M3 uses the continental An. gambiae prediction surface
# from Band 4 of uganda_preds_p.tif, prepared in
# R/02_prepare_covariates.R.

if (!"log_continental_offset" %in% names(d_model)) {
  stop(
    "M3 requires log_continental_offset. Supply the validated ",
    "continental An. gambiae prediction raster at ",
    "data/raw/continental/uganda_preds_p.tif and rerun ",
    "R/02_prepare_covariates.R."
  )
}

d_model <- d_model |>
  filter(is.finite(log_continental_offset))

if (!nrow(d_model)) {
  stop(
    "No records have a valid continental An. gambiae ",
    "offset for M3."
  )
}

# ------------------------------------------------------------
# 5. MODEL FORMULAS
# ------------------------------------------------------------

# M1: Household + season + travel + sampling effort + year
f1 <- an_gambiae_total ~
  people_under_net +
  n_nets +
  sprayed +
  house_type +
  month +
  travel +
  log_effort +
  year_c

# M2: M1 + environmental variables + site random effect
f2 <- update(
  f1,
  . ~ . + tmax + tmin + precip + (1 | site_id)
)

# M3: M2 + continental An. gambiae ecological offset
f3 <- update(
  f2,
  . ~ . + offset(log_continental_offset)
)

# ------------------------------------------------------------
# 6. FIT MODELS
# ------------------------------------------------------------

models <- list(
  M1 = glmmTMB(
    f1,
    family = nbinom2,
    data = d_model
  ),
  
  M2 = glmmTMB(
    f2,
    family = nbinom2,
    data = d_model
  ),
  
  M3 = glmmTMB(
    f3,
    family = nbinom2,
    data = d_model
  )
)

# ------------------------------------------------------------
# 7. MODEL COMPARISON
# ------------------------------------------------------------

comparison <- imap_dfr(
  models,
  ~ tibble(
    model = .y,
    n = nobs(.x),
    AIC = AIC(.x),
    BIC = BIC(.x),
    logLik = as.numeric(logLik(.x))
  )
) |>
  arrange(AIC)

print(comparison)

write_csv(
  comparison,
  "outputs/tables/model_comparison.csv"
)

# ------------------------------------------------------------
# 8. PREDICTIONS AND PERFORMANCE
# ------------------------------------------------------------

pred <- d_model |>
  mutate(
    predicted_M1 = predict(
      models$M1,
      type = "response",
      re.form = NA
    ),
    
    predicted_M2 = predict(
      models$M2,
      type = "response",
      re.form = NA
    ),
    
    predicted_M3 = predict(
      models$M3,
      type = "response",
      re.form = NA
    )
  )

performance <- map_dfr(
  names(models),
  \(m) {
    metric_fun(
      d_model$an_gambiae_total,
      pred[[paste0("predicted_", m)]]
    ) |>
      mutate(
        model = m,
        .before = 1
      )
  }
)

print(performance)

write_csv(
  performance,
  "outputs/tables/model_performance.csv"
)

write_csv(
  pred,
  "outputs/tables/model_predictions.csv"
)

# ------------------------------------------------------------
# 9. SAVE MODELS AND DATA
# ------------------------------------------------------------

saveRDS(
  models,
  "outputs/models/vector_abundance_models.rds"
)

saveRDS(
  d_model,
  "outputs/models/modeling_dataset.rds"
)

message(
  "Models fitted successfully: ",
  "M1 baseline, M2 environmental repeated-site model, ",
  "M3 continental An. gambiae offset model."
)

# ============================================================
# END OF SCRIPT
# ============================================================