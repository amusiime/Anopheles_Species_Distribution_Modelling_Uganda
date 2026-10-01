
# ============================================================
# R/04_fit_abundance_models.R
# Uganda Mosquito Abundance Modelling
# ============================================================

library(tidyverse)
library(MASS)

# 1. LOAD MODELLING DATA

cell_totals <- read_csv(
  "data/processed/model_data_cell_totals.csv",
  show_col_types = FALSE
)

# 2. PREPARE MODELLING DATA

model_data <- cell_totals %>%
  filter(
    is.finite(an_gambiae_total),
    an_gambiae_total >= 0,
    is.finite(tmin),
    is.finite(tmax),
    is.finite(precip),
    is.finite(travel),
    is.finite(log_continental_offset),
    n_cell_months > 0
  )

# 3. FIT NEGATIVE BINOMIAL MODELS

# Model 1: Environmental predictors and travel accessibility

M1 <- glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    offset(log(n_cell_months)),
  data = model_data
)

# Model 2: Model 1 plus continental abundance offset

M2 <- glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    offset(log(n_cell_months)) +
    offset(log_continental_offset),
  data = model_data
)

# 4. MODEL SUMMARIES AND COMPARISON

summary(M1)
summary(M2)

AIC(M1, M2)

# 5. SAVE MODELS

saveRDS(
  list(M1 = M1, M2 = M2),
  "outputs/models/local_environmental_models.rds"
)

# 6. SAVE MODEL METRICS

model_metrics <- tibble(
  model = c("M1", "M2"),
  observations = c(nobs(M1), nobs(M2)),
  AIC = c(AIC(M1), AIC(M2)),
  logLik = c(
    as.numeric(logLik(M1)),
    as.numeric(logLik(M2))
  ),
  theta = c(M1$theta, M2$theta)
)

write_csv(
  model_metrics,
  "data/processed/model_metrics.csv"
)

# 7. FINAL OUTPUTS

cat("\nModel fitting completed.\n")

cat("\nNumber of grid cells:", nrow(model_data), "\n")

cat("\nSampling effort:\n")
print(summary(model_data$n_cell_months))

cat("\nModel metrics:\n")
print(model_metrics)

