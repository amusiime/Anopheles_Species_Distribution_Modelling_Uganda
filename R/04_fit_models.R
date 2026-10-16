
# 04_fit_models.R
# Fit annual and monthly negative-binomial abundance models

library(tidyverse)
library(MASS)

# Load datasets ---------------------------------------------------------------
annual <- readr::read_csv(
  "data/processed/model_data_annual.csv", show_col_types = FALSE
)

monthly <- readr::read_csv(
  "data/processed/model_data_monthly.csv", show_col_types = FALSE
)

# Fit models ------------------------------------------------------------------
fit_models <- function(data) {
  list(
    M1 = MASS::glm.nb(
      an_gambiae_total ~ tmin + tmax + precip + offset(log_effort),
      data = data
    ),
    M2 = MASS::glm.nb(
      an_gambiae_total ~ tmin + tmax + precip +
        offset(log_effort) + offset(log_continental),
      data = data
    )
  )
}

annual_models <- fit_models(annual)
monthly_models <- fit_models(monthly)

# Save models -----------------------------------------------------------------
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

saveRDS(annual_models$M1, "outputs/models/annual_M1.rds")
saveRDS(annual_models$M2, "outputs/models/annual_M2.rds")
saveRDS(monthly_models$M1, "outputs/models/monthly_M1.rds")
saveRDS(monthly_models$M2, "outputs/models/monthly_M2.rds")


