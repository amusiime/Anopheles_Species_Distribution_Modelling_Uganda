# 04_fit_models.R
# Negative binomial abundance models: An. gambiae

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
})

# Load modelling data
annual_data <- read_csv(
  "data/processed/model_data_environment.csv",
  show_col_types = FALSE
)

monthly_data <- read_csv(
  "data/processed/model_data_monthly_environment.csv",
  show_col_types = FALSE
)

# Prepare complete observations
annual_data <- combined_data %>%
  mutate(log_effort = log(effort)) %>%
  drop_na(
    an_gambiae_total,
    bio6_min_temp,
    bio5_max_temp,
    bio12_precip
  )

monthly_data <- monthly_data %>%
  mutate(log_effort = log(effort)) %>%
  drop_na(an_gambiae_total, tmin, tmax, precip, travel, log_effort)

# Fit models
# Model 1: Environmental predictors only
M1 <- MASS::glm.nb(
  an_gambiae_total ~
    bio6_min_temp +
    bio5_max_temp +
    bio12_precip +
    travel+
    offset(log_effort),
  data = combined_data,
  control = glm.control(maxit = 100)
)



# M2: Monthly environmental predictors + travel
M2 <- MASS::glm.nb(
  an_gambiae_total ~
    tmin +
    tmax +
    precip +
    travel +
    offset(log_effort),
  data = monthly_data,
  control = glm.control(maxit = 100)
)


# Summarise model fit
model_comparison <- tibble(
  model = c("M1_Combined", "M2_Monthly"),
  observations = c(nobs(M1), nobs(M2)),
  AIC = c(AIC(M1), AIC(M2)),
  BIC = c(BIC(M1), BIC(M2))
)

# Save models and results
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  list(M1 = M1, M2 = M2),
  "outputs/models/abundance_models.rds"
)

write_csv(model_comparison, "outputs/tables/model_comparison.csv")









