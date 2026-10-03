# 04_fit_models.R
# Negative binomial abundance models: An. gambiae

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
})

# Load modelling data
combined_data <- read_csv(
  "data/processed/model_data_environment.csv",
  show_col_types = FALSE
)

monthly_data <- read_csv(
  "data/processed/model_data_monthly_environment.csv",
  show_col_types = FALSE
)

# Prepare complete observations
combined_data <- combined_data %>%
  mutate(log_effort = log(effort)) %>%
  drop_na(an_gambiae_total, bio1, bio5, bio12, travel, log_effort)

monthly_data <- monthly_data %>%
  mutate(log_effort = log(effort)) %>%
  drop_na(an_gambiae_total, tmin, tmax, precip, travel, log_effort)

# Fit models
M1 <- glm.nb(
  an_gambiae_total ~ bio1 + bio5 + bio12 + travel +
    offset(log_effort),
  data = combined_data
)

M2 <- glm.nb(
  an_gambiae_total ~ tmin + tmax + precip + travel +
    offset(log_effort),
  data = monthly_data
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









