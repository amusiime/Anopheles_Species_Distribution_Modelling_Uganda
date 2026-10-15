
library(tidyverse)
library(MASS)

# 1. Load prepared datasets
annual <- read_csv("data/processed/model_data_annual.csv", show_col_types = FALSE)
monthly <- read_csv("data/processed/model_data_monthly.csv", show_col_types = FALSE)

# 2. Fit annual models
annual_M1 <- glm.nb(
  an_gambiae_total ~ tmin + tmax + precip + offset(log_effort),
  data = annual
)

annual_M2 <- glm.nb(
  an_gambiae_total ~ tmin + tmax + precip +
    offset(log_effort) + offset(log_continental),
  data = annual
)

# 3. Fit monthly models
monthly_M1 <- glm.nb(
  an_gambiae_total ~ tmin + tmax + precip + offset(log_effort),
  data = monthly
)

monthly_M2 <- glm.nb(
  an_gambiae_total ~ tmin + tmax + precip +
    offset(log_effort) + offset(log_continental),
  data = monthly
)

# 4. Compare model summaries
summary(annual_M1)
summary(annual_M2)
summary(monthly_M1)
summary(monthly_M2)

# 5. Save fitted models
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

saveRDS(annual_M1, "outputs/models/annual_M1.rds")
saveRDS(annual_M2, "outputs/models/annual_M2.rds")
saveRDS(monthly_M1, "outputs/models/monthly_M1.rds")
saveRDS(monthly_M2, "outputs/models/monthly_M2.rds")


