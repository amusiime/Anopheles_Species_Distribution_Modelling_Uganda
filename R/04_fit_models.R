

# 04_fit_abundance_models.R

library(tidyverse)
library(MASS)

# 1. Load modelling data
model_data <- read_csv(
  "data/processed/model_data_observations.csv",
  show_col_types = FALSE
)

# 2. Prepare data

model_data <- model_data %>%
  mutate(log_effort = log(effort)) |> 
  drop_na(
    an_gambiae_total,
    tmin,
    tmax,
    precip,
    travel,
    log_continental_offset,
    log_effort)
    
# 3. Fit Negative Binomial models
# M1: Environmental and accessibility predictors
M1 <- MASS::glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    offset(log_effort),
  data = model_data,

)

# M2: Environmental predictors + continental offset
M2 <- MASS::glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    offset(log_continental_offset),
  data = model_data,
)



