# ============================================================
# 04_diagnostics.R
# DIAGNOSTICS FOR ALL THREE MODELS
# UGANDA VECTOR ABUNDANCE MODELLING
# ============================================================

source("functions/helpers.R")

library(tidyverse)
library(glmmTMB)
library(DHARMa)

# ------------------------------------------------------------
# 1. INPUTS AND OUTPUTS
# ------------------------------------------------------------

mods <- readRDS(
  "outputs/models/vector_abundance_models.rds"
)

d <- readRDS(
  "outputs/models/modeling_dataset.rds"
)

dir.create(
  "outputs/diagnostics",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. CHECK REQUIRED VARIABLES
# ------------------------------------------------------------

check_required(
  d,
  c(
    "observation_id",
    "site_id",
    "site_month_id",
    "event_date",
    "longitude",
    "latitude",
    "an_gambiae_total"
  )
)

# ------------------------------------------------------------
# 3. DHARMA DIAGNOSTIC FUNCTION
# ------------------------------------------------------------

run_diag <- function(model, model_name, data) {
  
  sim <- simulateResiduals(
    fittedModel = model,
    n = 500,
    plot = FALSE
  )
  
  uniformity <- testUniformity(sim)
  dispersion <- testDispersion(sim)
  zero_inflation <- testZeroInflation(sim)
  
  spatial <- tryCatch(
    testSpatialAutocorrelation(
      sim,
      x = data$longitude,
      y = data$latitude
    ),
    error = function(e) e
  )
  
  temporal <- tryCatch(
    testTemporalAutocorrelation(
      sim,
      time = data$event_date
    ),
    error = function(e) e
  )
  
  tibble(
    model = model_name,
    
    uniformity_p =
      uniformity$p.value,
    
    dispersion_p =
      dispersion$p.value,
    
    zero_inflation_p =
      zero_inflation$p.value,
    
    spatial_autocorrelation_p =
      if (inherits(spatial, "error"))
        NA_real_
    else
      spatial$p.value,
    
    temporal_autocorrelation_p =
      if (inherits(temporal, "error"))
        NA_real_
    else
      temporal$p.value,
    
    spatial_message =
      if (inherits(spatial, "error"))
        spatial$message
    else
      "",
    
    temporal_message =
      if (inherits(temporal, "error"))
        temporal$message
    else
      ""
  )
}

# ------------------------------------------------------------
# 4. RUN DHARMA DIAGNOSTICS
# ------------------------------------------------------------
# All three models were fitted to the same complete M3
# modelling dataset, allowing direct comparison.

diag_summary <- purrr::imap_dfr(
  mods,
  ~ run_diag(
    model = .x,
    model_name = .y,
    data = d
  )
)

print(diag_summary)

write_csv(
  diag_summary,
  "outputs/tables/DHARMa_summary.csv"
)

# ------------------------------------------------------------
# 5. SAVE DHARMA PLOTS
# ------------------------------------------------------------

purrr::iwalk(
  mods,
  function(model, model_name) {
    
    sim <- simulateResiduals(
      fittedModel = model,
      n = 500,
      plot = FALSE
    )
    
    png(
      filename =
        paste0(
          "outputs/figures/DHARMa_",
          model_name,
          ".png"
        ),
      width = 1800,
      height = 1400,
      res = 180
    )
    
    plot(sim)
    
    dev.off()
  }
)

# ------------------------------------------------------------
# 6. RESIDUAL TABLES
# ------------------------------------------------------------
# Predictions are marginal/population-level predictions.
# Random site effects are excluded using re.form = NA.

residual_tables <- purrr::imap_dfr(
  mods,
  function(model, model_name) {
    
    predicted <- predict(
      model,
      type = "response",
      re.form = NA
    )
    
    d |>
      transmute(
        model = model_name,
        observation_id,
        site_id,
        site_month_id,
        event_date,
        longitude,
        latitude,
        an_gambiae_total,
        predicted = predicted,
        residual = an_gambiae_total - predicted
      )
  }
)

write_csv(
  residual_tables,
  "outputs/tables/model_residuals_all.csv"
)

# ------------------------------------------------------------
# 7. SITE-LEVEL RESIDUAL SUMMARY
# ------------------------------------------------------------

site_res <- residual_tables |>
  group_by(
    model,
    site_id
  ) |>
  summarise(
    n = n(),
    
    mean_residual =
      mean(
        residual,
        na.rm = TRUE
      ),
    
    rmse =
      sqrt(
        mean(
          residual^2,
          na.rm = TRUE
        )
      ),
    
    .groups = "drop"
  )

write_csv(
  site_res,
  "outputs/tables/site_residual_summary.csv"
)

# ------------------------------------------------------------
# 8. OVERALL DIAGNOSTIC MESSAGE
# ------------------------------------------------------------

message(
  "Diagnostics completed for M1, M2 and M3. ",
  "Review DHARMa uniformity, dispersion, zero inflation, ",
  "spatial autocorrelation and temporal autocorrelation ",
  "before final model interpretation and mapping."
)

# ============================================================
# END OF SCRIPT
# ============================================================
