# ============================================================
# run_all.R
# Run the core Uganda vector abundance modelling workflow


scripts <- c(
  "R/00_install_packages.R",
  "R/01_clean_data.R",
  "R/02_prepare_covariates.R",
  "R/03_prepare_model_data.R",
  "R/04_fit_models.R",
  "R/05_diagnostics.R",
  "R/06_spatial_prediction.R",
  "R/07_final_prediction_map.R"
)

for (script in scripts) {
  if (!file.exists(script)) stop("Script not found: ", script)
  source(script, local = FALSE)
}


