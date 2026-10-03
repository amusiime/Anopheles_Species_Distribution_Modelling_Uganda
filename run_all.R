# run_all.R: Uganda vector abundance modelling pipeline

scripts <- c(
  "R/01_clean_data.R",
  #"R/01b_data_QA.R",
  "R/02_prepare_spatial_covariates.R",
  "R/03_prepare_model_data.R",
  "R/04_fit_models.R",
  "R/05_diagnostics.R",
  "R/06_spatial_prediction.R",
  "R/07_final_prediction_map.R"
)

# Check that all scripts exist
stopifnot(all(file.exists(scripts)))

# Run pipeline
for (script in scripts) {
  cat("\nRunning:", script, "\n")
  source(script)
}

cat("\nPipeline completed successfully.\n")
