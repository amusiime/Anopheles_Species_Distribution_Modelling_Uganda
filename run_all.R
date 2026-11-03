# run_all.R

# Run the complete workflow from the project root

library(here)

scripts <- c(
  "00_install_packages.R",
  "01_clean_data.R",
  "02_prepare_covariates.R",
  "02b_prepare_itn_raster.R",
  "03_prepare_model_data.R",
  "04_fit_models.R",
  "05_diagnostics.R",
  "06_spatial_prediction.R",
  "07_final_prediction_map.R"
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  source(
    here::here("R", s),
    local = new.env(parent = globalenv())
  )
}

dir.create(here::here("outputs"), recursive = TRUE, showWarnings = FALSE)

writeLines(
  capture.output(sessionInfo()),
  here::here("outputs", "session_info.txt")
)

