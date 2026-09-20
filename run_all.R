# ============================================================
# run_all.R
# COMPLETE VECTOR ABUNDANCE MODELLING PIPELINE - UGANDA
# ============================================================

cat(
  "\n============================================================",
  "\nUGANDA VECTOR ABUNDANCE MODELLING PIPELINE",
  "\n============================================================",
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 1. CHECK WORKING DIRECTORY
# ------------------------------------------------------------

cat("\nWorking directory:\n")
print(getwd())

# ------------------------------------------------------------
# 2. DEFINE PIPELINE SCRIPTS
# ------------------------------------------------------------

scripts <- c(
  "R/01_clean_data.R",
  "R/02_prepare_covariates.R",
  "R/03_fit_models.R",
  "R/04_diagnostics.R",
  "R/05_spatial_prediction.R"
)

# ------------------------------------------------------------
# 3. CHECK THAT ALL SCRIPTS EXIST
# ------------------------------------------------------------

missing_scripts <- scripts[
  !file.exists(scripts)
]

if (length(missing_scripts) > 0) {
  
  stop(
    "\nThe following pipeline scripts are missing:\n",
    paste(
      missing_scripts,
      collapse = "\n"
    ),
    "\n"
  )
  
}

cat(
  "\nAll pipeline scripts found successfully.\n"
)

# ------------------------------------------------------------
# 4. RUN PIPELINE
# ------------------------------------------------------------

for (script in scripts) {
  
  cat(
    "\n\n============================================================",
    "\nRUNNING: ",
    script,
    "\n============================================================\n",
    sep = ""
  )
  
  start_time <- Sys.time()
  
  tryCatch(
    
    {
      
      source(
        script,
        echo = TRUE
      )
      
      end_time <- Sys.time()
      
      cat(
        "\n------------------------------------------------------------",
        "\nCOMPLETED: ",
        script,
        "\nTime: ",
        round(
          as.numeric(
            difftime(
              end_time,
              start_time,
              units = "mins"
            )
          ),
          2
        ),
        " minutes",
        "\n------------------------------------------------------------\n",
        sep = ""
      )
      
    },
    
    error = function(e) {
      
      cat(
        "\n\n============================================================",
        "\nPIPELINE FAILED",
        "\n============================================================",
        "\nFailed script: ",
        script,
        "\nError message:",
        "\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )
      
      stop(
        "\nPipeline stopped because the script above failed."
      )
      
    }
    
  )
  
}

# ------------------------------------------------------------
# 5. FINAL MESSAGE
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nPIPELINE COMPLETED SUCCESSFULLY",
  "\n============================================================",
  "\n",
  "\nCompleted scripts:",
  "\n  01_clean_data.R",
  "\n  02_prepare_covariates.R",
  "\n  03_fit_models.R",
  "\n  04_diagnostics.R",
  "\n  05_spatial_prediction.R",
  "\n",
  "\nMain outputs:",
  "\n  Clean data:",
  "\n    data/processed/ento_clean.csv",
  "\n",
  "\n  Modelling data:",
  "\n    data/processed/model_data_environment.csv",
  "\n",
  "\n  Models:",
  "\n    outputs/models/vector_abundance_models.rds",
  "\n",
  "\n  Diagnostics:",
  "\n    outputs/tables/DHARMa_summary.csv",
  "\n",
  "\n  Predictions:",
  "\n    outputs/predictions/an_gambiae_M1_1km.tif",
  "\n    outputs/predictions/an_gambiae_M2_1km.tif",
  "\n    outputs/predictions/an_gambiae_M3_1km.tif",
  "\n",
  "\n============================================================",
  "\n",
  sep = ""
)

# ============================================================
# END OF run_all.R
# ============================================================
