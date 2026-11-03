# 00_install_packages.R
# Install the packages used by the workflow.
# For exact reproducibility, also run renv::init() once and commit renv.lock.

packages <- c(
  "tidyverse", "here", "janitor", "terra", "sf", "geodata",
  "MASS", "DHARMa", "patchwork"
)

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

message("Package check complete.")
