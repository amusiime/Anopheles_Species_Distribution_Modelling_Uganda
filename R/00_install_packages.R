# ============================================================
# 00_install_packages.R
# Install/check packages used by the Uganda vector abundance workflow
# ============================================================

packages <- c(
  "tidyverse", "lubridate", "janitor", "stringr", "terra",
  "geodata", "MASS", "DHARMa", "patchwork", "viridis"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing) > 0) {
  install.packages(missing)
}

message("Package check complete.")
