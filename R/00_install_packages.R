packages <- c(
  "tidyverse", "lubridate", "janitor", "stringr", "terra", "sf",
  "geodata", "glmmTMB", "DHARMa", "performance", "patchwork", "scales"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)
if (length(missing)) install.packages(missing)

message("Package check complete.")
