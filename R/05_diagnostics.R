# 05_diagnostics.R

library(DHARMa)
library(tidyverse)

files <- c(
  annual_M1 = "outputs/models/annual_M1.rds",
  annual_M2 = "outputs/models/annual_M2.rds",
  monthly_M1 = "outputs/models/monthly_M1.rds",
  monthly_M2 = "outputs/models/monthly_M2.rds"
)

mods <- lapply(files, readRDS)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

diagnostics <- purrr::imap_dfr(mods, function(m, name) {
  sim <- simulateResiduals(m, n = 500)
  
  png(paste0("outputs/figures/DHARMa_", name, ".png"),
      width = 1600, height = 1200, res = 160)
  plot(sim)
  dev.off()
  
  tibble(
    model = name, n = nobs(m),
    uniformity_p = testUniformity(sim)$p.value,
    dispersion_p = testDispersion(sim)$p.value,
    zero_inflation_p = testZeroInflation(sim)$p.value
  )
})

residuals <- purrr::imap_dfr(mods, function(m, name) {
  observed <- model.response(model.frame(m))
  predicted <- predict(m, type = "response")
  
  tibble(model = name, residual = observed - predicted)
}) |>
  group_by(model) |>
  summarise(
    n = n(),
    RMSE = sqrt(mean(residual^2, na.rm = TRUE)),
    mean_residual = mean(residual, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(diagnostics, "outputs/tables/DHARMa_summary.csv")
write_csv(residuals, "outputs/tables/residual_summary.csv")