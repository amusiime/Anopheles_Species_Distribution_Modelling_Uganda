library(tidyverse)
library(DHARMa)

# Load models
mods <- list(
  annual_M1  = readRDS("outputs/models/annual_M1.rds"),
  annual_M2  = readRDS("outputs/models/annual_M2.rds"),
  monthly_M1 = readRDS("outputs/models/monthly_M1.rds"),
  monthly_M2 = readRDS("outputs/models/monthly_M2.rds")
)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Diagnostics and plots
diagnostics <- purrr::imap_dfr(mods, function(m, name) {
  sim <- simulateResiduals(m, n = 500)
  
  png(paste0("outputs/figures/DHARMa_", name, ".png"))
  plot(sim)
  dev.off()
  
  tibble(
    model = name,
    uniformity_p = testUniformity(sim)$p.value,
    dispersion_p = testDispersion(sim)$p.value,
    zero_inflation_p = testZeroInflation(sim)$p.value
  )
})

write_csv(diagnostics, "outputs/tables/DHARMa_summary.csv")

# Residual summaries
purrr::imap_dfr(mods, function(m, name) {
  observed <- model.response(model.frame(m))
  predicted <- predict(m, type = "response")
  
  tibble(
    model = name,
    observed = observed,
    predicted = predicted,
    residual = observed - predicted
  )
}) |>
  group_by(model) |>
  summarise(
    n = n(),
    RMSE = sqrt(mean(residual^2)),
    mean_residual = mean(residual),
    .groups = "drop"
  ) |>
  write_csv("outputs/tables/residual_summary.csv")

message("Diagnostics completed.")