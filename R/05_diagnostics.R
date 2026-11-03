
# 05_diagnostics.R
# Diagnose annual and monthly negative-binomial models

source(here::here("R", "config.R"))

# 1. Load fitted models ---------------------------------------------------

models <- list(
  annual = readRDS(paths$models_annual),
  monthly = readRDS(paths$models_monthly)
) |>
  purrr::list_flatten(name_spec = "{outer}_{inner}")

# 2. Prepare output directories -------------------------------------------

dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)

# 3. Run DHARMa diagnostics -----------------------------------------------

diagnostics <- purrr::imap_dfr(models, \(m, name) {
  sim <- DHARMa::simulateResiduals(m, n = 500, seed = 123)
  
  png(
    file.path(paths$figures, paste0("DHARMa_", name, ".png")),
    width = 1600, height = 1200, res = 160
  )
  
  plot(sim)
  dev.off()
  
  tibble(
    model = name,
    n = nobs(m),
    uniformity_p = DHARMa::testUniformity(sim, plot = FALSE)$p.value,
    dispersion_p = DHARMa::testDispersion(sim, plot = FALSE)$p.value,
    zero_inflation_p = DHARMa::testZeroInflation(sim, plot = FALSE)$p.value
  )
})

# 4. Save and display results ---------------------------------------------

write_csv(diagnostics, file.path(paths$tables, "DHARMa_summary.csv"))

print(diagnostics)
