library(DHARMa)
library(performance)

# Simulate residuals
sim_M1 <- simulateResiduals(M1, n = 1000)
sim_M2 <- simulateResiduals(M2, n = 1000)

# Diagnostic plots
plot(sim_M1, main = "M1: Combined Environmental Model")
plot(sim_M2, main = "M2: Monthly Environmental Model")

# Diagnostic tests
diagnose_model <- function(model, residuals) {
  print(summary(model))
  print(AIC(model))
  print(testUniformity(residuals))
  print(testDispersion(residuals))
  print(testZeroInflation(residuals))
  print(check_collinearity(model))
}

diagnose_model(M1, sim_M1)
diagnose_model(M2, sim_M2)

# Save diagnostics
dir.create("outputs/diagnostics", recursive = TRUE,
           showWarnings = FALSE)

saveRDS(
  list(M1 = sim_M1, M2 = sim_M2),
  "outputs/diagnostics/model_diagnostics.rds"
)


