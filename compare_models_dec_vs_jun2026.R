library(here)
library(brms)
library(loo)

setwd(paste(here::here(),"/June_2026_workflow",sep="",collapse=""))

# --- Load the two model objects ---------------------------------------------
m_dec <- readRDS("deviation_model_bernoulli_dec.RDS")        # "prior" / earlier model
m_jun <- readRDS("deviation_model_bernoulli_Jun_2026.RDS")   # current model
m_dec
m_jun
# --- Primary comparison: LOO (Pareto-smoothed leave-one-out CV) -------------
# This is the standard model-comparison metric for brms/Stan models and is
# directly comparable to AIC on the deviance scale (lower looic = better fit),
# while also accounting for posterior uncertainty.

loo_dec <- loo(m_dec)
loo_jun <- loo(m_jun)

print(loo_dec)
print(loo_jun)

# elpd_diff > 0 favors the second model in the comparison (jun here);
# se_diff tells you whether the difference is meaningfully larger than noise
# (rule of thumb: |elpd_diff| > 2 * se_diff suggests a real difference)
loo_comparison <- loo_compare(loo_dec, loo_jun)
print(loo_comparison)

# --- Secondary comparison: WAIC ----------------------------------------------
waic_dec <- waic(m_dec)
waic_jun <- waic(m_jun)

print(waic_dec)
print(waic_jun)

waic_comparison <- loo_compare(waic_dec, waic_jun)
print(waic_comparison)

# --- Optional: approximate AIC / BIC for reporting purposes ------------------
# Requires the `performance` package (part of the easystats ecosystem).
# install.packages("performance") if not already installed.
if (requireNamespace("performance", quietly = TRUE)) {
  library(performance)

  perf_dec <- performance::model_performance(m_dec)
  perf_jun <- performance::model_performance(m_jun)

  cat("\n--- Dec model performance ---\n")
  print(perf_dec)

  cat("\n--- Jun 2026 model performance ---\n")
  print(perf_jun)

  cat("\n--- Side-by-side comparison ---\n")
  print(performance::compare_performance(m_dec, m_jun))
} else {
  message("Package 'performance' not installed; skipping AIC/BIC-style summary. ",
          "Install with install.packages('performance') if you want it.")
}

# --- Save results for reference ----------------------------------------------
results <- list(
  loo_dec = loo_dec,
  loo_jun = loo_jun,
  loo_comparison = loo_comparison,
  waic_dec = waic_dec,
  waic_jun = waic_jun,
  waic_comparison = waic_comparison
)

saveRDS(results, "model_comparison_dec_vs_jun2026.RDS")
