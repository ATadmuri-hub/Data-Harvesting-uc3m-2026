# =============================================================================
# 06_analysis_model.R
# Statistical Analysis & 2027 Electoral Forecast
#
# Analyses:
#   1. Merge all processed datasets into a master panel
#   2. Descriptive statistics table
#   3. Pearson correlation matrix (immigration intensity × polling)
#   4. Cross-correlation: do government statements LEAD polling changes?
#   5. OLS regression: PSOE ~ time + immigration_salience + n_statements
#   6. Heteroskedasticity-robust standard errors (Newey-West)
#   7. ARIMA(p,d,q) time-series model for PSOE with immigration regressor
#   8. 2027 electoral forecast — three scenarios around the 2026 regularization
#
# Outputs (data/processed/):
#   master_panel.csv       — merged monthly panel (all variables)
#   model_results.csv      — OLS coefficient table
#   forecast_2027.csv      — monthly point forecast + 95% CI to June 2027
#   correlation_matrix.csv — pairwise correlations
# =============================================================================

library(tidyverse)
library(lubridate)
library(broom)
library(lmtest)
library(sandwich)
library(forecast)
library(zoo)
library(scales)

# --- 1. Build master panel ---------------------------------------------------

cat("=== Building master panel ===\n")

polls   <- read_csv("data/processed/monthly_polls.csv",      show_col_types = FALSE)
stmts   <- read_csv("data/processed/monthly_statements.csv", show_col_types = FALSE)
salience <- read_csv("data/processed/salience_scores.csv",   show_col_types = FALSE)

# Annual unemployment by province — will average across provinces for national figure
unemp_annual <- read_csv("data/raw/ine_unemployment_annual.csv",
                         show_col_types = FALSE)

# Monthly panel
# Compute national unemployment average across all provinces
unemp_national <- unemp_annual |>
  group_by(year) |>
  summarise(unemp_rate_national = mean(unemployment_rate, na.rm = TRUE),
            .groups = "drop")

# Normalise immigration article count (salience has n_immigration raw)
salience <- salience |>
  mutate(
    imm_volume_norm = round(n_immigration / max(n_immigration, na.rm = TRUE) * 100, 1)
  )

panel <- polls |>
  left_join(stmts,   by = "year_month") |>
  left_join(salience |> select(year_month, n_immigration, imm_volume_norm,
                                salience_score),
            by = "year_month") |>
  mutate(
    year    = year(year_month),
    n_statements    = replace_na(n_statements, 0L),
    n_immigration   = replace_na(n_immigration, 0L),
    imm_volume_norm = replace_na(imm_volume_norm, 0),
    # Time index (1 = Nov 2023)
    t = as.integer(row_number()),
    # Lead/lag variables
    psoe_lag1  = lag(psoe_avg, 1),
    psoe_chg   = psoe_avg - lag(psoe_avg, 1),
    stmt_lag1  = lag(n_statements, 1),
    stmt_lag2  = lag(n_statements, 2),
    salience_lag1 = lag(imm_volume_norm, 1)
  ) |>
  left_join(unemp_national |> select(year, unemp_rate_national), by = "year")

write_csv(panel, "data/processed/master_panel.csv")
cat("  Master panel:", nrow(panel), "months,", ncol(panel), "variables\n")
cat("  Period:", as.character(min(panel$year_month)),
    "–", as.character(max(panel$year_month)), "\n")

# --- 2. Descriptive statistics -----------------------------------------------

cat("\n=== Descriptive statistics ===\n")

desc_vars <- c("psoe_avg", "pp_avg", "vox_avg", "n_statements",
               "imm_volume_norm", "n_immigration")
desc_labels <- c("PSOE (% voting intention)", "PP (% voting intention)",
                 "Vox (% voting intention)", "Gov. immigration statements (count/month)",
                 "El País immigration volume (0–100 norm.)",
                 "El País immigration articles (raw count)")

desc_stats <- map2_dfr(desc_vars, desc_labels, function(v, lbl) {
  x <- panel[[v]]
  tibble(
    Variable  = lbl,
    N         = sum(!is.na(x)),
    Mean      = round(mean(x, na.rm = TRUE), 2),
    SD        = round(sd(x, na.rm = TRUE), 2),
    Min       = round(min(x, na.rm = TRUE), 2),
    Median    = round(median(x, na.rm = TRUE), 2),
    Max       = round(max(x, na.rm = TRUE), 2)
  )
})

print(desc_stats, width = 120)
write_csv(desc_stats, "data/processed/descriptive_stats.csv")

# --- 3. Correlation matrix ---------------------------------------------------

cat("\n=== Correlation matrix ===\n")

corr_vars <- panel |>
  select(psoe_avg, pp_avg, vox_avg,
         n_statements, imm_volume_norm, n_immigration) |>
  rename(`PSOE %` = psoe_avg,
         `PP %` = pp_avg,
         `Vox %` = vox_avg,
         `Gov. statements` = n_statements,
         `Imm. media volume` = imm_volume_norm,
         `Imm. articles (raw)` = n_immigration)

corr_mat  <- cor(corr_vars, use = "complete.obs")
corr_long <- as.data.frame(corr_mat) |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  mutate(r = round(r, 3))

print(round(corr_mat, 3))
write_csv(corr_long, "data/processed/correlation_matrix.csv")

# --- 4. Cross-correlation: government statements → PSOE change ---------------

cat("\n=== Cross-correlation analysis ===\n")
cat("  Do immigration statements PRECEDE PSOE polling changes?\n")

# Filter to complete cases
cc_data <- panel |> filter(!is.na(psoe_chg), !is.na(n_statements))

if (nrow(cc_data) >= 10) {
  cc_result <- ccf(cc_data$n_statements, cc_data$psoe_chg,
                   lag.max = 6, plot = FALSE)
  ccf_df <- tibble(
    lag         = cc_result$lag[,,1],
    correlation = cc_result$acf[,,1]
  ) |>
    arrange(lag)  # Ensure ascending lag order for correct plotting
  cat("  Cross-correlations (statement count vs. PSOE monthly change):\n")
  print(ccf_df |> arrange(desc(abs(correlation))), n = 10)
  write_csv(ccf_df, "data/processed/crosscorr_statements_psoe.csv")

  # --- Granger causality test --------------------------------------------------
  # H0: immigration statements do NOT Granger-cause PSOE polling changes
  cat("\n=== Granger Causality Tests (order = 2) ===\n")
  granger_stmts <- tryCatch(
    grangertest(psoe_chg ~ n_statements, data = cc_data, order = 2),
    error = function(e) { cat("  Granger test (statements) failed:", e$message, "\n"); NULL }
  )
  granger_salience <- tryCatch(
    grangertest(psoe_chg ~ imm_volume_norm,
                data = panel |> filter(!is.na(psoe_chg), !is.na(imm_volume_norm)),
                order = 2),
    error = function(e) { cat("  Granger test (salience) failed:", e$message, "\n"); NULL }
  )
  if (!is.null(granger_stmts)) {
    cat("  H0: n_statements does NOT Granger-cause psoe_chg\n")
    print(granger_stmts)
  }
  if (!is.null(granger_salience)) {
    cat("  H0: imm_volume_norm does NOT Granger-cause psoe_chg\n")
    print(granger_salience)
  }
  # Save Granger results
  granger_df <- bind_rows(
    if (!is.null(granger_stmts))  as.data.frame(granger_stmts)  |> mutate(predictor = "n_statements",    model = c("Restricted","Unrestricted")),
    if (!is.null(granger_salience)) as.data.frame(granger_salience) |> mutate(predictor = "imm_volume_norm", model = c("Restricted","Unrestricted"))
  )
  if (nrow(granger_df) > 0) write_csv(granger_df, "data/processed/granger_test.csv")

} else {
  cat("  Insufficient data for CCF\n")
}

# --- 5. OLS regression -------------------------------------------------------

cat("\n=== OLS Regression: PSOE % ~ trend + immigration intensity ===\n")

model_data <- panel |>
  filter(!is.na(psoe_avg), !is.na(n_statements), !is.na(imm_volume_norm))

# Model 1: trend only
m1 <- lm(psoe_avg ~ t, data = model_data)

# Model 2: trend + government statements
m2 <- lm(psoe_avg ~ t + n_statements, data = model_data)

# Model 3: trend + statements + media salience
m3 <- lm(psoe_avg ~ t + n_statements + imm_volume_norm, data = model_data)

# Model 4: trend + statements (lag-1) + salience (lag-1)  — lagged predictors
m4 <- lm(psoe_avg ~ t + stmt_lag1 + salience_lag1, data = model_data)

# Newey-West HAC robust SEs for autocorrelation in residuals
nw_se <- function(model, lag = 3) {
  coeftest(model, vcov = NeweyWest(model, lag = lag, prewhite = FALSE))
}

cat("\n--- Model 3 (main specification): PSOE ~ trend + statements + salience ---\n")
print(nw_se(m3))

cat("\n--- Model fit summary ---\n")
model_summary <- bind_rows(
  glance(m1) |> mutate(model = "M1: Trend only"),
  glance(m2) |> mutate(model = "M2: Trend + Statements"),
  glance(m3) |> mutate(model = "M3: Trend + Statements + Salience"),
  glance(m4) |> mutate(model = "M4: Lagged predictors")
) |>
  select(model, r.squared, adj.r.squared, AIC, BIC, nobs) |>
  mutate(across(where(is.numeric), ~ round(., 3)))

print(model_summary)

# Save model comparison summary
model_summary_out <- bind_rows(
  glance(m1) |> mutate(model = "M1: Trend only"),
  glance(m2) |> mutate(model = "M2: Trend + Gov. statements"),
  glance(m3) |> mutate(model = "M3: Trend + Statements + Salience (main)"),
  glance(m4) |> mutate(model = "M4: Lagged predictors")
) |>
  select(model, r.squared, adj.r.squared, AIC, BIC, nobs) |>
  mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(model_summary_out, "data/processed/model_comparison.csv")

# Save coefficient table (main model = m3)
coef_table <- tidy(m3) |>
  bind_cols(confint(m3) |> as_tibble(.name_repair = "minimal") |>
              setNames(c("conf.low", "conf.high"))) |>
  mutate(
    # Newey-West p-values
    nw_p = nw_se(m3)[, "Pr(>|t|)"],
    sig  = case_when(nw_p < 0.01 ~ "***", nw_p < 0.05 ~ "**",
                     nw_p < 0.10 ~ "*",  TRUE ~ ""),
    across(where(is.double), ~ round(., 4))
  ) |>
  rename(Variable = term, Estimate = estimate, SE = std.error,
         t = statistic, p = p.value)

write_csv(coef_table, "data/processed/model_results.csv")
cat("\n  Coefficient table saved.\n")

# --- 6. ARIMA model with external regressors ---------------------------------

cat("\n=== ARIMA model with immigration regressor ===\n")

psoe_ts <- ts(model_data$psoe_avg,
              start = c(2023, 11), frequency = 12)

imm_xreg <- model_data$imm_volume_norm

# Auto-select ARIMA order
arima_fit <- tryCatch({
  auto.arima(psoe_ts, xreg = imm_xreg,
             stepwise = TRUE, approximation = TRUE, trace = FALSE)
}, error = function(e) {
  cat("  ARIMA with regressor failed, fitting without xreg\n")
  auto.arima(psoe_ts, stepwise = TRUE, approximation = TRUE)
})

cat("\n  ARIMA model selected:\n")
print(arima_fit)
cat("\n  AIC:", AIC(arima_fit), "| AICc:", arima_fit$aicc, "\n")

# --- 7. 2027 Electoral Forecast ----------------------------------------------

cat("\n=== 2027 Electoral Forecast ===\n")
cat("  Projecting PSOE voting intention to June 2027\n")
cat("  (16 months beyond Feb 2026; next election expected mid-2027)\n\n")

# Scenario assumptions for immigration salience (normalised 0–100)
# Based on 2026 regularization and expected political dynamics:
#   - Neutral: salience stays near 2025 average (~70)
#   - High:    regularization debate drives salience up (~90 = near max)
#   - Low:     regularization fades from news (~50)

n_ahead <- 16  # months: Mar 2026 – Jun 2027

scenarios <- list(
  Neutral       = rep(70, n_ahead),
  `High salience (regularization debate)`  = rep(90, n_ahead),
  `Low salience (issue fades)` = rep(50, n_ahead)
)

forecast_list <- map2_dfr(scenarios, names(scenarios), function(xreg_new, scenario) {
  fc <- tryCatch(
    forecast(arima_fit, h = n_ahead, xreg = xreg_new),
    error = function(e) forecast(arima_fit, h = n_ahead)
  )

  tibble(
    year_month  = seq(as.Date("2026-03-01"), by = "month",
                      length.out = n_ahead),
    scenario    = scenario,
    psoe_forecast = as.numeric(fc$mean),
    lower_80    = as.numeric(fc$lower[, "80%"]),
    upper_80    = as.numeric(fc$upper[, "80%"]),
    lower_95    = as.numeric(fc$lower[, "95%"]),
    upper_95    = as.numeric(fc$upper[, "95%"])
  )
})

# Clamp to plausible polling range [15, 45]
# (ARIMA extrapolation can produce physically implausible values beyond the observed range)
pre_clamp <- forecast_list
forecast_list <- forecast_list |>
  mutate(across(c(psoe_forecast, lower_80, upper_80, lower_95, upper_95),
                ~ pmax(15, pmin(45, .))))
n_clamped <- sum(rowSums(pre_clamp[,3:7] != forecast_list[,3:7]) > 0, na.rm = TRUE)
if (n_clamped > 0) cat("  NOTE:", n_clamped, "forecast rows had values clamped to [15, 45]\n")

cat("  Forecast summary (June 2027 by scenario):\n")
print(
  forecast_list |>
    filter(year_month == as.Date("2027-06-01")) |>
    select(scenario, psoe_forecast, lower_95, upper_95) |>
    mutate(across(where(is.numeric), ~ round(., 1)))
)

write_csv(forecast_list, "data/processed/forecast_2027.csv")
cat("\n  Saved forecast to data/processed/forecast_2027.csv\n")

# --- 8. Regularization impact note -------------------------------------------

cat("\n=== 2026 Regularization: Direct electoral impact assessment ===\n")
cat("
  KEY FINDING: The January 2026 regularization will NOT directly add
  voters to the 2027 Spanish General Election electorate.

  Legal basis:
  - Spanish General Election voting requires Spanish citizenship.
  - Standard naturalization requires 10 years of legal residence.
  - Exception: Ibero-American, Portuguese, and Filipino nationals (2 years).
  - A residence permit granted in 2026 does not meet the 2-year timeline
    for special-category nationals by the expected June 2027 election.

  Municipal elections (May 2027):
  - EU and reciprocity-agreement nationals with 3–5 years of legal status
    may qualify for local voting; this is a small subset of the 500,000.

  Indirect electoral effects (modelled as salience scenarios above):
  - PSOE base mobilization: regularization fulfils a 700,000-signature ILP;
    may boost turnout among left-leaning and pro-migrant voters.
  - PP/Vox opposition: 'efecto llamada' framing may mobilise right-wing
    voters; PP has announced a Supreme Court challenge.
  - Net centre/swing voter effect: genuinely uncertain; immigration polls
    as a top-3 concern for Spanish voters (CIS 2024–2025).
\n")

cat("=== Analysis complete ===\n")
cat("  Outputs in data/processed/:\n")
cat("    master_panel.csv, descriptive_stats.csv, correlation_matrix.csv\n")
cat("    crosscorr_statements_psoe.csv, model_results.csv\n")
cat("    forecast_2027.csv\n")
