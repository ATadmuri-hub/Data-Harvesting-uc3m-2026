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
library(vars)
library(tseries)

# Re-attach dplyr::select after MASS (loaded by vars) masks it
select <- dplyr::select

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
               "imm_volume_norm", "n_immigration", "unemp_rate_national")
desc_labels <- c("PSOE (% voting intention)", "PP (% voting intention)",
                 "Vox (% voting intention)", "Gov. immigration statements (count/month)",
                 "El País immigration volume (0–100 norm.)",
                 "El País immigration articles (raw count)",
                 "National unemployment rate (%)")

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
         n_statements, imm_volume_norm) |>
  rename(`PSOE %` = psoe_avg,
         `PP %` = pp_avg,
         `Vox %` = vox_avg,
         `Gov. statements` = n_statements,
         `Imm. media volume` = imm_volume_norm)

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

cat("\n=== Time series diagnostics ===\n")

psoe_ts <- ts(model_data$psoe_avg,
              start = c(2023, 11), frequency = 12)

# ADF stationarity test (H0: unit root = non-stationary)
adf_result <- tryCatch(
  adf.test(psoe_ts, alternative = "stationary"),
  error = function(e) { cat("  ADF test failed:", e$message, "\n"); NULL }
)
if (!is.null(adf_result)) {
  cat(sprintf("  ADF test: Dickey-Fuller = %.3f, p-value = %.4f\n",
              adf_result$statistic, adf_result$p.value))
  cat(sprintf("  Conclusion: PSOE series is %s at 5%% level\n",
              if (adf_result$p.value < 0.05) "STATIONARY" else "NON-STATIONARY (unit root)"))
}

# PP test (Phillips-Perron — more robust to serial correlation)
pp_result <- tryCatch(
  pp.test(psoe_ts),
  error = function(e) NULL
)
if (!is.null(pp_result)) {
  cat(sprintf("  PP test:  Dickey-Fuller Z(alpha) = %.3f, p-value = %.4f\n",
              pp_result$statistic, pp_result$p.value))
}

cat("\n=== ARIMA model with immigration regressor ===\n")

imm_xreg <- model_data$imm_volume_norm

# Auto-select ARIMA order — exhaustive search (N=28 is small enough)
# stepwise=FALSE, approximation=FALSE ensures all (p,d,q) combinations are evaluated
arima_fit <- tryCatch({
  auto.arima(psoe_ts, xreg = imm_xreg,
             stepwise = FALSE, approximation = FALSE,
             max.p = 3, max.q = 3, max.d = 2,
             trace = TRUE)
}, error = function(e) {
  cat("  ARIMA with regressor failed, fitting without xreg\n")
  auto.arima(psoe_ts, stepwise = FALSE, approximation = FALSE)
})

cat("\n  ARIMA model selected (single xreg: salience):\n")
print(arima_fit)
cat("\n  AIC:", AIC(arima_fit), "| AICc:", arima_fit$aicc, "\n")

# Residual diagnostics for initial ARIMA
cat("\n--- ARIMA residual diagnostics ---\n")
arima_resid <- residuals(arima_fit)
lb_test <- Box.test(arima_resid, lag = min(10, length(arima_resid) - 1),
                    type = "Ljung-Box")
cat(sprintf("  Ljung-Box test: Q = %.3f, df = %d, p-value = %.4f\n",
            lb_test$statistic, lb_test$parameter, lb_test$p.value))
cat(sprintf("  Conclusion: residuals are %s at 5%% level\n",
            if (lb_test$p.value > 0.05) "WHITE NOISE (good)" else "AUTOCORRELATED (model may be underfit)"))
cat(sprintf("  Residual SD: %.3f pp\n", sd(arima_resid, na.rm = TRUE)))

# --- 6a-ii. ARIMA with multiple external regressors -------------------------

cat("\n=== Dynamic regression: ARIMA errors + salience + statements ===\n")

imm_xreg_multi <- cbind(
  salience   = model_data$imm_volume_norm,
  statements = model_data$n_statements
)

arima_multi <- tryCatch({
  auto.arima(psoe_ts, xreg = imm_xreg_multi,
             stepwise = FALSE, approximation = FALSE,
             max.p = 3, max.q = 3, max.d = 2, trace = FALSE)
}, error = function(e) {
  cat("  Multi-regressor ARIMA failed:", e$message, "\n")
  NULL
})

if (!is.null(arima_multi)) {
  cat("  Multi-xreg ARIMA selected:\n")
  print(arima_multi)
  cat(sprintf("\n  AICc comparison:  single-xreg = %.2f  |  multi-xreg = %.2f\n",
              arima_fit$aicc, arima_multi$aicc))
  # Use multi-xreg model if it has better AICc
  if (arima_multi$aicc < arima_fit$aicc) {
    cat("  >>> Multi-xreg model is better — using for forecasts\n")
    arima_fit  <- arima_multi
    imm_xreg   <- imm_xreg_multi
    use_multi_xreg <- TRUE
  } else {
    cat("  >>> Single-xreg model is better — keeping salience-only\n")
    use_multi_xreg <- FALSE
  }
} else {
  use_multi_xreg <- FALSE
}

# --- 6b. ETS model (exponential smoothing — alternative to ARIMA) -----------

cat("\n=== ETS model (exponential smoothing state space) ===\n")

ets_fit <- ets(psoe_ts)
cat("  ETS model selected:", ets_fit$method, "\n")
cat("  AICc:", ets_fit$aicc, "\n")

# Compare ARIMA vs ETS by AICc
cat("\n--- ARIMA vs ETS comparison ---\n")
cat(sprintf("  ARIMA %-20s  AICc = %.2f\n", paste0("(", arima_fit), arima_fit$aicc))
cat(sprintf("  ETS   %-20s  AICc = %.2f\n", ets_fit$method, ets_fit$aicc))
best_ts <- if (arima_fit$aicc <= ets_fit$aicc) "ARIMA" else "ETS"
cat(sprintf("  >>> Best univariate model by AICc: %s\n", best_ts))
# Note: ETS cannot use external regressors, so ARIMA remains the primary
# scenario-based forecasting model. ETS serves as a benchmark.

# Forecast horizon (shared by VAR and ARIMA forecast sections)
n_ahead <- 16L  # months: Mar 2026 – Jun 2027

# --- 6c. VAR model (PSOE + PP as bivariate system) -------------------------

cat("\n=== VAR model: PSOE + PP jointly ===\n")
cat("  Captures dynamic interdependence between government and opposition polling\n")

var_data <- model_data |> select(psoe_avg, pp_avg) |> as.data.frame()
var_ts   <- ts(var_data, start = c(2023, 11), frequency = 12)

# Select optimal lag order (max 4 given N=28)
var_select <- VARselect(var_ts, lag.max = 4, type = "const")
cat("\n  Information criteria by lag order:\n")
print(round(var_select$criteria, 2))

optimal_lag <- as.integer(which.min(var_select$criteria["AIC(n)", ]))
optimal_lag <- max(1L, min(optimal_lag, 3L))  # safety clamp for small N
cat(sprintf("  Optimal lag by AIC: %d\n", optimal_lag))

var_fit <- VAR(var_ts, p = optimal_lag, type = "const")

# VAR forecast to June 2027
var_fc      <- predict(var_fit, n.ahead = n_ahead, ci = 0.95)
var_psoe_fc <- var_fc$fcst$psoe_avg
var_pp_fc   <- var_fc$fcst$pp_avg

cat("\n  VAR forecast (June 2027):\n")
cat(sprintf("    PSOE: %.1f%% [95%% CI: %.1f – %.1f]\n",
            var_psoe_fc[n_ahead, "fcst"],
            var_psoe_fc[n_ahead, "lower"],
            var_psoe_fc[n_ahead, "upper"]))
cat(sprintf("    PP:   %.1f%% [95%% CI: %.1f – %.1f]\n",
            var_pp_fc[n_ahead, "fcst"],
            var_pp_fc[n_ahead, "lower"],
            var_pp_fc[n_ahead, "upper"]))

# Export VAR forecast for use in D'Hondt script
var_forecast_export <- tibble(
  year_month   = seq(as.Date("2026-03-01"), by = "month", length.out = n_ahead),
  psoe_var     = var_psoe_fc[, "fcst"],
  psoe_var_lo  = var_psoe_fc[, "lower"],
  psoe_var_hi  = var_psoe_fc[, "upper"],
  pp_var       = var_pp_fc[, "fcst"],
  pp_var_lo    = var_pp_fc[, "lower"],
  pp_var_hi    = var_pp_fc[, "upper"]
)
write_csv(var_forecast_export, "data/processed/var_forecast_2027.csv")
cat("  Saved VAR forecast to data/processed/var_forecast_2027.csv\n")

# Estimate PP-PSOE correlation from data (used in D'Hondt Monte Carlo)
pp_psoe_corr <- cor(model_data$psoe_avg, model_data$pp_avg, use = "complete.obs")
cat(sprintf("\n  Empirical PP-PSOE correlation: %.3f\n", pp_psoe_corr))
# Export for use in script 07
write_csv(tibble(pp_psoe_correlation = pp_psoe_corr),
          "data/processed/pp_psoe_correlation.csv")

# --- 7. 2027 Electoral Forecast ----------------------------------------------

cat("\n=== 2027 Electoral Forecast ===\n")
cat("  Projecting PSOE voting intention to June 2027\n")
cat("  (16 months beyond Feb 2026; next election expected mid-2027)\n\n")

# Scenario assumptions for immigration salience (normalised 0–100)
# Based on 2026 regularization and expected political dynamics:
#   - Neutral: salience stays near 2025 average (~70)
#   - High:    regularization debate drives salience up (~90 = near max)
#   - Low:     regularization fades from news (~50)

# Average statements/month in the data (for multi-xreg scenarios)
avg_stmts <- mean(model_data$n_statements, na.rm = TRUE)

# Build scenario xreg — adapts to whether multi-xreg was selected
build_xreg <- function(salience_path) {
  if (use_multi_xreg) {
    cbind(salience = salience_path, statements = rep(avg_stmts, length(salience_path)))
  } else {
    salience_path
  }
}

# Time-varying salience paths (more realistic than constant values)
# Regularization announced Jan 2026 → debate peaks → gradual decay
# Months: Mar 2026 (1) through Jun 2027 (16)
salience_decay <- function(peak, floor, halflife_months = 6) {
  t_seq <- seq_len(n_ahead) - 1  # 0 to 15
  floor + (peak - floor) * exp(-log(2) / halflife_months * t_seq)
}

scenarios <- list(
  # Neutral: starts at 75 (slightly above 2025 avg), decays to 60
  Neutral       = salience_decay(peak = 75, floor = 60, halflife_months = 8),
  # High: regularization debate keeps salience elevated, slow decay from 95
  `High salience (regularization debate)` = salience_decay(peak = 95, floor = 70, halflife_months = 10),
  # Low: issue fades quickly from news after initial spike
  `Low salience (issue fades)` = salience_decay(peak = 65, floor = 40, halflife_months = 4)
)

cat("  Scenario salience paths (months 1, 6, 12, 16):\n")
for (nm in names(scenarios)) {
  s <- scenarios[[nm]]
  cat(sprintf("    %-45s: %.0f → %.0f → %.0f → %.0f\n",
              nm, s[1], s[6], s[12], s[16]))
}

forecast_list <- map2_dfr(scenarios, names(scenarios), function(xreg_new, scenario) {
  fc <- tryCatch(
    forecast(arima_fit, h = n_ahead, xreg = build_xreg(xreg_new)),
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

# Export ARIMA prediction SEs for D'Hondt Monte Carlo (Improvement 5)
# Derive SE from the 95% CI width: SE = (upper_95 - lower_95) / (2 * 1.96)
neutral_fc <- forecast_list |> filter(scenario == "Neutral")
arima_pred_se <- neutral_fc |>
  mutate(
    pred_se = (upper_95 - lower_95) / (2 * 1.96)
  ) |>
  select(year_month, psoe_forecast, pred_se)

# June 2027 SE (the one used for D'Hondt)
jun2027_se <- arima_pred_se |> filter(year_month == as.Date("2027-06-01"))
cat(sprintf("  PSOE prediction SE at Jun 2027: %.3f pp\n", jun2027_se$pred_se))

write_csv(arima_pred_se, "data/processed/arima_prediction_se.csv")
cat("  Saved prediction SEs to data/processed/arima_prediction_se.csv\n")

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

# --- 9. Forecast Accuracy Metrics -------------------------------------------
# One-step-ahead rolling forecast evaluation: hold out last 6 months,
# re-fit model on the expanding window, compute prediction error each step.

cat("\n=== Forecast accuracy metrics (rolling one-step-ahead) ===\n")

n_total  <- length(psoe_ts)
n_holdout <- min(6L, n_total - 10L)   # hold out up to 6 months (need ≥10 for fitting)

if (n_holdout >= 3) {
  osa_errors_arima <- numeric(n_holdout)
  osa_errors_ets   <- numeric(n_holdout)

  for (h in seq_len(n_holdout)) {
    train_end <- n_total - n_holdout + h - 1
    y_train   <- window(psoe_ts, end = time(psoe_ts)[train_end])
    y_actual  <- as.numeric(psoe_ts[train_end + 1])

    # ARIMA with xreg
    xreg_train <- if (use_multi_xreg) imm_xreg_multi[1:train_end, , drop = FALSE] else imm_xreg[1:train_end]
    xreg_test  <- if (use_multi_xreg) imm_xreg_multi[train_end + 1, , drop = FALSE] else imm_xreg[train_end + 1]

    fit_a <- tryCatch(
      auto.arima(y_train, xreg = xreg_train, stepwise = TRUE, approximation = TRUE),
      error = function(e) auto.arima(y_train, stepwise = TRUE)
    )
    fc_a <- tryCatch(
      forecast(fit_a, h = 1, xreg = xreg_test),
      error = function(e) forecast(fit_a, h = 1)
    )
    osa_errors_arima[h] <- y_actual - as.numeric(fc_a$mean)

    # ETS
    fit_e <- ets(y_train)
    fc_e  <- forecast(fit_e, h = 1)
    osa_errors_ets[h]  <- y_actual - as.numeric(fc_e$mean)
  }

  # Compute metrics
  calc_metrics <- function(errors, label) {
    tibble(
      model = label,
      RMSE  = round(sqrt(mean(errors^2)), 3),
      MAE   = round(mean(abs(errors)), 3),
      ME    = round(mean(errors), 3),
      MAPE  = round(mean(abs(errors) / abs(as.numeric(psoe_ts[(n_total - n_holdout + 1):n_total]))) * 100, 2)
    )
  }

  accuracy_table <- bind_rows(
    calc_metrics(osa_errors_arima, "ARIMA (dynamic regression)"),
    calc_metrics(osa_errors_ets,   "ETS (exponential smoothing)")
  )

  # Add in-sample residual metrics for comparison
  arima_resid_all <- residuals(arima_fit)
  ets_resid_all   <- residuals(ets_fit)
  accuracy_table <- bind_rows(
    accuracy_table,
    tibble(model = "ARIMA (in-sample residuals)",
           RMSE = round(sqrt(mean(arima_resid_all^2, na.rm = TRUE)), 3),
           MAE  = round(mean(abs(arima_resid_all), na.rm = TRUE), 3),
           ME   = round(mean(arima_resid_all, na.rm = TRUE), 3),
           MAPE = round(mean(abs(arima_resid_all) / abs(as.numeric(psoe_ts)), na.rm = TRUE) * 100, 2)),
    tibble(model = "ETS (in-sample residuals)",
           RMSE = round(sqrt(mean(ets_resid_all^2, na.rm = TRUE)), 3),
           MAE  = round(mean(abs(ets_resid_all), na.rm = TRUE), 3),
           ME   = round(mean(ets_resid_all, na.rm = TRUE), 3),
           MAPE = round(mean(abs(ets_resid_all) / abs(as.numeric(psoe_ts)), na.rm = TRUE) * 100, 2))
  )

  cat("\n  Forecast accuracy comparison:\n")
  print(accuracy_table, width = 100)

  write_csv(accuracy_table, "data/processed/forecast_accuracy.csv")
  cat("  Saved to data/processed/forecast_accuracy.csv\n")

} else {
  cat("  Insufficient data for rolling evaluation (need ≥13 observations)\n")
}

# --- 10. Diagnostics summary export ----------------------------------------

diagnostics_summary <- tibble(
  test = c("ADF stationarity", "Phillips-Perron", "Ljung-Box residuals",
           "ARIMA order", "Best univariate (AICc)", "Multi-xreg selected",
           "VAR lag order", "PP-PSOE correlation", "Residual SD (pp)"),
  result = c(
    if (!is.null(adf_result)) sprintf("DF=%.3f, p=%.4f", adf_result$statistic, adf_result$p.value) else "N/A",
    if (!is.null(pp_result))  sprintf("Z=%.3f, p=%.4f", pp_result$statistic, pp_result$p.value) else "N/A",
    sprintf("Q=%.3f, p=%.4f", lb_test$statistic, lb_test$p.value),
    paste0(arima_fit),
    best_ts,
    as.character(use_multi_xreg),
    as.character(optimal_lag),
    sprintf("%.3f", pp_psoe_corr),
    sprintf("%.3f", sd(arima_resid, na.rm = TRUE))
  )
)
write_csv(diagnostics_summary, "data/processed/diagnostics_summary.csv")
cat("  Saved diagnostics summary to data/processed/diagnostics_summary.csv\n")

cat("\n=== Analysis complete ===\n")
cat("  Outputs in data/processed/:\n")
cat("    master_panel.csv, descriptive_stats.csv, correlation_matrix.csv\n")
cat("    crosscorr_statements_psoe.csv, model_results.csv\n")
cat("    forecast_2027.csv, var_forecast_2027.csv\n")
cat("    arima_prediction_se.csv, pp_psoe_correlation.csv\n")
cat("    forecast_accuracy.csv, diagnostics_summary.csv\n")
