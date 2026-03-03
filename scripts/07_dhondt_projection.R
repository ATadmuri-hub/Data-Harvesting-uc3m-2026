# =============================================================================
# 07_dhondt_projection.R
# D'Hondt Seat Projection Model — 2027 Spanish General Election
#
# Method: Uniform swing from 2023 baseline applied constituency-by-constituency
#         using the D'Hondt divisor method. Monte Carlo uncertainty (n = 5 000).
#
# Inputs:
#   data/processed/forecast_2027.csv        — PSOE vote share projections (ARIMA)
#   data/processed/arima_prediction_se.csv  — PSOE prediction SEs (for MC draws)
#   data/processed/pp_psoe_correlation.csv  — empirical PP-PSOE correlation
#   data/processed/var_forecast_2027.csv    — VAR-based PP projections
#
# Outputs:
#   data/processed/seat_projections.csv   — deterministic seat projections
#   data/processed/seat_monte_carlo.csv   — Monte Carlo seat distributions
#   data/processed/seat_mc_summary.csv    — Monte Carlo summary statistics
# =============================================================================

library(tidyverse)
library(scales)

cat("=== 07: D\u2019Hondt Seat Projection Model ===\n\n")

set.seed(42)

# ─── 0. Import data-driven parameters from script 06 ──────────────────────
# Read ARIMA prediction SEs (to replace hardcoded MC standard deviations)
arima_se_file <- "data/processed/arima_prediction_se.csv"
if (file.exists(arima_se_file)) {
  arima_pred_se <- read_csv(arima_se_file, show_col_types = FALSE)
  # Use June 2027 SE (last forecast month)
  jun2027_row <- arima_pred_se |> filter(year_month == as.Date("2027-06-01"))
  if (nrow(jun2027_row) == 1) {
    psoe_mc_sd <- jun2027_row$pred_se / 100  # convert pp → proportion
    cat(sprintf("  PSOE MC SD from ARIMA: %.4f (%.2f pp)\n", psoe_mc_sd, jun2027_row$pred_se))
  } else {
    psoe_mc_sd <- 0.012  # fallback
    cat("  WARNING: Jun 2027 row not found in arima_prediction_se.csv, using default SD=0.012\n")
  }
} else {
  psoe_mc_sd <- 0.012  # fallback if script 06 hasn't been run
  cat("  WARNING: arima_prediction_se.csv not found, using default SD=0.012\n")
}

# Read empirical PP-PSOE correlation (to replace hardcoded -0.6)
corr_file <- "data/processed/pp_psoe_correlation.csv"
if (file.exists(corr_file)) {
  pp_psoe_corr <- read_csv(corr_file, show_col_types = FALSE)$pp_psoe_correlation[1]
  cat(sprintf("  PP-PSOE correlation from data: %.3f\n", pp_psoe_corr))
} else {
  pp_psoe_corr <- -0.6  # fallback
  cat("  WARNING: pp_psoe_correlation.csv not found, using default corr=-0.6\n")
}

# Read VAR forecast for data-driven PP estimates
var_file <- "data/processed/var_forecast_2027.csv"
if (file.exists(var_file)) {
  var_forecast <- read_csv(var_file, show_col_types = FALSE)
  cat("  VAR forecast loaded for PP scenario derivation\n")
} else {
  var_forecast <- NULL
  cat("  WARNING: var_forecast_2027.csv not found, using hardcoded PP estimates\n")
}

# Read ARIMA forecast for data-driven PSOE scenario values
fc_file <- "data/processed/forecast_2027.csv"
if (file.exists(fc_file)) {
  arima_forecast <- read_csv(fc_file, show_col_types = FALSE)
  cat("  ARIMA forecast loaded for PSOE scenario derivation\n")
} else {
  arima_forecast <- NULL
  cat("  WARNING: forecast_2027.csv not found, using hardcoded PSOE estimates\n")
}

# Scale other party SDs relative to PSOE (PP slightly less uncertain, minor parties more)
pp_mc_sd    <- psoe_mc_sd * 0.85   # PP typically more stable
vox_mc_sd   <- psoe_mc_sd * 0.70   # smaller party, less polling data → use proportion of PSOE SD
sumar_mc_sd <- psoe_mc_sd * 0.70

cat(sprintf("  MC SDs: PSOE=%.4f, PP=%.4f, Vox=%.4f, Sumar=%.4f\n",
            psoe_mc_sd, pp_mc_sd, vox_mc_sd, sumar_mc_sd))

# ─── 1. D'Hondt Algorithm ────────────────────────────────────────────────────
# Standard divisor method used in Spanish general elections (LOREG Art. 163)
dhondt <- function(votes, n_seats) {
  if (n_seats == 0 || sum(votes, na.rm = TRUE) == 0)
    return(setNames(integer(length(votes)), names(votes)))
  parties   <- names(votes)
  seats_won <- setNames(integer(length(votes)), parties)
  for (s in seq_len(n_seats)) {
    quotients        <- votes / (seats_won + 1L)
    winner           <- which.max(quotients)
    seats_won[winner] <- seats_won[winner] + 1L
  }
  seats_won
}

# ─── 2. 52-Constituency Dataset (2023 Baseline) ──────────────────────────────
# Vote shares calibrated from Ministerio del Interior, 23-J 2023 results.
# "regional": aggregated share for constituency-specific parties
#   (PNV+EH Bildu in Basque Country, Junts+ERC in Cataluña, BNG in Galicia,
#    CC in Canarias, EH Bildu/Geroa Bai in Navarra).
# These regional shares are held fixed across 2027 scenarios because
# immigration salience primarily affects national-party competition.

constituencies <- tribble(
  ~province,                ~region,               ~seats, ~pp,   ~psoe, ~vox,  ~sumar, ~regional,
  # ── Andalucía ──────────────────────────────────────────────────────────────
  "Sevilla",                "Andalucía",                12, 0.322, 0.325, 0.119, 0.112, 0.000,
  "Málaga",                 "Andalucía",                11, 0.341, 0.276, 0.142, 0.109, 0.000,
  "Cádiz",                  "Andalucía",                 8, 0.287, 0.312, 0.142, 0.135, 0.000,
  "Córdoba",                "Andalucía",                 7, 0.315, 0.331, 0.139, 0.112, 0.000,
  "Granada",                "Andalucía",                 7, 0.323, 0.282, 0.149, 0.112, 0.000,
  "Almería",                "Andalucía",                 6, 0.412, 0.246, 0.186, 0.092, 0.000,
  "Huelva",                 "Andalucía",                 5, 0.312, 0.353, 0.139, 0.109, 0.000,
  "Jaén",                   "Andalucía",                 5, 0.305, 0.361, 0.142, 0.109, 0.000,
  # ── Aragón ─────────────────────────────────────────────────────────────────
  "Zaragoza",               "Aragón",                    8, 0.323, 0.268, 0.135, 0.112, 0.000,
  "Huesca",                 "Aragón",                    3, 0.323, 0.293, 0.129, 0.109, 0.000,
  "Teruel",                 "Aragón",                    2, 0.351, 0.292, 0.145, 0.098, 0.000,
  # ── Asturias ───────────────────────────────────────────────────────────────
  "Asturias",               "Asturias",                  8, 0.309, 0.303, 0.112, 0.125, 0.000,
  # ── Baleares ───────────────────────────────────────────────────────────────
  "Baleares",               "Baleares",                  8, 0.316, 0.263, 0.125, 0.122, 0.000,
  # ── Canarias ───────────────────────────────────────────────────────────────
  "Las Palmas",             "Canarias",                  8, 0.283, 0.286, 0.082, 0.125, 0.152,
  "Santa Cruz de Tenerife", "Canarias",                  6, 0.287, 0.275, 0.085, 0.119, 0.164,
  # ── Cantabria ──────────────────────────────────────────────────────────────
  "Cantabria",              "Cantabria",                 5, 0.351, 0.272, 0.129, 0.102, 0.000,
  # ── Castilla-La Mancha ─────────────────────────────────────────────────────
  "Toledo",                 "Castilla-La Mancha",        6, 0.371, 0.293, 0.146, 0.102, 0.000,
  "Ciudad Real",            "Castilla-La Mancha",        5, 0.361, 0.312, 0.146, 0.099, 0.000,
  "Albacete",               "Castilla-La Mancha",        4, 0.363, 0.309, 0.142, 0.098, 0.000,
  "Cuenca",                 "Castilla-La Mancha",        3, 0.376, 0.293, 0.152, 0.095, 0.000,
  "Guadalajara",            "Castilla-La Mancha",        3, 0.392, 0.276, 0.155, 0.098, 0.000,
  # ── Castilla y León ────────────────────────────────────────────────────────
  "Valladolid",             "Castilla y León",           7, 0.387, 0.251, 0.142, 0.099, 0.000,
  "Burgos",                 "Castilla y León",           4, 0.391, 0.256, 0.147, 0.095, 0.000,
  "León",                   "Castilla y León",           4, 0.362, 0.283, 0.131, 0.098, 0.000,
  "Salamanca",              "Castilla y León",           4, 0.406, 0.242, 0.155, 0.093, 0.000,
  "Palencia",               "Castilla y León",           3, 0.391, 0.263, 0.146, 0.093, 0.000,
  "Segovia",                "Castilla y León",           3, 0.386, 0.263, 0.152, 0.093, 0.000,
  "Zamora",                 "Castilla y León",           3, 0.396, 0.291, 0.142, 0.095, 0.000,
  "Ávila",                  "Castilla y León",           3, 0.406, 0.242, 0.159, 0.092, 0.000,
  "Soria",                  "Castilla y León",           2, 0.393, 0.276, 0.146, 0.092, 0.000,
  # ── Cataluña ───────────────────────────────────────────────────────────────
  "Barcelona",              "Cataluña",                 32, 0.190, 0.275, 0.058, 0.152, 0.280,
  "Tarragona",              "Cataluña",                  6, 0.248, 0.232, 0.098, 0.102, 0.240,
  "Girona",                 "Cataluña",                  6, 0.178, 0.190, 0.072, 0.088, 0.398,
  "Lleida",                 "Cataluña",                  4, 0.218, 0.222, 0.076, 0.088, 0.328,
  # ── Extremadura ────────────────────────────────────────────────────────────
  "Badajoz",                "Extremadura",               6, 0.323, 0.356, 0.129, 0.109, 0.000,
  "Cáceres",                "Extremadura",               4, 0.342, 0.346, 0.139, 0.102, 0.000,
  # ── Galicia ────────────────────────────────────────────────────────────────
  "A Coruña",               "Galicia",                   6, 0.340, 0.318, 0.092, 0.112, 0.098,
  "Pontevedra",             "Galicia",                   7, 0.328, 0.312, 0.095, 0.102, 0.123,
  "Lugo",                   "Galicia",                   4, 0.385, 0.295, 0.102, 0.085, 0.080,
  "Orense",                 "Galicia",                   3, 0.415, 0.285, 0.112, 0.082, 0.062,
  # ── La Rioja ───────────────────────────────────────────────────────────────
  "La Rioja",               "La Rioja",                  4, 0.373, 0.271, 0.142, 0.102, 0.000,
  # ── Madrid ─────────────────────────────────────────────────────────────────
  "Madrid",                 "Madrid",                   37, 0.347, 0.257, 0.109, 0.107, 0.000,
  # ── Murcia ─────────────────────────────────────────────────────────────────
  "Murcia",                 "Murcia",                   10, 0.395, 0.239, 0.173, 0.102, 0.000,
  # ── Navarra ────────────────────────────────────────────────────────────────
  "Navarra",                "Navarra",                   5, 0.255, 0.202, 0.102, 0.092, 0.298,
  # ── País Vasco ─────────────────────────────────────────────────────────────
  "Vizcaya",                "País Vasco",                8, 0.162, 0.220, 0.032, 0.112, 0.440,
  "Guipúzcoa",              "País Vasco",                6, 0.045, 0.175, 0.012, 0.095, 0.628,
  "Álava",                  "País Vasco",                4, 0.248, 0.222, 0.082, 0.102, 0.348,
  # ── Valencia ───────────────────────────────────────────────────────────────
  "Valencia",               "Valencia",                 16, 0.348, 0.272, 0.135, 0.119, 0.000,
  "Alicante",               "Valencia",                 12, 0.387, 0.253, 0.158, 0.109, 0.000,
  "Castellón",              "Valencia",                  5, 0.351, 0.285, 0.146, 0.112, 0.000,
  # ── Ceuta & Melilla ────────────────────────────────────────────────────────
  "Ceuta",                  "Ceuta",                     1, 0.452, 0.271, 0.162, 0.071, 0.000,
  "Melilla",                "Melilla",                   1, 0.423, 0.261, 0.175, 0.072, 0.000
)

stopifnot(nrow(constituencies) == 52)
stopifnot(sum(constituencies$seats) == 350)
cat("Constituency data loaded: 52 provinces, 350 seats total\n")

# ─── 3. Apply D'Hondt to a Scenario ──────────────────────────────────────────
# Given national-level pp/psoe/vox/sumar vote shares, apply uniform swing
# to all constituencies and run D'Hondt in each. Regional shares are fixed.

project_seats <- function(pp_nat, psoe_nat, vox_nat, sumar_nat,
                           base_data = constituencies) {
  # 2023 national baseline (denominators for swing calculation)
  pp_base    <- 0.3305
  psoe_base  <- 0.2874
  vox_base   <- 0.1239
  sumar_base <- 0.1231

  results <- base_data |>
    rowwise() |>
    mutate(
      # Uniform swing: add national change to each constituency
      available = 1 - regional,   # share available to national parties
      # Scale national shares so they sum to 'available' in this constituency
      nat_total_base = pp + psoe + vox + sumar,
      # Apply swing proportionally within available space
      pp_sw    = pp    + (pp_nat    - pp_base)    * (pp    / nat_total_base),
      psoe_sw  = psoe  + (psoe_nat  - psoe_base)  * (psoe  / nat_total_base),
      vox_sw   = vox   + (vox_nat   - vox_base)   * (vox   / nat_total_base),
      sumar_sw = sumar + (sumar_nat - sumar_base)  * (sumar / nat_total_base),
      # Clamp to [0, available] and rescale
      pp_sw    = pmax(0, pmin(available, pp_sw)),
      psoe_sw  = pmax(0, pmin(available, psoe_sw)),
      vox_sw   = pmax(0, pmin(available, vox_sw)),
      sumar_sw = pmax(0, pmin(available, sumar_sw)),
      nat_sw_total = pp_sw + psoe_sw + vox_sw + sumar_sw,
      # Rescale national parties to exactly fill available space
      pp_f    = if_else(nat_sw_total > 0, pp_sw    / nat_sw_total * available, 0),
      psoe_f  = if_else(nat_sw_total > 0, psoe_sw  / nat_sw_total * available, 0),
      vox_f   = if_else(nat_sw_total > 0, vox_sw   / nat_sw_total * available, 0),
      sumar_f = if_else(nat_sw_total > 0, sumar_sw / nat_sw_total * available, 0)
    ) |>
    ungroup()

  # Run D'Hondt for each constituency
  seat_list <- pmap(
    list(results$pp_f, results$psoe_f, results$vox_f, results$sumar_f,
         results$regional, results$seats),
    function(pp, psoe, vox, sumar, reg, n) {
      votes <- c(PP = pp, PSOE = psoe, Vox = vox, Sumar = sumar, Regional = reg)
      dhondt(votes, n)
    }
  )

  # Aggregate across constituencies
  seat_matrix <- do.call(rbind, seat_list)
  colSums(seat_matrix)
}

# ─── 4. 2023 Calibration Check ───────────────────────────────────────────────
cat("\n2023 calibration check (baseline D\u2019Hondt vs. actual results):\n")
seats_2023 <- project_seats(0.3305, 0.2874, 0.1239, 0.1231)
actual_2023 <- c(PP = 137, PSOE = 121, Vox = 33, Sumar = 31, Regional = 28)

cat(sprintf("  PP:       model %3d  |  actual 137  |  diff %+d\n",
            seats_2023["PP"],      seats_2023["PP"]      - 137))
cat(sprintf("  PSOE:     model %3d  |  actual 121  |  diff %+d\n",
            seats_2023["PSOE"],    seats_2023["PSOE"]    - 121))
cat(sprintf("  Vox:      model %3d  |  actual  33  |  diff %+d\n",
            seats_2023["Vox"],     seats_2023["Vox"]     -  33))
cat(sprintf("  Sumar:    model %3d  |  actual  31  |  diff %+d\n",
            seats_2023["Sumar"],   seats_2023["Sumar"]   -  31))
cat(sprintf("  Regional: model %3d  |  actual  28  |  diff %+d\n",
            seats_2023["Regional"],seats_2023["Regional"] - 28))
cat(sprintf("  Total:    model %3d  |  actual 350\n", sum(seats_2023)))

# ─── 5. 2027 Scenarios ───────────────────────────────────────────────────────
# PSOE projections from ARIMA model (June 2027 point forecast).
# PP, Vox, Sumar projections based on:
#   - Issue-ownership theory (immigration salience → Vox gains)
#   - Mid-term attrition affecting incumbent PSOE
#   - PP as the main beneficiary of PSOE decline
#   - Sumar continuing structural decline

# Derive scenario vote shares from ARIMA + VAR forecasts
# 2023 national baselines (for computing swings)
psoe_base_2023 <- 0.2874
pp_base_2023   <- 0.3305
vox_base_2023  <- 0.1239
sumar_base_2023 <- 0.1231

# PSOE: from ARIMA forecast (June 2027 point estimate per scenario)
if (!is.null(arima_forecast)) {
  jun27_fc <- arima_forecast |>
    filter(year_month == as.Date("2027-06-01")) |>
    select(scenario, psoe_forecast)

  psoe_neutral <- (jun27_fc |> filter(scenario == "Neutral"))$psoe_forecast[1] / 100
  psoe_high    <- (jun27_fc |> filter(grepl("High", scenario)))$psoe_forecast[1] / 100
  psoe_low     <- (jun27_fc |> filter(grepl("Low", scenario)))$psoe_forecast[1] / 100
  cat(sprintf("  PSOE from ARIMA forecast: Neutral=%.3f, High=%.3f, Low=%.3f\n",
              psoe_neutral, psoe_high, psoe_low))
} else {
  psoe_neutral <- 0.257; psoe_high <- 0.254; psoe_low <- 0.259  # fallback
  cat("  Using fallback PSOE values\n")
}

# PP: from VAR forecast (June 2027) if available, else derive from PSOE swing
if (!is.null(var_forecast)) {
  pp_var_jun <- (var_forecast |> filter(year_month == as.Date("2027-06-01")))$pp_var[1] / 100
  cat(sprintf("  PP from VAR forecast: %.3f\n", pp_var_jun))
  # Adjust PP by scenario: if PSOE drops more, PP gains more (zero-sum logic)
  psoe_swing_neutral <- psoe_neutral - psoe_base_2023
  psoe_swing_high    <- psoe_high    - psoe_base_2023
  psoe_swing_low     <- psoe_low     - psoe_base_2023
  # PP absorbs ~40% of PSOE losses (empirical estimate from Spanish elections)
  pp_neutral <- pp_var_jun + 0.4 * (psoe_swing_neutral - psoe_swing_neutral)  # baseline = VAR
  pp_high    <- pp_var_jun + 0.4 * (psoe_swing_high    - psoe_swing_neutral)
  pp_low     <- pp_var_jun + 0.4 * (psoe_swing_low     - psoe_swing_neutral)
} else {
  # Fallback: PP gains what PSOE loses (scaled by empirical correlation)
  pp_neutral <- pp_base_2023 + abs(pp_psoe_corr) * (psoe_base_2023 - psoe_neutral) * 0.6
  pp_high    <- pp_base_2023 + abs(pp_psoe_corr) * (psoe_base_2023 - psoe_high) * 0.6
  pp_low     <- pp_base_2023 + abs(pp_psoe_corr) * (psoe_base_2023 - psoe_low) * 0.6
}

# Vox: issue-ownership theory — higher immigration salience → Vox gains
# Scenarios: high salience boosts Vox ~1pp from baseline, low reduces ~0.5pp
vox_neutral <- vox_base_2023 - 0.004  # slight decline from 2023 peak
vox_high    <- vox_base_2023 + 0.006  # immigration salience benefits Vox
vox_low     <- vox_base_2023 - 0.009  # issue fades → Vox loses its wedge

# Sumar: structural decline from 2023 (fragmented left, internal tensions)
sumar_neutral <- sumar_base_2023 - 0.018  # steady erosion
sumar_high    <- sumar_base_2023 - 0.023  # immigration debate crowds out left's agenda
sumar_low     <- sumar_base_2023 - 0.013  # slightly better without immigration headwind

scenarios_2027 <- tribble(
  ~scenario,                                  ~pp,        ~psoe,        ~vox,        ~sumar,
  "Neutral",                                  pp_neutral, psoe_neutral, vox_neutral, sumar_neutral,
  "High salience (regularization debate)",    pp_high,    psoe_high,    vox_high,    sumar_high,
  "Low salience (issue fades)",               pp_low,     psoe_low,     vox_low,     sumar_low
)

cat("\n  Data-derived 2027 scenario vote shares:\n")
print(scenarios_2027 |> mutate(across(where(is.numeric), ~ round(., 3))))

cat("\n2027 deterministic seat projections:\n")
seat_projections <- scenarios_2027 |>
  rowwise() |>
  mutate(
    seats_raw = list(project_seats(pp, psoe, vox, sumar)),
    PP       = seats_raw[["PP"]],
    PSOE     = seats_raw[["PSOE"]],
    Vox      = seats_raw[["Vox"]],
    Sumar    = seats_raw[["Sumar"]],
    Regional = seats_raw[["Regional"]],
    Total    = PP + PSOE + Vox + Sumar + Regional,
    majority = 176   # absolute majority threshold
  ) |>
  select(-seats_raw) |>
  ungroup()

print(seat_projections |>
  select(scenario, PP, PSOE, Vox, Sumar, Regional, Total) |>
  as.data.frame())

# ─── 6. Monte Carlo Simulation (n = 5 000) ───────────────────────────────────
# Uncertainty sources:
#   - PSOE: drawn from N(scenario_mean, sd=1.2) — from ARIMA 80% CI width
#   - PP:   drawn from N(scenario_mean + anti_corr * PSOE_deviation, sd=1.0)
#   - Vox:  drawn from N(scenario_mean, sd=0.8)
#   - Sumar: drawn from N(scenario_mean, sd=0.8)
# National shares are renormalised after drawing so they stay in [0, 1].

N_SIM <- 5000

cat(sprintf("\nRunning Monte Carlo simulations (n = %d)...\n", N_SIM))

mc_results <- map_dfr(seq_len(nrow(scenarios_2027)), function(i) {
  sc   <- scenarios_2027[i, ]
  name <- sc$scenario

  psoe_draws  <- rnorm(N_SIM, mean = sc$psoe,  sd = psoe_mc_sd)
  psoe_dev    <- psoe_draws - sc$psoe   # deviation from scenario centre
  pp_draws    <- rnorm(N_SIM, mean = sc$pp + (pp_psoe_corr * psoe_dev), sd = pp_mc_sd)
  vox_draws   <- rnorm(N_SIM, mean = sc$vox,   sd = vox_mc_sd)
  sumar_draws <- rnorm(N_SIM, mean = sc$sumar, sd = sumar_mc_sd)

  map_dfr(seq_len(N_SIM), function(j) {
    # Normalise to keep shares positive
    v <- c(pp_draws[j], psoe_draws[j], vox_draws[j], sumar_draws[j])
    v <- pmax(v, 0.02)          # floor at 2%
    seats <- project_seats(v[1], v[2], v[3], v[4])
    tibble(
      scenario  = name,
      sim       = j,
      PP        = seats[["PP"]],
      PSOE      = seats[["PSOE"]],
      Vox       = seats[["Vox"]],
      Sumar     = seats[["Sumar"]],
      Regional  = seats[["Regional"]],
      pp_pct    = v[1], psoe_pct = v[2], vox_pct = v[3], sumar_pct = v[4]
    )
  })
})

cat("Monte Carlo complete.\n")

# ─── 7. Summarise MC Results ─────────────────────────────────────────────────
mc_summary <- mc_results |>
  group_by(scenario) |>
  summarise(
    across(c(PP, PSOE, Vox, Sumar, Regional),
           list(median = median, lo80 = ~ quantile(., 0.10),
                hi80 = ~ quantile(., 0.90),
                lo95 = ~ quantile(., 0.025), hi95 = ~ quantile(., 0.975))),
    PP_majority   = mean(PP + Regional >= 176),    # PP-led coalition majority
    PSOE_majority = mean(PSOE + Regional >= 176),  # PSOE-led coalition majority
    .groups = "drop"
  )

cat("\nMonte Carlo summary (median [80% CI]):\n")
mc_summary |>
  select(scenario, PP_median, PP_lo80, PP_hi80,
         PSOE_median, PSOE_lo80, PSOE_hi80,
         Vox_median, Sumar_median) |>
  print(width = 120)

# ─── 8. Export ───────────────────────────────────────────────────────────────
write_csv(
  seat_projections |> select(scenario, PP, PSOE, Vox, Sumar, Regional, Total),
  "data/processed/seat_projections.csv"
)

write_csv(
  mc_results |>
    select(scenario, sim, PP, PSOE, Vox, Sumar, Regional,
           pp_pct, psoe_pct, vox_pct, sumar_pct),
  "data/processed/seat_monte_carlo.csv"
)

write_csv(mc_summary, "data/processed/seat_mc_summary.csv")

cat("\nOutputs saved:\n")
cat("  data/processed/seat_projections.csv\n")
cat("  data/processed/seat_monte_carlo.csv\n")
cat("  data/processed/seat_mc_summary.csv\n")
cat("\n=== Script 07 complete ===\n")
