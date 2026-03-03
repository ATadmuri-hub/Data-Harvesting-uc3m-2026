# =============================================================================
# main.R — Master Orchestration Script
# Spanish Immigration Rhetoric & Government Popularity (2023–2026)
# Data Harvesting Course — UC3M, 2026
# =============================================================================
# Run this file to reproduce the entire pipeline:
#   Rscript main.R
#
# To also render the HTML report after data collection:
#   Rscript -e "rmarkdown::render('report.Rmd')"
#
# Estimated runtime: 10–20 minutes (due to polite sleep delays)
# =============================================================================

cat("=== Spain Immigration Data Harvesting Pipeline ===\n\n")

# --- 0. Check required packages -----------------------------------------------
required_packages <- c(
  "rvest", "httr2", "jsonlite", "xml2",
  "tidyverse", "lubridate", "stringr",
  "sf", "ggplot2", "patchwork", "scales", "robotstxt",
  "broom", "lmtest", "sandwich", "forecast", "zoo",
  "plotly", "kableExtra", "ggcorrplot", "rmarkdown"
)

missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# --- 1. Scrape La Moncloa press releases --------------------------------------
cat("[1/8] Scraping La Moncloa press releases...\n")
source("scripts/01_scrape_lamoncloa.R")
cat("    -> Saved: data/raw/lamoncloa_raw.csv\n")
cat("    -> Saved: data/processed/immigration_statements.csv\n")
cat("    -> Saved: data/processed/monthly_statements.csv\n\n")

# --- 2. Scrape opinion polls from Wikipedia -----------------------------------
cat("[2/8] Scraping Spanish opinion polls from Wikipedia...\n")
source("scripts/02_scrape_polls.R")
cat("    -> Saved: data/raw/polls_raw.csv\n")
cat("    -> Saved: data/processed/monthly_polls.csv\n\n")

# --- 3. Fetch INE demographic data via API ------------------------------------
cat("[3/8] Fetching INE demographic data (API)...\n")
source("scripts/03_ine_demographics.R")
cat("    -> Saved: data/raw/ine_demographics_raw.csv\n\n")

# --- 4. Scrape El País for media salience -------------------------------------
cat("[4/8] Scraping El País for immigration media salience...\n")
source("scripts/04_media_salience.R")
cat("    -> Saved: data/raw/elpais_headlines_raw.csv\n")
cat("    -> Saved: data/processed/salience_scores.csv\n\n")

# --- 5. Build visualisations --------------------------------------------------
cat("[5/8] Generating visualisations...\n")
source("scripts/05_visualizations.R")
cat("    -> Saved: output/plots/01_speech_vs_polls_timeline.png\n")
cat("    -> Saved: output/plots/02_reaction_map.png\n")
cat("    -> Saved: output/plots/03_salience_barchart.png\n\n")

# --- 6. Statistical analysis and 2027 forecast --------------------------------
cat("[6/8] Running statistical analysis and 2027 electoral forecast...\n")
source("scripts/06_analysis_model.R")
cat("    -> Saved: data/processed/master_panel.csv\n")
cat("    -> Saved: data/processed/descriptive_stats.csv\n")
cat("    -> Saved: data/processed/correlation_matrix.csv\n")
cat("    -> Saved: data/processed/crosscorr_statements_psoe.csv\n")
cat("    -> Saved: data/processed/model_results.csv\n")
cat("    -> Saved: data/processed/forecast_2027.csv\n")
cat("    -> Saved: data/processed/var_forecast_2027.csv\n")
cat("    -> Saved: data/processed/arima_prediction_se.csv\n")
cat("    -> Saved: data/processed/pp_psoe_correlation.csv\n")
cat("    -> Saved: data/processed/forecast_accuracy.csv\n")
cat("    -> Saved: data/processed/diagnostics_summary.csv\n\n")

# --- 7. D'Hondt seat projection model -----------------------------------------
cat("[7/8] Running D'Hondt seat projection model (Monte Carlo n=5000)...\n")
source("scripts/07_dhondt_projection.R")
cat("    -> Saved: data/processed/seat_projections.csv\n")
cat("    -> Saved: data/processed/seat_monte_carlo.csv\n")
cat("    -> Saved: data/processed/seat_mc_summary.csv\n\n")

# --- 8. Historical 2005 comparison --------------------------------------------
cat("[8/8] Scraping 2004-2008 polls and running ITS analysis...\n")
source("scripts/08_historical_2005.R")
cat("    -> Saved: data/processed/historical_polls_2004_2008.csv\n")
cat("    -> Saved: data/processed/its_results.csv\n\n")

cat("=== Pipeline complete. ===\n")
cat("  Data outputs: data/processed/\n")
cat("  Visualisations: output/plots/\n")
cat("  Render report: Rscript -e \"rmarkdown::render('report.Rmd')\"\n")
