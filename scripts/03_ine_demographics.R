# =============================================================================
# 03_ine_demographics.R
# Fetch demographic data from the INE (Instituto Nacional de Estadística) API
#
# Data collected (confirmed working endpoints):
#   1. Unemployment rate by province — EPA Table 65349
#      Operation 293 (Encuesta de Población Activa), quarterly, ~477 series
#   2. Foreign population by nationality trend — Table 9683
#      Filtered to: Total foreign pop (non-Spanish), Ambos sexos, national level
#      Variable filters: tv=70032:16420 (Total nationality), tv=70013:451 (Ambos sexos)
#
# INE JSON API base: https://servicios.ine.es/wstempus/js/ES/
# Docs: https://www.ine.es/dyngs/DAB/en/index.htm?cid=1102
# =============================================================================

library(httr2)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(stringr)

USER_AGENT <- paste0(
  "Mozilla/5.0 (UC3M Data Harvesting Course Project; ",
  "Contact: 100502844@alumnos.uc3m.es)"
)

INE_BASE <- "https://servicios.ine.es/wstempus/js/ES"

# --- Helper: fetch INE table and return parsed JSON list ----------------------

fetch_ine <- function(endpoint, params = "") {
  url <- paste0(INE_BASE, "/", endpoint, if (nchar(params) > 0) paste0("?", params) else "")
  cat("  GET", url, "\n")

  resp <- tryCatch(
    request(url) |>
      req_headers(`User-Agent` = USER_AGENT) |>
      req_retry(max_tries = 3, backoff = ~ 10) |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) {
      message("  ERROR: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)

  body <- resp_body_string(resp)

  # INE sometimes returns a JSON object with a "status" error
  if (str_detect(body, '"status"\\s*:\\s*"No puede')) {
    message("  INE volume restriction — try with stronger filters")
    return(NULL)
  }

  Sys.sleep(runif(1, 2, 3))

  tryCatch(
    fromJSON(body, simplifyVector = FALSE),
    error = function(e) {
      message("  JSON parse error: ", e$message)
      return(NULL)
    }
  )
}

# --- Helper: flatten list-of-series to data frame ----------------------------
# NombrePeriodo is often empty in EPA; use Anyo + FK_Periodo instead.
# FK_Periodo for EPA quarterly data: 19=Q1 2025, 20=Q2 2025, 21=Q3 2025, 22=Q4 2025
# Quarter within year = ((FK_Periodo - 3) %% 4) + 1  (empirically derived)

fkperiodo_to_quarter <- function(fkp) {
  # FK_Periodo 19 → Q1, 20 → Q2, 21 → Q3, 22 → Q4
  # Pattern: (fkp - 3) mod 4 gives 0..3, then +1 gives 1..4
  q <- ((as.integer(fkp) - 3) %% 4) + 1
  # Guard: if mod arithmetic gives 0 or negative due to edge cases
  if (is.na(q) || q < 1 || q > 4) q <- 1L
  q
}

quarter_to_month <- function(q) {
  # Q1→"03", Q2→"06", Q3→"09", Q4→"12"
  c("03", "06", "09", "12")[max(1, min(4, q))]
}

flatten_series <- function(series_list) {
  if (is.null(series_list) || length(series_list) == 0) return(tibble())

  map_dfr(series_list, function(s) {
    if (!is.list(s) || is.null(s$Data) || length(s$Data) == 0) return(tibble())
    map_dfr(s$Data, function(d) {
      yr  <- as.integer(d$Anyo %||% NA_integer_)
      fkp <- as.integer(d$FK_Periodo %||% NA_integer_)
      q   <- if (!is.na(fkp)) fkperiodo_to_quarter(fkp) else NA_integer_
      mo  <- if (!is.na(q))   quarter_to_month(q)        else "06"

      date_str <- if (!is.na(yr)) paste0(yr, "-", mo, "-01") else NA_character_
      tibble(
        serie_name = s$Nombre %||% NA_character_,
        period     = d$NombrePeriodo %||% NA_character_,
        fk_periodo = fkp,
        year       = yr,
        quarter    = q,
        date       = suppressWarnings(as.Date(date_str)),
        value      = as.numeric(d$Valor %||% NA_real_)
      )
    })
  })
}

# ==============================================================================
# 1. Unemployment rate by province (EPA)
#    Table 65349: Tasas de actividad, paro y empleo por provincia y sexo
#    477 series — filter to Tasa de paro + Ambos sexos
# ==============================================================================

cat("\n=== 1. EPA Unemployment by Province (Table 65349) ===\n")

epa_raw <- fetch_ine("DATOS_TABLA/65349", "nult=20")

if (!is.null(epa_raw)) {
  unemployment_long <- flatten_series(epa_raw) |>
    filter(
      str_detect(serie_name, regex("Tasa de paro", ignore_case = TRUE)),
      str_detect(serie_name, regex("Ambos sexos",  ignore_case = TRUE))
    ) |>
    mutate(
      # Series name format: "Tasa de paro de la población. PROVINCE. Ambos sexos. Total."
      # National total format: "Tasa de paro de la población. Ambos sexos. Total Nacional. Total."
      province = str_extract(serie_name,
                             "(?<=de la población\\. )(.+?)(?=\\. Ambos sexos)"),
      year     = as.integer(year)
    ) |>
    filter(!is.na(value)) |>
    select(province, fk_periodo, year, quarter, date, unemployment_rate = value)

  cat("  Records:", nrow(unemployment_long), "\n")
  cat("  Provinces:", n_distinct(unemployment_long$province), "\n")
  cat("  Year range:", min(unemployment_long$year, na.rm=TRUE),
      "–", max(unemployment_long$year, na.rm=TRUE), "\n")

  # Annual average per province
  unemployment_annual <- unemployment_long |>
    group_by(province, year) |>
    summarise(
      unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
      n_quarters        = n(),
      .groups           = "drop"
    ) |>
    filter(year >= 2022)

  write_csv(unemployment_long,   "data/raw/ine_unemployment_quarterly.csv")
  write_csv(unemployment_annual, "data/raw/ine_unemployment_annual.csv")
  cat("  Saved quarterly and annual unemployment CSVs\n")

} else {
  cat("  WARNING: EPA table unavailable\n")
  unemployment_annual <- tibble()
}

# ==============================================================================
# 2. Foreign population national trend
#    Strategy: Foreign pop = Total pop - Spanish pop
#    Series CP305495 = Total Nacional, Total, Todas edades, Total, Población
#    Series CP305423 = Total Nacional, Española, Todas edades, Total, Población
#    Both from Table 9683 (Estadística Continua de Población, Op 72)
# ==============================================================================

cat("\n=== 2. Foreign Population Trend (ECP, via DATOS_SERIE) ===\n")

fetch_series <- function(cod) {
  url <- paste0(INE_BASE, "/DATOS_SERIE/", cod, "?nult=10")
  cat("  GET", url, "\n")
  resp <- tryCatch(
    request(url) |>
      req_headers(`User-Agent` = USER_AGENT) |>
      req_retry(max_tries = 2, backoff = ~ 5) |>
      req_perform(),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(resp)) return(tibble())
  Sys.sleep(runif(1, 1.5, 3))

  parsed <- tryCatch(
    fromJSON(resp_body_string(resp), simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed) || is.null(parsed$Data)) return(tibble())

  as_tibble(parsed$Data) |>
    mutate(
      serie_name = parsed$Nombre,
      year       = as.integer(Anyo),
      # NombrePeriodo is e.g. "1 enero 2024"
      date       = suppressWarnings(
        as.Date(paste0(year, "-01-01"))  # annual data: always Jan 1
      ),
      value      = as.numeric(Valor)
    ) |>
    select(serie_name, year, date, value)
}

total_pop   <- fetch_series("CP305495")  # Total nacional, all nationalities
spanish_pop <- fetch_series("CP305423")  # Total nacional, Spanish nationality

if (nrow(total_pop) > 0 && nrow(spanish_pop) > 0) {
  # ECP data is quarterly — take annual average per year
  total_annual   <- total_pop   |> group_by(year) |>
    summarise(total_pop   = mean(value, na.rm = TRUE), .groups = "drop")
  spanish_annual <- spanish_pop |> group_by(year) |>
    summarise(spanish_pop = mean(value, na.rm = TRUE), .groups = "drop")

  foreign_pop <- total_annual |>
    left_join(spanish_annual, by = "year") |>
    mutate(
      foreign_pop = total_pop - spanish_pop,
      pct_foreign = round(foreign_pop / total_pop * 100, 2),
      date        = as.Date(paste0(year, "-01-01"))
    ) |>
    filter(year >= 2020) |>
    select(year, date, total_pop, spanish_pop, foreign_pop, pct_foreign)

  cat("  Foreign population records:", nrow(foreign_pop), "\n")
  print(foreign_pop |> select(year, foreign_pop, pct_foreign))
  write_csv(foreign_pop, "data/raw/ine_foreign_population.csv")
} else {
  cat("  WARNING: Could not fetch ECP series — saving placeholder\n")
  foreign_pop <- tibble()
  write_csv(tibble(note = "ECP series unavailable"), "data/raw/ine_foreign_population.csv")
}

# ==============================================================================
# 3. Combine and save merged demographics
# ==============================================================================

cat("\n=== 3. Saving demographics ===\n")

# Build a combined raw CSV for reproducibility
unemp_rows <- if (nrow(unemployment_annual) > 0) {
  unemployment_annual |>
    mutate(indicator = "unemployment_rate", value = unemployment_rate) |>
    select(province, year, indicator, value)
} else tibble()

foreign_rows <- if (nrow(foreign_pop) > 0) {
  foreign_pop |>
    mutate(indicator = "foreign_pop_national", province = "Total Nacional") |>
    select(province, year, indicator, value = foreign_pop)
} else tibble()

combined <- bind_rows(unemp_rows, foreign_rows)

write_csv(combined, "data/raw/ine_demographics_raw.csv")
cat("  Saved combined demographics:", nrow(combined), "rows\n")
