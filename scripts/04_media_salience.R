# =============================================================================
# 04_media_salience.R
# Compute monthly Immigration Media Salience from El País
#
# Strategy:
#   1. Scrape El País immigration topic pages (/noticias/inmigracion/N/)
#      — each page has ~20 articles with datetime attributes
#   2. Paginate until we reach articles before Nov 2023
#   3. Count articles per month → immigration count (numerator)
#   4. Scrape El País general España section for total article count per month
#   5. Salience = immigration / total * 100
#
# Page structure:
#   URL: https://elpais.com/noticias/inmigracion/N/  (N = page number)
#   Dates: <time datetime="YYYY-MM-DDTHH:MM:SS+01:00">
#   Headlines: <h2>, <h3> inside <article>
# =============================================================================

library(rvest)
library(httr2)
library(tidyverse)
library(lubridate)
library(stringr)

USER_AGENT <- paste0(
  "Mozilla/5.0 (UC3M Data Harvesting Course Project; ",
  "Contact: 100502844@alumnos.uc3m.es)"
)

START_DATE <- as.Date("2023-11-01")
END_DATE   <- as.Date("2026-02-28")

# --- Helper: scrape one El País topic page ------------------------------------

scrape_elpais_page <- function(url) {
  page <- tryCatch(
    read_html(
      request(url) |>
        req_headers(
          `User-Agent`      = USER_AGENT,
          `Accept-Language` = "es-ES,es;q=0.9"
        ) |>
        req_retry(max_tries = 3, backoff = ~ 5) |>
        req_timeout(20) |>
        req_perform() |>
        resp_body_string()
    ),
    error = function(e) {
      message("  Failed: ", url, " — ", e$message)
      return(NULL)
    }
  )

  if (is.null(page)) return(tibble())
  Sys.sleep(runif(1, 3, 5))

  # Extract headlines
  headlines <- page |>
    html_elements("h2, h3") |>
    html_text(trim = TRUE) |>
    (\(x) x[nchar(x) > 15])()

  # Extract dates from time[datetime] attributes
  times <- page |>
    html_elements("time[datetime]") |>
    html_attr("datetime")

  # Parse ISO 8601 timestamps
  dates <- suppressWarnings(ymd_hms(times, tz = "Europe/Madrid"))

  # Match headlines to dates: typically one date per article header
  # pages show ~20 articles; align by position where possible
  n <- min(length(headlines), length(dates))
  if (n == 0 && length(dates) > 0) {
    # No headlines matched — return dates only (count articles)
    return(tibble(
      headline = NA_character_,
      date     = as.Date(dates),
      url      = url
    ))
  }

  tibble(
    headline = if (n > 0) headlines[seq_len(n)] else NA_character_,
    date     = if (n > 0) as.Date(dates[seq_len(n)]) else NA_Date_,
    url      = url
  )
}

# ==============================================================================
# PART 1: Scrape immigration topic pages
# ==============================================================================

cat("  Scraping El País immigration topic pages...\n")
cat("  Target: Nov 2023 – Feb 2026\n\n")

immigration_articles <- tibble()
base_url <- "https://elpais.com/noticias/inmigracion/"

for (page_num in 1:100) {
  page_url <- if (page_num == 1) base_url else paste0(base_url, page_num - 1, "/")

  cat("  Page", page_num, ":", page_url, "\n")
  page_data <- scrape_elpais_page(page_url)

  if (nrow(page_data) == 0) {
    cat("    No data — stopping\n")
    break
  }

  immigration_articles <- bind_rows(immigration_articles, page_data)

  # Check if we've reached articles older than our start date
  min_date <- min(page_data$date, na.rm = TRUE)
  cat("    Oldest date on page:", as.character(min_date), "| Articles:", nrow(page_data), "\n")

  if (!is.na(min_date) && min_date < START_DATE) {
    cat("  Reached pre-scope articles. Stopping.\n")
    break
  }
}

# Filter to project scope
immigration_articles <- immigration_articles |>
  filter(!is.na(date), date >= START_DATE, date <= END_DATE) |>
  distinct(headline, date, .keep_all = TRUE)

cat("\n  Total immigration articles in scope:", nrow(immigration_articles), "\n")
cat("  Date range:", as.character(min(immigration_articles$date, na.rm=TRUE)),
    "to", as.character(max(immigration_articles$date, na.rm=TRUE)), "\n")

# ==============================================================================
# PART 2: Scrape general España section for total article counts
# ==============================================================================

cat("\n  Scraping El País España section for total articles...\n")

espana_articles <- tibble()
espana_url <- "https://elpais.com/espana/"

for (page_num in 1:60) {
  # España section uses ?page=N for pagination (unlike immigration topic which uses /N/)
  page_url <- if (page_num == 1) espana_url else paste0(espana_url, "?page=", page_num)

  cat("  España page", page_num, "\n")
  page_data <- scrape_elpais_page(page_url)

  if (nrow(page_data) == 0) break

  espana_articles <- bind_rows(espana_articles, page_data)
  min_date <- min(page_data$date, na.rm = TRUE)

  if (!is.na(min_date) && min_date < START_DATE) break
}

espana_articles <- espana_articles |>
  filter(!is.na(date), date >= START_DATE, date <= END_DATE) |>
  distinct(headline, date, .keep_all = TRUE)

cat("  Total España articles in scope:", nrow(espana_articles), "\n")

# ==============================================================================
# PART 3: Save raw headlines
# ==============================================================================

all_headlines <- bind_rows(
  immigration_articles |> mutate(source = "immigration_topic"),
  espana_articles      |> mutate(source = "espana_section")
)

write_csv(all_headlines, "data/raw/elpais_headlines_raw.csv")
cat("\n  Saved raw headlines:", nrow(all_headlines), "rows\n")

# ==============================================================================
# PART 4: Compute monthly salience score
# ==============================================================================

months_df <- tibble(
  year  = c(rep(2023, 2), rep(2024, 12), rep(2025, 12), rep(2026, 2)),
  month = c(11, 12, 1:12, 1:12, 1, 2)
) |>
  mutate(year_month = as.Date(paste(year, month, "01", sep = "-")))

imm_monthly <- immigration_articles |>
  mutate(year_month = floor_date(date, "month")) |>
  count(year_month, name = "n_immigration")

total_monthly <- espana_articles |>
  mutate(year_month = floor_date(date, "month")) |>
  count(year_month, name = "n_espana")

salience_scores <- months_df |>
  left_join(imm_monthly,   by = "year_month") |>
  left_join(total_monthly, by = "year_month") |>
  mutate(
    n_immigration = replace_na(n_immigration, 0),
    n_espana      = replace_na(n_espana, 0),
    n_total       = n_immigration + n_espana,
    # Primary salience: raw immigration article count (more reliable metric
    # since general España section pagination may not go back 2+ years)
    # Ratio salience (when España data available): immigration / total * 100
    salience_score = if_else(
      n_total > 0,
      round(n_immigration / n_total * 100, 1),
      NA_real_
    ),
    # Normalised immigration volume (0–100 scale across months)
    imm_volume_norm = round(n_immigration / max(n_immigration, na.rm = TRUE) * 100, 1)
  ) |>
  select(year_month, year, month, n_immigration, n_espana, salience_score, imm_volume_norm)

cat("\n  Monthly salience scores:\n")
print(salience_scores |> filter(!is.na(salience_score)), n = 30)

write_csv(salience_scores, "data/processed/salience_scores.csv")
cat("\n  Saved:", nrow(salience_scores), "months of salience data\n")
