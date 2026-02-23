# =============================================================================
# 02_scrape_polls.R
# Scrape Spanish opinion poll data from Wikipedia
# Source: "Opinion polling for the next Spanish general election"
#
# Page structure (verified):
#   - 5 wikitables, sorted newest-first within each table
#   - Table 1: 2026 polls
#   - Table 2: 2025 polls
#   - Table 3: 2024 polls
#   - Table 4: post-July 2023 election polls (2023)
#   - Table 5: CIS-only historical series
#
# Column layout (from img alt attributes in header row):
#   Col 1: Polling firm | Col 2: Fieldwork date | Col 3: Sample size
#   Col 4: Turnout | Col 5: PP | Col 6: PSOE | Col 7: Vox | Col 8: Sumar
#   Col 9-18: smaller parties | Col 19: Lead
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

WIKI_URL <- paste0(
  "https://en.wikipedia.org/wiki/",
  "Opinion_polling_for_the_next_Spanish_general_election"
)

# --- Fetch the Wikipedia page -------------------------------------------------

cat("  Fetching Wikipedia opinion polling page...\n")

page <- tryCatch(
  read_html(
    request(WIKI_URL) |>
      req_headers(`User-Agent` = USER_AGENT) |>
      req_retry(max_tries = 3, backoff = ~ 5) |>
      req_perform() |>
      resp_body_string()
  ),
  error = function(e) stop("Failed to fetch Wikipedia polls page: ", e$message)
)

Sys.sleep(2)

# --- Extract column names from header row img alt attributes ------------------
# Wikipedia uses party logos (images) as column headers — text is in img$alt

get_col_names <- function(tbl) {
  rows <- tbl |> html_elements("tr")
  if (length(rows) == 0) return(character(0))

  header_row <- rows[[1]]
  ths <- header_row |> html_elements("th")

  names <- map_chr(ths, function(th) {
    # Try plain text first
    txt <- html_text(th, trim = TRUE)
    if (nchar(txt) > 0) return(txt)
    # Then img alt
    img_alt <- th |> html_element("img") |> html_attr("alt")
    if (!is.na(img_alt) && nchar(img_alt) > 0) return(img_alt)
    # Then link title
    a_title <- th |> html_element("a") |> html_attr("title")
    if (!is.na(a_title) && nchar(a_title) > 0) return(a_title)
    return("unknown")
  })

  # Deduplicate
  make.names(names, unique = TRUE)
}

# --- Parse one wikitable into a clean data frame ------------------------------

parse_polls_table <- function(tbl, assumed_year) {
  col_names <- get_col_names(tbl)
  if (length(col_names) == 0) return(tibble())

  # Get data rows (skip header row 1)
  rows <- tbl |> html_elements("tr")
  data_rows <- rows[-1]  # drop header

  parsed <- map_dfr(data_rows, function(row) {
    cells <- row |> html_elements("td, th") |> html_text(trim = TRUE)
    if (length(cells) < 6) return(tibble())

    # Pad or trim to match column count
    n_cols <- length(col_names)
    if (length(cells) < n_cols) cells <- c(cells, rep(NA_character_, n_cols - length(cells)))
    if (length(cells) > n_cols) cells <- cells[seq_len(n_cols)]

    as_tibble(as.list(cells), .name_repair = "minimal") |>
      setNames(col_names)
  })

  if (nrow(parsed) == 0) return(tibble())

  # Identify key columns dynamically
  find_col <- function(patterns) {
    nms <- colnames(parsed)
    for (p in patterns) {
      m <- str_detect(nms, regex(p, ignore_case = TRUE))
      if (any(m)) return(nms[which(m)[1]])
    }
    NA_character_
  }

  col_pollster <- find_col(c("Polling.firm", "Pollster", "Commissioner", "firm"))
  col_date     <- find_col(c("Fieldwork", "date", "Date"))
  col_pp       <- find_col(c("^PP$", "^PP\\.", "\\.PP$"))
  col_psoe     <- find_col(c("^PSOE", "PSOE"))
  col_vox      <- find_col(c("^Vox", "Vox"))
  col_sumar    <- find_col(c("^Sumar", "Sumar"))

  clean_pct <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_real_)
    x |>
      str_replace_all("%|\\[\\d+\\]|\\?", "") |>
      str_replace_all(",", ".") |>
      str_trim() |>
      suppressWarnings(as.numeric())
  }

  # Parse date: fieldwork dates look like "22–29 Dec", "31 Dec", "2–6 Feb 2026"
  parse_date_field <- function(x, year_hint) {
    if (is.na(x) || nchar(trimws(x)) == 0) return(NA_Date_)

    # Remove footnotes like [1]
    x <- str_replace_all(x, "\\[\\d+\\]", "") |> str_trim()

    # If contains a 4-digit year, use it
    if (str_detect(x, "\\d{4}")) {
      # Extract end date from range if needed: "2–6 Feb 2026" -> "6 Feb 2026"
      end_part <- str_extract(x, "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}")
      if (is.na(end_part)) end_part <- x
      return(suppressWarnings(dmy(end_part)))
    }

    # No year present — extract end date from range "22–29 Dec" -> "29 Dec"
    # Or single date "31 Dec"
    end_part <- str_extract(x, "\\d{1,2}\\s+[A-Za-z]+$")
    if (is.na(end_part)) end_part <- str_extract(x, "[A-Za-z]+$")
    if (is.na(end_part)) return(NA_Date_)

    d <- suppressWarnings(dmy(paste(end_part, year_hint)))

    # If the parsed month > current row's assumed year context, it crosses years
    # e.g., "27 Dec–3 Jan" in 2025 table → end date Jan 2026
    if (!is.na(d) && month(d) == 1 && str_detect(x, "Dec|Dic")) {
      d <- d + years(1)   # cross-year: Jan belongs to next year
    }
    d
  }

  # Safe column extractor — returns NA vector if column not found
  get_col <- function(df, col_name) {
    if (is.na(col_name) || !col_name %in% colnames(df))
      return(rep(NA_character_, nrow(df)))
    df[[col_name]]
  }

  out <- parsed |>
    mutate(
      pollster     = get_col(parsed, col_pollster),
      date_raw     = get_col(parsed, col_date),
      pp_pct       = as.double(sapply(get_col(parsed, col_pp),    clean_pct)),
      psoe_pct     = as.double(sapply(get_col(parsed, col_psoe),  clean_pct)),
      vox_pct      = as.double(sapply(get_col(parsed, col_vox),   clean_pct)),
      sumar_pct    = as.double(sapply(get_col(parsed, col_sumar), clean_pct)),
      date         = map_vec(date_raw, parse_date_field, year_hint = assumed_year),
      assumed_year = assumed_year
    ) |>
    select(pollster, date_raw, date, pp_pct, psoe_pct, vox_pct, sumar_pct, assumed_year) |>
    filter(!is.na(pollster), nchar(pollster) > 2)

  out
}

# --- Extract all 5 wikitables -------------------------------------------------

tables <- page |> html_elements("table.wikitable")
cat("  Found", length(tables), "wikitables\n")

# Assign expected year to each table based on position
# Table 1=2026, 2=2025, 3=2024, 4=2023, 5=2026 (CIS)
table_years <- c(2026, 2025, 2024, 2023, 2026)

all_polls <- map2_dfr(
  tables,
  table_years[seq_along(tables)],
  function(tbl, yr) {
    cat("  Parsing table (year ~", yr, ")...\n")
    parse_polls_table(tbl, yr)
  }
)

cat("  Total rows parsed:", nrow(all_polls), "\n")

# --- Filter to project scope: Nov 2023 – Feb 2026 ----------------------------

polls_in_scope <- all_polls |>
  filter(
    !is.na(date),
    date >= as.Date("2023-11-01"),
    date <= as.Date("2026-02-28"),
    !is.na(psoe_pct)
  ) |>
  distinct(pollster, date, .keep_all = TRUE) |>
  arrange(date)

cat("  Polls in scope (Nov 2023–Feb 2026):", nrow(polls_in_scope), "\n")
if (nrow(polls_in_scope) > 0) {
  cat("  Date range:", as.character(min(polls_in_scope$date, na.rm=TRUE)),
      "to", as.character(max(polls_in_scope$date, na.rm=TRUE)), "\n")
  cat("  PSOE range:", round(min(polls_in_scope$psoe_pct, na.rm=TRUE), 1),
      "–", round(max(polls_in_scope$psoe_pct, na.rm=TRUE), 1), "%\n")
}

# --- Save raw -----------------------------------------------------------------

write_csv(polls_in_scope, "data/raw/polls_raw.csv")

# --- Monthly PSOE average ----------------------------------------------------

monthly_polls <- polls_in_scope |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarise(
    psoe_avg  = mean(psoe_pct,  na.rm = TRUE),
    pp_avg    = mean(pp_pct,    na.rm = TRUE),
    vox_avg   = mean(vox_pct,   na.rm = TRUE),
    sumar_avg = mean(sumar_pct, na.rm = TRUE),
    n_polls   = n(),
    .groups   = "drop"
  ) |>
  filter(!is.nan(psoe_avg)) |>
  arrange(year_month)

cat("  Monthly averages:", nrow(monthly_polls), "months\n")
print(monthly_polls |> select(year_month, psoe_avg, pp_avg, n_polls), n = 10)

write_csv(monthly_polls, "data/processed/monthly_polls.csv")
