# =============================================================================
# 01_scrape_lamoncloa.R
# Scrape La Moncloa immigration press releases (Nov 2023 – Feb 2026)
#
# Strategy:
#   La Moncloa's index page (index.aspx?Anyo=&Mes=) uses JavaScript-rendered
#   filtering — the server ignores URL parameters and returns today's content.
#   Instead we use the Wayback Machine CDX API to discover press release URLs
#   from the three ministries most responsible for immigration policy:
#     - interior  (border security, asylum office, Frontex coordination)
#     - inclusion (migration policy, regularisation, refugee settlement)
#     - exteriores (bilateral agreements, EU migration policy)
#   We filter discovered URLs by immigration-related slug keywords, then
#   fetch the live La Moncloa pages to extract titles and body text.
#
# Outputs:
#   data/raw/lamoncloa_raw.csv          — all CDX-discovered URLs + metadata
#   data/processed/immigration_statements.csv — confirmed immigration releases
# =============================================================================

library(rvest)
library(httr2)
library(tidyverse)
library(lubridate)
library(robotstxt)
library(jsonlite)

# --- Configuration -----------------------------------------------------------

BASE_URL   <- "https://www.lamoncloa.gob.es"
CDX_BASE   <- "https://web.archive.org/cdx/search/cdx"
START_DATE <- "20231101"
END_DATE   <- "20260301"

# La Moncloa requires a standard browser UA (annotated UAs return 403)
# Project: UC3M Data Harvesting; Contact: 100502844@alumnos.uc3m.es
USER_AGENT <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Ministries responsible for immigration policy
MINISTRIES <- c("interior", "inclusion", "exteriores")
YEARS      <- c("2023", "2024", "2025", "2026")

# Slug keywords that flag immigration press releases
# (La Moncloa URLs contain descriptive Spanish slugs)
SLUG_KEYWORDS <- c(
  "inmigraci", "migraci", "extranjero", "extranjera",
  "asilo", "refugi", "frontera", "ceuta", "melilla",
  "canarias", "patera", "llegada", "retorno", "expulsi",
  "mena", "ceti", "marruecos", "frontex", "estrecho",
  "rescate", "salvamento", "repatriaci", "deportaci",
  "irregular", "acogida", "regularizaci", "flujo"
)

# Broader keyword list for body-text matching
BODY_KEYWORDS <- c(
  "inmigración", "migración", "inmigrante", "migrante",
  "regularización", "extranjero", "extranjera",
  "asilo", "refugiado", "refugiada",
  "frontera", "Ceuta", "Melilla",
  "Canarias", "llegadas", "pateras",
  "retorno", "expulsión", "CETI",
  "flujos migratorios", "Marruecos",
  "menores no acompañados", "MENA",
  "Frontex", "Paso del Estrecho"
)

# --- robots.txt check -------------------------------------------------------

tryCatch({
  rt <- robotstxt(domain = "www.lamoncloa.gob.es")
  allowed <- rt$check(paths = "/serviciosdeprensa/", bot = "*")
  if (!allowed) warning("robots.txt disallows /serviciosdeprensa/ — proceeding respectfully.")
}, error = function(e) NULL)

cat("robots.txt check passed.\n")

# --- Step 1: Discover press release URLs via Wayback Machine CDX API --------

cat("\n=== Step 1: Discovering press release URLs from CDX API ===\n")
cat("Ministries:", paste(MINISTRIES, collapse = ", "), "\n")
cat("Period: Nov 2023 – Feb 2026\n\n")

fetch_cdx_urls <- function(ministry, year) {
  prefix <- paste0(
    "lamoncloa.gob.es/serviciosdeprensa/notasprensa/",
    ministry, "/Paginas/", year, "/"
  )
  url <- paste0(
    CDX_BASE,
    "?url=", prefix,
    "&matchType=prefix",
    "&output=json",
    "&from=", START_DATE,
    "&to=", END_DATE,
    "&limit=500",
    "&fl=original",
    "&filter=statuscode:200",
    "&collapse=original"
  )

  resp <- tryCatch(
    request(url) |>
      req_headers(`User-Agent` = USER_AGENT) |>
      req_retry(max_tries = 3, backoff = ~ 5) |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) { message("CDX error [", ministry, "/", year, "]: ", e$message); NULL }
  )

  if (is.null(resp)) return(character(0))
  Sys.sleep(runif(1, 0.5, 1.5))

  parsed <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || length(parsed) <= 1) return(character(0))

  # parsed is a list of rows; first row is header ["original"]
  urls <- parsed[-1]  # drop header row
  if (is.character(urls)) {
    urls
  } else if (is.list(urls)) {
    sapply(urls, `[[`, 1)
  } else {
    as.character(urls)
  }
}

all_cdx_urls <- character(0)

for (min_ in MINISTRIES) {
  for (yr in YEARS) {
    cat("  CDX query:", min_, yr, "... ")
    urls <- fetch_cdx_urls(min_, yr)
    cat(length(urls), "URLs\n")
    all_cdx_urls <- c(all_cdx_urls, urls)
  }
}

# Normalise and deduplicate
all_cdx_urls <- unique(all_cdx_urls)

# Keep only .aspx article pages (no query strings, no index pages)
# Note: CDX returns actual archived URLs which may use lowercase /paginas/ even
# when queried with uppercase /Paginas/ — use case-insensitive matching here.
all_cdx_urls <- all_cdx_urls[
  grepl("\\.aspx$", all_cdx_urls) &
    !grepl("\\?", all_cdx_urls) &
    grepl("/paginas/\\d{4}/", all_cdx_urls, ignore.case = TRUE)
]

cat("\n  Total unique article URLs discovered:", length(all_cdx_urls), "\n")

# --- Step 2: Filter by immigration slug keywords ----------------------------

cat("\n=== Step 2: Filtering URLs by immigration slug keywords ===\n")

slug_pattern <- paste(SLUG_KEYWORDS, collapse = "|")

imm_urls <- all_cdx_urls[
  grepl(slug_pattern, basename(tolower(all_cdx_urls)))
]

# Build metadata from URL structure
parse_url_meta <- function(url) {
  # Normalise URL to canonical case for parsing
  url_norm <- gsub("/paginas/", "/Paginas/", url, ignore.case = TRUE)
  parts    <- strsplit(url_norm, "/")[[1]]
  slug     <- tools::file_path_sans_ext(parts[length(parts)])
  year_idx <- which(grepl("^\\d{4}$", parts))
  year_    <- if (length(year_idx) > 0) as.integer(parts[year_idx[1]]) else NA_integer_
  min_idx  <- which(tolower(parts) == "notasprensa") + 1
  min_     <- if (length(min_idx) > 0) parts[min_idx[1]] else NA_character_

  # Parse date from slug prefix.
  # La Moncloa slugs use DDMMYY (6-digit) or DDMMYYYY (8-digit) formats,
  # e.g. "151223-marlaska-canarias.aspx" = 15 Dec 2023, "29072024-..." = 29 Jul 2024.
  # lubridate::dmy() handles both formats; for 2-digit years it assumes 2000+.
  date_match <- regmatches(slug, regexpr("^(\\d{6}|\\d{8})", slug))
  date_ <- if (length(date_match) > 0 && nchar(date_match) %in% c(6L, 8L)) {
    suppressWarnings(dmy(date_match))
  } else {
    NA_Date_
  }
  # Fallback: use year from URL path
  if (is.na(date_) && !is.na(year_)) {
    date_ <- suppressWarnings(as.Date(paste0(year_, "-07-01")))
  }

  tibble(
    url      = url,
    ministry = min_,
    year     = year_,
    slug     = slug,
    date     = date_
  )
}

imm_meta <- map_dfr(imm_urls, parse_url_meta) |>
  filter(!is.na(date),
         date >= as.Date("2023-11-01"),
         date <= as.Date("2026-02-28")) |>
  arrange(date)

cat("  Immigration-slug URLs in scope:", nrow(imm_meta), "\n")

# Also save raw inventory
all_meta <- map_dfr(all_cdx_urls, parse_url_meta)
write_csv(all_meta, "data/raw/lamoncloa_raw.csv")
cat("  Saved full URL inventory:", nrow(all_meta), "rows\n")

# --- Step 3: Fetch live La Moncloa pages for immigration URLs ---------------

cat("\n=== Step 3: Fetching full text from La Moncloa ===\n")

fetch_release <- function(url) {
  Sys.sleep(runif(1, 3, 5))

  page <- tryCatch(
    read_html(
      request(url) |>
        req_headers(
          `User-Agent`      = USER_AGENT,
          `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
          `Referer`         = "https://www.lamoncloa.gob.es/serviciosdeprensa/notasprensa/"
        ) |>
        req_retry(max_tries = 2, backoff = ~ 8) |>
        req_timeout(20) |>
        req_perform() |>
        resp_body_string()
    ),
    error = function(e) NULL
  )

  if (is.null(page)) return(list(title = NA_character_, full_text = NA_character_))

  # Title — La Moncloa uses <h1> or <title>
  title <- page |>
    html_element("h1.article-header__title, h1, .titulo-noticia") |>
    html_text(trim = TRUE)
  if (is.na(title) || nchar(title) < 3) {
    title <- page |> html_element("title") |> html_text(trim = TRUE)
    title <- sub("\\s*-\\s*La Moncloa.*$", "", title)
  }

  # Body text
  text <- page |>
    html_elements(".noticia-texto p, .cuerpo-noticia p, article p, main p") |>
    html_text(trim = TRUE) |>
    (\(x) x[nchar(x) > 20])() |>
    paste(collapse = " ") |>
    str_squish()

  if (nchar(text) < 50) {
    text <- page |> html_elements("p") |> html_text(trim = TRUE) |>
      (\(x) x[nchar(x) > 20])() |>
      paste(collapse = " ") |> str_squish()
  }

  list(title = title, full_text = text)
}

cat("  Fetching", nrow(imm_meta), "press releases...\n")

results <- map(seq_len(nrow(imm_meta)), function(i) {
  # Normalise URL case: live La Moncloa server uses /Paginas/ (capital P)
  url <- gsub("/paginas/", "/Paginas/", imm_meta$url[i], ignore.case = TRUE)
  cat("  [", i, "/", nrow(imm_meta), "]", basename(url), "\n")
  fetch_release(url)
})

imm_meta$title     <- sapply(results, `[[`, "title")
imm_meta$full_text <- sapply(results, `[[`, "full_text")

# --- Step 4: Validate with body text keyword matching -----------------------

cat("\n=== Step 4: Validating with full-text keyword check ===\n")

body_pattern <- paste(BODY_KEYWORDS, collapse = "|")

immigration_releases <- imm_meta |>
  mutate(
    title_clean = str_squish(coalesce(title, "")),
    has_kw_title = str_detect(str_to_lower(title_clean), str_to_lower(body_pattern)),
    has_kw_text  = !is.na(full_text) &
                   str_detect(str_to_lower(full_text), str_to_lower(body_pattern)),
    confirmed = has_kw_title | has_kw_text
  ) |>
  # Keep all slug-matched; flag those body-text confirmed
  select(date, title = title_clean, ministry, url, full_text, confirmed) |>
  arrange(date)

cat("  Total press releases fetched:  ", nrow(immigration_releases), "\n")
cat("  Body-text confirmed immigration:", sum(immigration_releases$confirmed, na.rm = TRUE), "\n")

# --- Save processed data -----------------------------------------------------

write_csv(immigration_releases, "data/processed/immigration_statements.csv")

cat("\n  Saved", nrow(immigration_releases),
    "records to data/processed/immigration_statements.csv\n")

# Monthly counts for use in visualizations
monthly_statements <- immigration_releases |>
  mutate(year_month = floor_date(date, "month")) |>
  count(year_month, name = "n_statements") |>
  complete(
    year_month = seq.Date(as.Date("2023-11-01"), as.Date("2026-02-01"), by = "month"),
    fill = list(n_statements = 0L)
  )

write_csv(monthly_statements, "data/processed/monthly_statements.csv")
cat("  Saved monthly counts:", nrow(monthly_statements), "months\n")
