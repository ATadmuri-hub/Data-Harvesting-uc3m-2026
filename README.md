# Spanish Immigration Rhetoric & Government Popularity (2023–2026)

A reproducible data harvesting pipeline built in R for a Data Harvesting course project (UC3M, 2026).

## Research Question

Does the frequency and tone of official government immigration statements correlate with shifts in PSOE/government popularity in opinion polls between November 2023 and February 2026?

## Team

-   Abdullah Tadmuri ,[100502844\@alumnos.uc3m.es](mailto:100502844@alumnos.uc3m.es){.email} [https://github.com/ATadmuri-hub]
-   Maiheliya Maimaitiming ,[100562134\@alumnos.uc3m.es](mailto:100562134@alumnos.uc3m.es){.email} [https://github.com/Ines11python]
-   Tone Varberg Sabri ,[100570811\@alumnos.uc3m.es](mailto:100570811@alumnos.uc3m.es){.email} [https://github.com/tone-vs]


## Data Sources

| Source | Method | Data Collected |
|-------------------|-------------------|------------------------------------|
| [La Moncloa Newsroom](https://www.lamoncloa.gob.es/serviciosdeprensa/notasprensa/Paginas/index.aspx) | Wayback Machine CDX API → `rvest` | Official immigration press releases from interior/inclusion/exteriores ministries (Nov 2023 – Feb 2026) |
| [Wikipedia – Spanish Opinion Polls](https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election) | `rvest` scraping | Monthly PSOE/PP/Vox voting intentions by pollster |
| [INE JSON API](https://servicios.ine.es/wstempus/js/) | `httr2` + `jsonlite` | Foreign-born population by province; unemployment rate by province |
| [El País](https://elpais.com/espana/) | `rvest` scraping | News headlines to compute Immigration Media Salience score |

## Project Structure

```         
spain-immigration-project/
├── README.md
├── main.R                        # Master script ,runs all steps in order
├── report.Rmd                    # Full HTML report (render after main.R)
├── report_style.css              # Custom CSS for the HTML report
├── scripts/
│   ├── 01_scrape_lamoncloa.R    # Scrape La Moncloa press releases
│   ├── 02_scrape_polls.R         # Scrape Wikipedia opinion polls
│   ├── 03_ine_demographics.R     # INE API: foreign population + unemployment
│   ├── 04_media_salience.R       # El País headline scraper + salience score
│   ├── 05_visualizations.R       # All plots and maps
│   └── 06_analysis_model.R       # Statistical analysis + 2027 electoral forecast
├── data/
│   ├── raw/                      # Raw scraped/downloaded data (CSV)
│   └── processed/                # Cleaned, merged data ready for analysis
└── output/
    └── plots/                    # Generated PNG/PDF visualizations
```

## How to Reproduce

### 1. Prerequisites

Install R (≥ 4.3) and the following packages:

``` r
install.packages(c(
  "rvest", "httr2", "jsonlite", "xml2",
  "tidyverse", "lubridate", "sf",
  "ggplot2", "patchwork", "scales", "robotstxt",
  "broom", "lmtest", "sandwich", "forecast", "zoo",
  "kableExtra", "ggcorrplot", "rmarkdown"
))
```

### 2. Clone and run

``` bash
git clone https://github.com/YOUR_USERNAME/spain-immigration-project.git
cd spain-immigration-project
Rscript main.R
# Then render the HTML report:
Rscript -e "rmarkdown::render('report.Rmd')"
```

The pipeline runs six steps:

1.  **La Moncloa** ,Use Wayback Machine CDX API to discover press release URLs from immigration-related ministries (interior, inclusion, exteriores); fetch live La Moncloa pages for slug-keyword matches; extract titles and body text
2.  **Wikipedia polls** ,Scrape the Spanish opinion polling table (4 year-tables, party columns identified via `img alt` attributes)
3.  **INE API** ,Fetch EPA unemployment by province (Table 65349) and foreign population trend via confirmed series codes
4.  **El País** ,Paginate immigration topic section (`/noticias/inmigracion/N/`) to count monthly immigration article volumes
5.  **Visualisations** ,Generate all plots in `output/plots/`
6.  **Statistical analysis** ,Build master panel, run OLS + Newey-West regression, ARIMA model, and produce a 2027 PSOE electoral forecast under three scenarios

### 3. Outputs

| File | Description |
|-----------------------|-------------------------------------------------|
| `data/raw/lamoncloa_raw.csv` | All CDX-discovered press release URLs with parsed metadata |
| `data/raw/polls_raw.csv` | All opinion poll rows from Wikipedia |
| `data/raw/ine_demographics_raw.csv` | INE foreign population + unemployment by province |
| `data/raw/elpais_headlines_raw.csv` | El País headlines with immigration flag |
| `data/processed/immigration_statements.csv` | Confirmed immigration press releases with full text |
| `data/processed/monthly_statements.csv` | Monthly count of government immigration statements |
| `data/processed/monthly_polls.csv` | Monthly PSOE/PP/Vox/Sumar poll averages |
| `data/processed/salience_scores.csv` | Monthly El País immigration media salience score |
| `data/processed/master_panel.csv` | Merged monthly panel with all variables (28 months) |
| `data/processed/descriptive_stats.csv` | Summary statistics table |
| `data/processed/correlation_matrix.csv` | Pearson correlation matrix (long format) |
| `data/processed/crosscorr_statements_psoe.csv` | Cross-correlation: statements vs. PSOE change |
| `data/processed/model_results.csv` | OLS regression coefficient table (Newey-West SEs) |
| `data/processed/forecast_2027.csv` | PSOE forecast to June 2027 (three scenarios + 95% CI) |
| `output/plots/01_speech_vs_polls_timeline.png` | Line graph: PSOE popularity + speech markers |
| `output/plots/02_reaction_map.png` | Provincial chart of poll shifts |
| `output/plots/03_salience_barchart.png` | Immigration media salience by month |
| `report.html` | Full HTML report (rendered from `report.Rmd`) |

## Ethical Compliance

-   A standard browser `User-Agent` is sent to all requests (La Moncloa blocks annotated UAs; project contact is documented in code comments)
-   `Sys.sleep()` delays (3–5 seconds) between page fetches ,0.5–1.5 s between CDX API calls
-   `robots.txt` is checked before scraping La Moncloa
-   Wikipedia is fetched once (bulk table extraction, no repeated crawling)
-   Wayback Machine CDX API is used read-only for URL discovery (no archive content fetched)

## Notes

-   **La Moncloa**: The main index page (`index.aspx?Anyo=&Mes=`) uses client-side JavaScript filtering that is invisible to HTTP scraping. The Wayback Machine CDX API is used to discover press release URLs from key immigration ministries, which are then fetched directly from the live La Moncloa server.
-   **INE API**: Data is fetched via the official public JSON API (`servicios.ine.es/wstempus/js/ES`). EPA unemployment: Table 65349; Foreign population: series CP305495 (total) and CP305423 (Spanish) ,foreign = total − Spanish.
-   No API tokens are required for this project.
