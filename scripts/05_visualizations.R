# =============================================================================
# 05_visualizations.R
# Produce three main visualisations:
#   1. Speech vs Polls Timeline — line graph of PSOE popularity with
#      immigration speech markers
#   2. Reaction Map — provincial map showing poll shifts after key events
#   3. Immigration Media Salience — monthly bar chart
# =============================================================================

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)
library(sf)

# Attempt to load leaflet for interactive map (not required)
has_leaflet <- requireNamespace("leaflet", quietly = TRUE)

# --- Load processed data ------------------------------------------------------

cat("  Loading processed data...\n")

# Monthly polls
polls <- tryCatch(
  read_csv("data/processed/monthly_polls.csv", show_col_types = FALSE),
  error = function(e) {
    message("  No polls data found: ", e$message)
    tibble(year_month = as.Date(character()), psoe_avg = numeric())
  }
)

# Immigration statements
statements <- tryCatch(
  read_csv("data/processed/immigration_statements.csv", show_col_types = FALSE),
  error = function(e) {
    message("  No statements data found: ", e$message)
    tibble(date = as.Date(character()), title = character())
  }
)

# Media salience
salience <- tryCatch(
  read_csv("data/processed/salience_scores.csv", show_col_types = FALSE),
  error = function(e) {
    message("  No salience data found: ", e$message)
    tibble(year_month = as.Date(character()), salience_score = numeric())
  }
)

# Demographics
demographics <- tryCatch(
  read_csv("data/raw/ine_demographics_raw.csv", show_col_types = FALSE),
  error = function(e) {
    message("  No demographics data found: ", e$message)
    tibble()
  }
)

cat("  Polls rows:", nrow(polls),
    "| Statements:", nrow(statements),
    "| Salience months:", nrow(salience), "\n")

# --- Common theme -------------------------------------------------------------

theme_project <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(colour = "grey40", size = 10, hjust = 0),
      plot.caption  = element_text(colour = "grey60", size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

# Okabe-Ito colorblind-safe palette (distinguishable under protanopia/deuteranopia)
PSOE_COLOR  <- "#D55E00"   # vermillion (warm red-orange)
PP_COLOR    <- "#0072B2"   # blue
VOX_COLOR   <- "#009E73"   # bluish green
ACCENT      <- "#E69F00"   # amber

# ==============================================================================
# PLOT 1: Speech vs Polls Timeline
# ==============================================================================

cat("  Building Plot 1: Speech vs Polls Timeline...\n")

if (nrow(polls) > 0 && "year_month" %in% colnames(polls)) {

  # Mark months with immigration speeches
  speech_months <- statements |>
    filter(!is.na(date)) |>
    mutate(year_month = floor_date(as.Date(date), "month")) |>
    group_by(year_month) |>
    summarise(n_speeches = n(), .groups = "drop")

  polls_annotated <- polls |>
    left_join(speech_months, by = "year_month") |>
    mutate(
      n_speeches  = replace_na(n_speeches, 0),
      has_speech  = n_speeches > 0
    )

  # Key events to label
  key_events <- tibble(
    date  = as.Date(c("2023-11-17", "2026-01-08")),
    label = c("Sánchez re-elected\nPM (Nov 2023)",
              "Regularisation\nDecree (Jan 2026)"),
    psoe  = c(28, 28)
  ) |>
    filter(date >= min(polls_annotated$year_month, na.rm = TRUE),
           date <= max(polls_annotated$year_month, na.rm = TRUE))

  p1 <- ggplot(polls_annotated, aes(x = year_month)) +

    # Shade areas where immigration speeches were made
    geom_rect(
      data = polls_annotated |>
        filter(has_speech) |>
        mutate(xmax = year_month %m+% months(1)),
      aes(xmin = year_month, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = ACCENT, alpha = 0.15,
      inherit.aes = FALSE
    ) +

    # PP line (for comparison)
    geom_line(aes(y = pp_avg,   colour = "PP"),   linewidth = 0.8, linetype = "dashed",
              na.rm = TRUE) +

    # Vox line
    geom_line(aes(y = vox_avg,  colour = "Vox"),  linewidth = 0.8, linetype = "dashed",
              na.rm = TRUE) +

    # PSOE main line
    geom_line(aes(y = psoe_avg, colour = "PSOE"), linewidth = 1.3, na.rm = TRUE) +
    geom_point(aes(y = psoe_avg, size = n_speeches, colour = "PSOE"),
               alpha = 0.7, na.rm = TRUE) +

    # Key event labels
    {if (nrow(key_events) > 0)
      geom_vline(data = key_events, aes(xintercept = date),
                 linetype = "dotted", colour = "grey40")
    } +
    {if (nrow(key_events) > 0)
      geom_label(data = key_events, aes(x = date, y = psoe, label = label),
                 size = 2.8, hjust = 0.5, vjust = 1, fill = "white",
                 label.size = 0.2, inherit.aes = FALSE)
    } +

    scale_colour_manual(
      values  = c("PSOE" = PSOE_COLOR, "PP" = PP_COLOR, "Vox" = VOX_COLOR),
      name    = "Party"
    ) +
    scale_size_continuous(name = "Immigration speeches", range = c(1.5, 5)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(5, 45)) +
    labs(
      title    = "PSOE Polling Intention vs. Official Immigration Statements",
      subtitle = paste0(
        "Orange shading = months with ≥1 official immigration statement from La Moncloa\n",
        "Dot size proportional to number of statements that month"
      ),
      x       = NULL,
      y       = "Voting intention (%)",
      caption = "Sources: Wikipedia opinion polls; La Moncloa press releases"
    ) +
    theme_project() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("output/plots/01_speech_vs_polls_timeline.png",
         p1, width = 14, height = 7, dpi = 300)
  cat("    Saved: output/plots/01_speech_vs_polls_timeline.png\n")

} else {
  cat("    SKIP: insufficient polls data for Plot 1\n")

  # Create placeholder
  p1 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Polls data not yet available.\nRun scripts 01 and 02 first.",
             size = 6, colour = "grey50") +
    labs(title = "Speech vs Polls Timeline") +
    theme_void()
  ggsave("output/plots/01_speech_vs_polls_timeline.png",
         p1, width = 12, height = 6, dpi = 300)
}

# ==============================================================================
# PLOT 2: Reaction Map — provincial poll shifts
# ==============================================================================

cat("  Building Plot 2: Reaction Map...\n")

# We approximate regional polling shift using demographics as a proxy:
# provinces with high foreign population may react differently to immigration rhetoric.
# If regional poll data is unavailable we visualise foreign population distribution.

if (nrow(demographics) > 0 && !"note" %in% colnames(demographics)) {

  # Try to load Spain province shapefile
  # Use rnaturalearth or a bundled geojson if available
  has_rne  <- requireNamespace("rnaturalearth",     quietly = TRUE)
  has_rneh <- requireNamespace("rnaturalearthdata", quietly = TRUE)
  # Also check rnaturalearthhires which is needed for state-level detail
  has_rnehires <- requireNamespace("rnaturalearthhires", quietly = TRUE)

  if (has_rne && has_rneh && has_rnehires) {
    library(rnaturalearth)
    spain_map <- ne_states(country = "spain", returnclass = "sf")

    # Normalise province name for join
    normalise_prov <- function(x) {
      x |> str_to_lower() |>
        str_replace_all("[áàä]", "a") |> str_replace_all("[éèë]", "e") |>
        str_replace_all("[íìï]", "i") |> str_replace_all("[óòö]", "o") |>
        str_replace_all("[úùü]", "u") |> str_replace_all("ñ", "n") |>
        str_squish()
    }

    spain_map <- spain_map |>
      mutate(province_key = normalise_prov(name))

    latest_demog <- demographics |>
      filter(!is.na(foreign_pop)) |>
      group_by(province_key) |>
      slice_max(year, n = 1) |>
      ungroup()

    map_data <- spain_map |>
      left_join(latest_demog, by = "province_key")

    p2 <- ggplot(map_data) +
      geom_sf(aes(fill = foreign_pop / 1000), colour = "white", linewidth = 0.3) +
      scale_fill_gradientn(
        colours  = c("#f7f7f7", "#fdae6b", "#e6550d", "#a63603"),
        name     = "Foreign-born\npopulation (thousands)",
        na.value = "grey80",
        labels   = label_comma()
      ) +
      labs(
        title    = "Foreign-Born Population by Province (Latest Available Year)",
        subtitle = "Provinces with higher foreign-born populations are hypothesised to\nreact more strongly to official immigration discourse",
        caption  = "Source: INE Padrón Municipal"
      ) +
      theme_void() +
      theme(
        plot.title    = element_text(face = "bold", size = 13, hjust = 0),
        plot.subtitle = element_text(colour = "grey40", size = 9, hjust = 0),
        plot.caption  = element_text(colour = "grey60", size = 8, hjust = 1),
        legend.position = "right"
      )

    ggsave("output/plots/02_reaction_map.png",
           p2, width = 10, height = 9, dpi = 300)
    cat("    Saved: output/plots/02_reaction_map.png\n")

  } else {
    cat("    INFO: rnaturalearth not installed — creating provincial chart fallback\n")

    # Border/immigration-relevant provinces for highlighting
    border_provs <- c("Ceuta", "Melilla", "Las Palmas", "Santa Cruz de Tenerife",
                       "Cádiz", "Almería", "Málaga", "Huelva")

    top_provinces <- demographics |>
      filter(indicator == "unemployment_rate" | is.na(indicator)) |>
      filter(!is.na(value)) |>
      group_by(province) |>
      slice_max(year, n = 1) |>
      ungroup() |>
      filter(!is.na(province)) |>
      slice_max(value, n = 20) |>
      mutate(
        is_border = province %in% border_provs | str_detect(province, "Ceuta|Melilla|Las Palmas|Tenerife|Cádiz|Almería|Málaga|Huelva"),
        prov_label = ifelse(is_border, paste0(province, " *"), province)
      )

    natl_avg <- demographics |>
      filter(indicator == "unemployment_rate" | is.na(indicator)) |>
      filter(!is.na(value)) |>
      group_by(province) |>
      slice_max(year, n = 1) |>
      ungroup() |>
      pull(value) |>
      mean(na.rm = TRUE)

    p2 <- ggplot(top_provinces,
                 aes(x = reorder(prov_label, value),
                     y = value,
                     fill = is_border)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = natl_avg, linetype = "dashed",
                 colour = "#555555", linewidth = 0.6) +
      annotate("text", x = 1.5, y = natl_avg + 0.5,
               label = paste0("National avg: ", round(natl_avg, 1), "%"),
               colour = "#555555", size = 3, hjust = 0) +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#fdae6b", "TRUE" = "#a63603")) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title    = "Provincial Unemployment: Immigration Gateway Provinces",
        subtitle = "Starred (*) provinces are border/coastal entry points — economic vulnerability\nmay amplify receptivity to anti-immigration rhetoric",
        x        = NULL,
        y        = "Unemployment rate (%)",
        caption  = "Source: INE EPA (Table 65349) · * = border or maritime immigration entry provinces"
      ) +
      theme_project()

    ggsave("output/plots/02_reaction_map.png",
           p2, width = 10, height = 7, dpi = 300)
    cat("    Saved: output/plots/02_reaction_map.png (provincial chart)\n")
  }

} else {
  cat("    SKIP: no demographics data for Plot 2\n")
  p2 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Demographics data not available.\nRun script 03 first.",
             size = 6, colour = "grey50") +
    theme_void()
  ggsave("output/plots/02_reaction_map.png", p2, width = 10, height = 9, dpi = 300)
}

# ==============================================================================
# PLOT 3: Immigration Media Salience Bar Chart
# ==============================================================================

cat("  Building Plot 3: Media Salience Barchart...\n")

if (nrow(salience) > 0 && "salience_score" %in% colnames(salience) &&
    "year_month" %in% colnames(salience)) {

  # Use normalised immigration volume if salience_score is all 100% (España pagination incomplete)
  salience <- salience |>
    mutate(year_month = as.Date(year_month))

  # Compute normalised volume if missing from CSV
  if (!"imm_volume_norm" %in% colnames(salience) && "n_immigration" %in% colnames(salience)) {
    salience <- salience |>
      mutate(imm_volume_norm = round(n_immigration / max(n_immigration, na.rm = TRUE) * 100, 1))
    cat("    NOTE: Computed imm_volume_norm from n_immigration (column was missing from CSV)\n")
  }

  use_col <- if ("imm_volume_norm" %in% colnames(salience) &&
                 n_distinct(salience$imm_volume_norm, na.rm = TRUE) > 1) {
    "imm_volume_norm"
  } else if (n_distinct(salience$salience_score, na.rm = TRUE) > 1) {
    "salience_score"
  } else {
    "n_immigration"
  }

  salience_plot <- salience |>
    filter(!is.na(.data[[use_col]])) |>
    mutate(
      plot_value  = .data[[use_col]],
      label_month = format(year_month, "%b\n%Y"),
      highlight   = plot_value >= quantile(plot_value, 0.75, na.rm = TRUE)
    )

  # Add statement count overlay if available
  if (nrow(statements) > 0) {
    stmt_monthly <- statements |>
      filter(!is.na(date)) |>
      mutate(year_month = floor_date(as.Date(date), "month")) |>
      count(year_month, name = "n_statements")

    salience_plot <- salience_plot |>
      left_join(stmt_monthly, by = "year_month") |>
      mutate(n_statements = replace_na(n_statements, 0))
  } else {
    salience_plot <- salience_plot |> mutate(n_statements = 0)
  }

  y_label <- switch(use_col,
    "imm_volume_norm" = "Immigration news volume (normalised, 0–100)",
    "salience_score"  = "Immigration salience (%)",
    "n_immigration"   = "Immigration articles (count)"
  )

  p3 <- ggplot(salience_plot,
               aes(x = year_month, y = plot_value, fill = highlight)) +
    geom_col(width = 25, show.legend = FALSE) +
    geom_hline(
      yintercept = quantile(salience_plot$plot_value, 0.75, na.rm = TRUE),
      linetype = "dashed", colour = "grey50", linewidth = 0.6
    ) +
    annotate("text",
             x     = min(salience_plot$year_month) + 30,
             y     = quantile(salience_plot$plot_value, 0.75, na.rm = TRUE) * 1.04,
             label = "75th percentile",
             colour = "grey50", size = 3, hjust = 0) +

    # Overlay statement counts as points (if available)
    {if ("n_statements" %in% colnames(salience_plot) && any(salience_plot$n_statements > 0, na.rm = TRUE)) {
      max_val   <- max(salience_plot$plot_value, na.rm = TRUE)
      max_stmts <- max(salience_plot$n_statements, na.rm = TRUE)
      geom_point(aes(y = n_statements * max_val / max_stmts * 0.9),
                 colour = PSOE_COLOR, size = 2.5, shape = 18)
    }} +

    scale_fill_manual(values = c("FALSE" = "#A8DADC", "TRUE" = ACCENT),
                      labels = c("FALSE" = "Normal", "TRUE" = "Peak month")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(name = y_label, expand = expansion(mult = c(0, 0.08))) +
    labs(
      title    = "Immigration Media Volume — Monthly El País Coverage",
      subtitle = paste0(
        "Orange bars = months in top 25% for immigration news volume (El País)\n",
        "Source: El País /noticias/inmigracion/ — ", nrow(salience_plot), " months scraped"
      ),
      x       = NULL,
      caption = "Source: El País immigration topic section (elpais.com/noticias/inmigracion/)"
    ) +
    theme_project() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("output/plots/03_salience_barchart.png",
         p3, width = 14, height = 6, dpi = 300)
  cat("    Saved: output/plots/03_salience_barchart.png\n")

} else {
  cat("    SKIP: insufficient salience data for Plot 3\n")
  p3 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Salience data not available.\nRun script 04 first.",
             size = 6, colour = "grey50") +
    theme_void()
  ggsave("output/plots/03_salience_barchart.png",
         p3, width = 14, height = 6, dpi = 300)
}

# ==============================================================================
# PLOT 4: Salience vs PSOE Scatter (correlation visualisation)
# ==============================================================================

cat("  Building Plot 4: Salience vs PSOE scatter...\n")

master_panel_path <- "data/processed/master_panel.csv"
if (file.exists(master_panel_path)) {
  panel_data <- read_csv(master_panel_path, show_col_types = FALSE) |>
    mutate(year_month = as.Date(year_month))

  # Colour-code by period
  panel_col <- panel_data |>
    filter(!is.na(psoe_avg), !is.na(imm_volume_norm)) |>
    mutate(
      period = case_when(
        year_month < as.Date("2024-07-01") ~ "Early (Nov 23\u2013Jun 24)",
        year_month < as.Date("2025-07-01") ~ "Mid (Jul 24\u2013Jun 25)",
        TRUE                               ~ "Late (Jul 25\u2013Feb 26)"
      ),
      period = factor(period, levels = c(
        "Early (Nov 23\u2013Jun 24)",
        "Mid (Jul 24\u2013Jun 25)",
        "Late (Jul 25\u2013Feb 26)"
      ))
    )

  r_val <- cor(panel_col$imm_volume_norm, panel_col$psoe_avg, use = "complete.obs")

  p4 <- ggplot(panel_col, aes(x = imm_volume_norm, y = psoe_avg)) +
    geom_point(aes(colour = period), size = 3.5, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE,
                colour = ACCENT, fill = ACCENT, alpha = 0.12,
                linewidth = 0.9) +
    scale_colour_manual(values = c("#56B4E9", "#332288", PSOE_COLOR)) +
    scale_x_continuous(labels = function(x) paste0(x)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Immigration Media Salience vs PSOE Voting Intention",
      subtitle = paste0("Each point = one month (N = ", nrow(panel_col),
                        ")   \u00b7   Pearson r = ", round(r_val, 3)),
      x        = "Immigration salience (El Pa\u00eds, normalised 0\u2013100)",
      y        = "PSOE vote intention (%)",
      colour   = NULL,
      caption  = "Sources: El Pa\u00eds immigration articles; Wikipedia opinion polls"
    ) +
    theme_project() +
    theme(legend.position = "bottom")

  ggsave("output/plots/04_salience_scatter.png",
         p4, width = 9, height = 7, dpi = 300)
  cat("    Saved: output/plots/04_salience_scatter.png\n")
} else {
  cat("    SKIP: master_panel.csv not found — run script 06 first\n")
}

# ==============================================================================
# BONUS: Combined summary panel (patchwork)
# ==============================================================================

cat("  Building combined summary panel...\n")

if (exists("p1") && exists("p3")) {
  combined <- p1 / p3 +
    plot_annotation(
      title   = "Spanish Government Immigration Rhetoric & Public Opinion (2023–2026)",
      caption = "UC3M Data Harvesting Project — Team: Maimaitiming, Varberg Sabri, Tadmuri",
      theme   = theme(plot.title = element_text(face = "bold", size = 15))
    )
  ggsave("output/plots/00_combined_summary.png",
         combined, width = 14, height = 12, dpi = 300)
  cat("    Saved: output/plots/00_combined_summary.png\n")
}

cat("\n  All visualisations complete.\n")
