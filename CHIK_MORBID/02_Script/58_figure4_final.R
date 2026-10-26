# ---------------------------------------------------------------------------
# 58_figure4_final.R
#
# Final Main Figure 4: within-count standardised risk differences (RD) for
# hospitalisation and chikungunya-attributed death, as one integrated
# forest plot with a single shared comparison-label column.
#
# Estimates are read from 56_within_count_contrasts.R output - nothing is
# refitted or altered for display. Also writes the title/legend Word file,
# the plotted-data CSV and a value check against the figure's published
# numbers.
#
# Output: 03_Output/figure_revision/figure4_final/
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(officer)
})

source("02_Script/figure_revision_theme.R")

src_dir <- file.path(fig_rev_dir, "within_count")
out_dir <- file.path(fig_rev_dir, "figure4_final")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

con <- readr::read_csv(file.path(src_dir, "within_count_direct_contrasts.csv"), show_col_types = FALSE)
risks <- readr::read_csv(file.path(src_dir, "within_count_standardised_risks.csv"), show_col_types = FALSE,
                         col_types = readr::cols(level = "c", n_conditions = "c"))

## ============================================================
## 1. Checks
## ============================================================
## Pre-specified rows (top to bottom) and their y positions. Group headings
## take their own rows; the gap before "Two conditions" separates groups.
rows <- tibble::tribble(
  ~profile_a, ~profile_b, ~group,           ~y,
  NA,         NA,         "One condition",  8,
  "DM",       "HTN",      "One condition",  7,
  "CKD",      "HTN",      "One condition",  6,
  "CKD",      "DM",       "One condition",  5,
  NA,         NA,         "Two conditions", 3.4,
  "DM+CKD",   "DM+HTN",   "Two conditions", 2.4,
  "HTN+CKD",  "DM+HTN",   "Two conditions", 1.4,
  "HTN+CKD",  "DM+CKD",   "Two conditions", 0.4
)
contrast_rows <- dplyr::filter(rows, !is.na(profile_a))

d <- con |>
  dplyr::inner_join(contrast_rows, by = c("profile_a", "profile_b", "group"))
stopifnot(nrow(d) == 12, all(table(d$outcome) == 6))

## RD must equal risk(first) - risk(second) from the standardised risks.
rk <- risks |> dplyr::filter(model == "profile") |> dplyr::select(outcome, level, std_risk_pct)
chk_rd <- d |>
  dplyr::left_join(dplyr::rename(rk, profile_a = level, risk_a_chk = std_risk_pct), by = c("outcome", "profile_a")) |>
  dplyr::left_join(dplyr::rename(rk, profile_b = level, risk_b_chk = std_risk_pct), by = c("outcome", "profile_b"))
stopifnot(max(abs(chk_rd$rd_pp - (chk_rd$risk_a_chk - chk_rd$risk_b_chk))) < 1e-10)

## Sparse rule (as coded in script 56): either profile has <10 outcome events.
stopifnot(identical(d$sparse, pmin(d$n_events_a, d$n_events_b) < 10))

## Values transcribed from the previous Candidate B figure, checked against
## the source CSV at the displayed precision.
transcribed <- tibble::tribble(
  ~outcome, ~profile_a, ~profile_b, ~shown,
  "hosp",  "DM",      "HTN",    "0.6 (0.2 to 1.1)",
  "hosp",  "CKD",     "HTN",    "4.1 (2.8 to 5.7)",
  "hosp",  "CKD",     "DM",     "3.4 (2.1 to 5.0)",
  "hosp",  "DM+CKD",  "DM+HTN", "4.7 (1.5 to 8.8)",
  "hosp",  "HTN+CKD", "DM+HTN", "7.3 (5.3 to 9.5)",
  "hosp",  "HTN+CKD", "DM+CKD", "2.6 (-2.0 to 6.6)",
  "death", "DM",      "HTN",    "0.09 (0.02 to 0.17)",
  "death", "CKD",     "HTN",    "0.37 (0.14 to 0.80)",
  "death", "CKD",     "DM",     "0.28 (0.02 to 0.71)",
  "death", "DM+CKD",  "DM+HTN", "0.33 (-0.09 to 1.36)",
  "death", "HTN+CKD", "DM+HTN", "0.51 (0.22 to 0.97)",
  "death", "HTN+CKD", "DM+CKD", "0.18 (-0.90 to 0.81)"
)
dp <- function(o) ifelse(o == "death", 2, 1)
fmt <- function(x, k) mapply(function(v, kk) formatC(v, format = "f", digits = kk), x, k)
d <- d |>
  dplyr::mutate(k = dp(outcome),
                rd_text_ascii = paste0(fmt(rd_pp, k), " (", fmt(rd_lower_pp, k), " to ", fmt(rd_upper_pp, k), ")"),
                rd_text = gsub("-", "−", rd_text_ascii))
verify <- transcribed |>
  dplyr::left_join(dplyr::select(d, outcome, profile_a, profile_b, rd_text_ascii),
                   by = c("outcome", "profile_a", "profile_b")) |>
  dplyr::mutate(match = shown == rd_text_ascii)
print(verify)
readr::write_csv(verify, file.path(out_dir, "Figure4_value_check_vs_previous_figure.csv"))
stopifnot(all(verify$match))

## ============================================================
## 2. Plot pieces (shared numeric y grid -> exact row alignment)
## ============================================================
y_lim <- c(-0.25, 8.6)
blue <- "#1F4E89"
grey_sparse <- "#9A9A9A"
lab <- function(a, b) paste0(gsub("+", " + ", a, fixed = TRUE), " vs ", gsub("+", " + ", b, fixed = TRUE))
heading_rows <- dplyr::filter(rows, is.na(profile_a))
label_rows <- contrast_rows |> dplyr::mutate(label = lab(profile_a, profile_b))

base_theme <- function() {
  ggplot2::theme_void(base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 8.6, colour = ink, hjust = 0,
                                         margin = ggplot2::margin(b = 4)),
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

## Column 1: shared comparison labels and horizontal group headings.
p_labels <- ggplot2::ggplot() +
  ggplot2::geom_text(data = heading_rows, ggplot2::aes(x = 0, y = y, label = group),
                     hjust = 0, size = 2.75, fontface = "bold", colour = ink_soft, family = "Arial") +
  ggplot2::geom_text(data = label_rows, ggplot2::aes(x = 0.06, y = y, label = label),
                     hjust = 0, size = 2.75, colour = ink, family = "Arial") +
  ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
  ggplot2::labs(title = "Profile comparison") +
  base_theme() +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 6, 6))

## Columns 2-3 / 4-5: forest + RD (95% CI) column drawn in the right margin
## (outside the plotting area) at x = text_x, with clipping off.
outcome_panel <- function(o, title, limits, breaks, text_x) {
  pd <- dplyr::filter(d, outcome == o) |>
    dplyr::mutate(col = ifelse(sparse, grey_sparse, blue), fill = ifelse(sparse, "white", blue))
  stopifnot(min(pd$rd_lower_pp) >= limits[1], max(pd$rd_upper_pp) <= limits[2])
  ggplot2::ggplot(pd, ggplot2::aes(y = y)) +
    ggplot2::geom_vline(xintercept = breaks, linewidth = 0.2, colour = "#EBEBEB") +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "#7A7A7A") +
    ggplot2::geom_segment(ggplot2::aes(x = rd_lower_pp, xend = rd_upper_pp, yend = y, colour = col),
                          linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = rd_pp, colour = col, fill = fill), shape = 21, size = 1.9, stroke = 0.6) +
    ggplot2::geom_text(ggplot2::aes(x = text_x, label = rd_text, colour = col), hjust = 0, size = 2.6,
                       family = "Arial") +
    ggplot2::annotate("text", x = text_x, y = heading_rows$y[1], label = "RD (95% CI)", hjust = 0,
                      size = 2.6, fontface = "bold", colour = ink_soft, family = "Arial") +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ## Window set in coord_cartesian (not scale limits) so the RD column,
    ## placed beyond the axis range, is drawn in the margin, not dropped.
    ggplot2::scale_x_continuous(breaks = breaks,
                                labels = function(x) gsub("-", "−", format(x, trim = TRUE, drop0trailing = TRUE))) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = limits, ylim = y_lim, expand = FALSE, clip = "off") +
    ggplot2::labs(title = title, x = "Standardised risk difference\n(percentage points)", y = NULL) +
    base_theme() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      axis.ticks.length.x = ggplot2::unit(1.2, "mm"),
      axis.text.x = ggplot2::element_text(size = 7, colour = "#333333", margin = ggplot2::margin(t = 2)),
      axis.title.x = ggplot2::element_text(size = 7.6, colour = ink, margin = ggplot2::margin(t = 4),
                                           lineheight = 1.05),
      ## Right margin reserves the RD (95% CI) column.
      plot.margin = ggplot2::margin(6, 85, 6, 4)
    )
}

p_hosp <- outcome_panel("hosp", "Hospitalisation", limits = c(-4, 10), breaks = seq(-4, 10, 2), text_x = 10.8)
p_death <- outcome_panel("death", "Chikungunya-attributed death", limits = c(-1, 1.5),
                         breaks = seq(-1, 1.5, 0.5), text_x = 1.64)

fig4 <- (p_labels | p_hosp | p_death) + patchwork::plot_layout(widths = c(0.62, 1, 1))
save_figure(fig4, "Figure4_within_count_RD_FINAL", width = 250, height = 84, dir = out_dir)
## Low-resolution proof (screen/journal-display check of legibility).
ggplot2::ggsave(file.path(out_dir, "Figure4_within_count_RD_FINAL_lowres_proof.png"), fig4,
                width = 250, height = 84, units = "mm", dpi = 180, bg = "white", device = ragg::agg_png)

## ============================================================
## 3. Plotted data
## ============================================================
plotted <- d |>
  dplyr::transmute(outcome = ifelse(outcome == "hosp", "Hospitalisation", "Chikungunya-attributed death"),
                   group, comparison = lab(profile_a, profile_b), profile_a, profile_b,
                   risk_a_pct, risk_b_pct, rd_pp, rd_lower_pp, rd_upper_pp,
                   rd_lower_simult_pp, rd_upper_simult_pp, rr, rr_lower, rr_upper,
                   n_cases_a, n_events_a, n_cases_b, n_events_b,
                   sparse_either_profile_lt10_events = sparse, displayed_text = rd_text_ascii,
                   interval = "pointwise 95% (simultaneous Bonferroni 99.17% in rd_*_simult columns)")
readr::write_csv(plotted, file.path(out_dir, "Figure4_within_count_RD_data.csv"))

## ============================================================
## 4. Title and legend (Word)
## ============================================================
n_h <- 776072; e_h <- 25363; n_d <- 983947; e_d <- 1050
title <- paste0("Figure 4. Within-count differences in standardised risks of hospitalisation and ",
                "chikungunya-attributed death across comorbidity profiles")
legend <- paste0(
  "Points show standardised risk differences (RD) between pairs of comorbidity profiles with the same number ",
  "of conditions, among confirmed chikungunya cases notified in Brazil in 2017–2025, for hospitalisation ",
  "(n = 776,072; 25,363 hospitalisations) and chikungunya-attributed death (n = 983,947; 1,050 deaths). ",
  "Six contrasts were pre-specified: among single-condition profiles, DM vs HTN, CKD vs HTN and CKD vs DM; ",
  "among two-condition profiles, DM+CKD vs DM+HTN, HTN+CKD vs DM+HTN and HTN+CKD vs DM+CKD. Profiles with no ",
  "condition or all three conditions are not compared because each is the only profile with that count. ",
  "RD is the standardised risk of the first-listed profile minus that of the second-listed profile, in ",
  "percentage points; positive values indicate higher risk in the first-listed profile. Risks were estimated ",
  "from penalised logistic generalised additive models for each outcome including the eight mutually exclusive ",
  "comorbidity profiles, a smooth function of age, sex, notification year and state of residence (random ",
  "effect), and standardised by g-computation to the observed joint distribution of age, sex, notification ",
  "year and state of residence in that outcome's cohort. Horizontal bars are pointwise 95% confidence ",
  "intervals from 4,000 draws of the model coefficients, with both risks in each contrast computed from the ",
  "same draw; they are not adjusted for multiple comparisons (multiplicity-adjusted intervals are given in ",
  "Supplementary Table S[X]). Open grey circles mark contrasts involving a profile with fewer than 10 outcome ",
  "events: both mortality contrasts involving DM+CKD (4 deaths among 313 cases), which are imprecise and ",
  "should not be interpreted as evidence of a difference or of no difference. ",
  "DM, diabetes mellitus; HTN, hypertension; CKD, chronic kidney disease."
)
note <- paste0(
  "Figure 2 compares each profile with cases who have none of the three conditions. Figure 4 instead compares ",
  "profiles that share the same number of conditions, directly and on the absolute scale, with uncertainty for ",
  "each difference estimated jointly. These within-count differences cannot be read reliably from Figure 2: ",
  "comparing two profiles' separate confidence intervals ignores their covariance. Figure 4 therefore tests ",
  "whether which conditions are present carries information beyond how many are present."
)

body_text <- officer::fp_text(font.family = "Arial", font.size = 11)
bold_text <- officer::fp_text(font.family = "Arial", font.size = 11, bold = TRUE)
para <- officer::fp_par(text.align = "justify", line_spacing = 1.5, padding.bottom = 10)
doc <- officer::read_docx() |>
  officer::body_add_fpar(officer::fpar(officer::ftext(title, bold_text), fp_p = para)) |>
  officer::body_add_fpar(officer::fpar(officer::ftext(legend, body_text), fp_p = para)) |>
  officer::body_add_fpar(officer::fpar(officer::ftext(
    "Figure note (for authors and reviewers; not part of the legend)", bold_text), fp_p = para)) |>
  officer::body_add_fpar(officer::fpar(officer::ftext(note, body_text), fp_p = para))
doc <- officer::cursor_begin(doc)
if (nchar(officer::docx_summary(doc)$text[1]) == 0) doc <- officer::body_remove(doc)
print(doc, target = file.path(out_dir, "Figure4_Title_and_Legend.docx"))
message("[58] legend words: ", length(strsplit(legend, "\\s+")[[1]]))
message("[58] Figure 4 written to ", out_dir)
