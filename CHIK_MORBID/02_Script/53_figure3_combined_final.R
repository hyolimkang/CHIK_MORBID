# ---------------------------------------------------------------------------
# 53_figure3_combined_final.R
#
# Final combined Figure 3: age-specific standardised absolute risks of
# hospitalisation (A, B) and chikungunya-attributed death (C, D) by
# multimorbidity profile, plus supplementary age figures for both outcomes.
#
# Inputs (no model fitting here):
#   hospitalisation: 03_Output/figure_revision/figure3_final/ (script 52)
#   mortality:       03_Output/validation/figure5_age_validation/ (script 46),
#                    reproduced unchanged and checked against the current
#                    03_Output/figure_revision/figure3_plotted_values.csv
#
# Outputs (03_Output/figure_revision/figure3_final/):
#   Figure3_combined_absolute_risks_FINAL.pdf/.png/.svg
#   Figure3_candidate1_rows_by_outcome.png, Figure3_candidate2_columns_by_outcome.png
#   Figure3_combined_absolute_risks_data.csv, Figure3_combined_support_table.csv
#   hospital_age_profile_standardised_risks.csv / _RRs.csv / _risk_differences.csv
#   hospital_age_profile_support_heatmap.pdf/.png and Supp_*.pdf/.png
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

out_dir <- file.path(fig_rev_dir, "figure3_final")
mort_dir <- "03_Output/validation/figure5_age_validation"
min_events <- 10

## Hospitalisation model used for the main estimates. Chosen in
## FIGURE3_FINAL_VALIDATION.md section 3 from hospital_age_model_comparison.csv,
## hospital_age_model_robustness*.csv and the curve support audit.
selected_hosp_model <- "H2"

## Panel A / C profiles: the mortality panel's adequately supported profiles
## (None, HTN, DM+HTN) in both outcomes, so the two age panels can be read
## against each other; CKD is added for hospitalisation because it is
## adequately supported (>=10 events) in every age group there and is the
## profile that most departs from its condition count (Figure 4).
panel_a_profiles <- c("None", "HTN", "DM+HTN", "CKD")
panel_c_profiles <- c("None", "HTN", "DM+HTN")

## ============================================================
## 1. Data
## ============================================================
h_all <- readr::read_csv(file.path(out_dir, "hospital_age_profile_all_model_estimates.csv"),
                         show_col_types = FALSE)
h_sup <- readr::read_csv(file.path(out_dir, "hospital_age_profile_support.csv"), show_col_types = FALSE)
m_est <- readr::read_csv(file.path(mort_dir, "age_profile_standardised_estimates.csv"), show_col_types = FALSE)
m_sup <- readr::read_csv(file.path(mort_dir, "age_profile_support.csv"), show_col_types = FALSE) |>
  dplyr::filter(age_scheme == "primary")

stopifnot(sum(h_sup$n_cases) == 776072, sum(h_sup$n_hosp) == 25363,
          sum(m_sup$n_cases) == 983947, sum(m_sup$n_deaths) == 1050)

h_est <- dplyr::filter(h_all, model == selected_hosp_model)

## Required per-quantity hospitalisation outputs (selected model, all cells).
key_cols <- c("model", "age_group", "core_profile", "n_cases", "n_hosp", "n_nonhosp",
              "support_category", "estimate_status", "target_share_in_supported_state_years")
readr::write_csv(dplyr::select(h_est, dplyr::all_of(key_cols), std_risk_pct, std_risk_lower_pct, std_risk_upper_pct),
                 file.path(out_dir, "hospital_age_profile_standardised_risks.csv"))
readr::write_csv(dplyr::select(h_est, dplyr::all_of(key_cols), rr, rr_lower, rr_upper),
                 file.path(out_dir, "hospital_age_profile_standardised_RRs.csv"))
readr::write_csv(dplyr::select(h_est, dplyr::all_of(key_cols), rd_pp, rd_lower_pp, rd_upper_pp),
                 file.path(out_dir, "hospital_age_profile_risk_differences.csv"))

## Harmonise both outcomes to one long format.
harmonise <- function(d, outcome, events_col) {
  d |>
    dplyr::transmute(
      outcome = outcome, model = if ("model" %in% names(d)) model else NA_character_,
      age_group = factor(age_group, levels = age_levels),
      core_profile = factor(core_profile, levels = core_profile_levels),
      n_cases, n_events = .data[[events_col]],
      std_risk_pct, std_risk_lower_pct, std_risk_upper_pct,
      rr, rr_lower, rr_upper, rd_pp, rd_lower_pp, rd_upper_pp,
      adequate = n_events >= min_events & (n_cases - n_events) >= min_events
    )
}
hosp_d <- harmonise(h_est, "Hospitalisation", "n_hosp")
mort_d <- harmonise(m_est, "Chikungunya-attributed death", "n_deaths")

## Mortality must reproduce the current Figure 3 exactly.
cur_fig3 <- readr::read_csv(file.path(fig_rev_dir, "figure3_plotted_values.csv"), show_col_types = FALSE)
mort_check <- cur_fig3 |>
  dplyr::transmute(age_group, core_profile, cur_risk = std_risk_pct, cur_lo = std_risk_lower_pct,
                   cur_hi = std_risk_upper_pct) |>
  dplyr::inner_join(dplyr::mutate(mort_d, age_group = as.character(age_group),
                                  core_profile = as.character(core_profile)),
                    by = c("age_group", "core_profile")) |>
  dplyr::summarise(n_values = dplyr::n(),
                   max_abs_diff = max(abs(c(cur_risk - std_risk_pct, cur_lo - std_risk_lower_pct,
                                            cur_hi - std_risk_upper_pct))))
print(mort_check)
stopifnot(mort_check$max_abs_diff < 1e-10)
readr::write_csv(mort_check, file.path(out_dir, "mortality_reproduces_current_figure3.csv"))

## ============================================================
## 2. Panel builders
## ============================================================
pct_lab <- function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")
fmt_k <- function(n) ifelse(n >= 10000, paste0(round(n / 1000), "k"),
                     ifelse(n >= 1000, paste0(formatC(n / 1000, format = "f", digits = 1), "k"),
                            as.character(n)))
dodge <- ggplot2::position_dodge(width = 0.36)

## `child_reference_only`: in 0-14 only the None baseline is plotted. Recorded
## comorbidity in children is of doubtful validity (e.g. hypertension recorded
## in 3.4% of infants), so childhood profile contrasts are kept to the
## Supplement even where event counts are adequate.
is_plotted <- function(d, child_reference_only) {
  d$adequate & !(child_reference_only & d$age_group == "0-14" & d$core_profile != "None")
}
age_panel <- function(d, profiles, ages, title, y_lab, breaks, event_word, child_reference_only = FALSE) {
  d$plotted <- is_plotted(d, child_reference_only)
  pd <- d |> dplyr::filter(core_profile %in% profiles, age_group %in% ages, plotted) |>
    dplyr::mutate(core_profile = factor(core_profile, levels = profiles),
                  age_group = factor(age_group, levels = ages))
  ## Lines join only consecutive plotted age groups: a gap (unsupported cell)
  ## breaks the line rather than bridging it.
  pd <- pd |> dplyr::group_by(core_profile) |> dplyr::arrange(age_group, .by_group = TRUE) |>
    dplyr::mutate(run = cumsum(c(1, diff(as.integer(age_group)) != 1))) |> dplyr::ungroup()
  last <- pd |> dplyr::group_by(core_profile) |> dplyr::filter(as.integer(age_group) == max(as.integer(age_group))) |>
    dplyr::ungroup() |> dplyr::mutate(label = profile_display[as.character(core_profile)])
  n_ages <- length(ages)

  p <- ggplot2::ggplot(pd, ggplot2::aes(x = age_group, y = std_risk_pct, colour = core_profile)) +
    ggplot2::geom_line(ggplot2::aes(group = interaction(core_profile, run)), position = dodge,
                       linewidth = 0.5, alpha = 0.55) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                            position = dodge, linewidth = 0.55) +
    ggplot2::geom_point(ggplot2::aes(shape = core_profile, fill = core_profile), position = dodge,
                        size = 1.9, stroke = 0.7) +
    ggplot2::geom_text(data = last, ggplot2::aes(x = n_ages + 0.3, label = label), hjust = 0,
                       size = 2.35, family = "Arial", fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_x_discrete(labels = age_labels, drop = FALSE,
                              expand = ggplot2::expansion(add = c(0.45, 1.25))) +
    ggplot2::scale_y_log10(breaks = breaks, labels = pct_lab) +
    ggplot2::scale_colour_manual(values = profile_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = scales::alpha(profile_colours, 0.25), guide = "none") +
    ggplot2::scale_shape_manual(values = profile_shapes, guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, x = NULL, y = y_lab) +
    theme_rev() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                   axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(4, 4, 0, 4))

  strip <- d |> dplyr::filter(core_profile %in% profiles, age_group %in% ages) |>
    dplyr::mutate(core_profile = factor(core_profile, levels = rev(profiles)),
                  age_group = factor(age_group, levels = ages),
                  label = paste0(n_events, " / ", fmt_k(n_cases)))
  s <- ggplot2::ggplot(strip, ggplot2::aes(x = age_group, y = core_profile)) +
    ggplot2::geom_text(ggplot2::aes(label = label,
                                    colour = ifelse(plotted, as.character(core_profile), "muted")),
                       size = 1.95, family = "Arial") +
    ggplot2::scale_colour_manual(values = c(profile_colours, muted = ink_muted), guide = "none") +
    ggplot2::scale_x_discrete(labels = age_labels, drop = FALSE,
                              expand = ggplot2::expansion(add = c(0.45, 1.25))) +
    ggplot2::scale_y_discrete(labels = profile_display) +
    ggplot2::labs(x = "Age group (years)", y = NULL, subtitle = paste0(event_word, " / cases")) +
    theme_rev() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 6, colour = ink_soft),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(size = 6.2, colour = ink_soft,
                                                         margin = ggplot2::margin(b = 0)),
                   plot.margin = ggplot2::margin(0, 4, 4, 4))
  (p / s) + patchwork::plot_layout(heights = c(3.3, 0.18 * length(profiles) + 0.15))
}

profile_panel <- function(d, age, title, x_lab, breaks, limits, event_word) {
  pd <- d |> dplyr::filter(age_group == age) |>
    dplyr::mutate(
      count = factor(profile_count[as.character(core_profile)], levels = 0:3),
      count_label = factor(ifelse(count == "0", "", count_labels[as.character(count)]),
                           levels = c("", count_labels[-1])),
      profile_label = factor(profile_display[as.character(core_profile)], levels = rev(profile_display)),
      events_text = paste0(n_events, " / ", fmt_int(n_cases))
    )
  ref <- pd$std_risk_pct[pd$core_profile == "None"]
  ggplot2::ggplot(pd, ggplot2::aes(y = profile_label)) +
    ggplot2::geom_vline(xintercept = ref, linewidth = 0.3, colour = ink_muted, linetype = "22") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = std_risk_lower_pct, xmax = std_risk_upper_pct,
                                         colour = core_profile,
                                         ## sparse cells: dashed interval as well as an open
                                         ## marker (the DM+CKD symbol cannot be drawn open)
                                         linetype = ifelse(adequate, "solid", "22")), linewidth = 0.55) +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_point(ggplot2::aes(x = std_risk_pct, colour = core_profile, shape = core_profile,
                                     fill = ifelse(adequate, as.character(core_profile), "open")),
                        size = 1.9, stroke = 0.7) +
    ggplot2::geom_text(ggplot2::aes(x = limits[2], label = events_text,
                                    colour = ifelse(adequate, "soft", "muted")),
                       hjust = 1, size = 1.95, family = "Arial") +
    ggplot2::geom_text(data = dplyr::filter(pd, core_profile == "None"),
                       ggplot2::aes(x = limits[2], y = Inf, label = paste0(event_word, " / cases")),
                       hjust = 1, vjust = -0.9, size = 1.95, family = "Arial", colour = ink_soft,
                       fontface = "bold", inherit.aes = FALSE) +
    ggplot2::facet_grid(count_label ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_log10(breaks = breaks, labels = pct_lab, limits = limits) +
    ggplot2::scale_colour_manual(values = c(profile_colours, muted = ink_muted, soft = ink_soft),
                                 guide = "none") +
    ggplot2::scale_fill_manual(values = c(scales::alpha(profile_colours, 0.25), open = "white"),
                               guide = "none") +
    ggplot2::scale_shape_manual(values = profile_shapes, guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, x = x_lab, y = NULL) +
    theme_rev() +
    ggplot2::theme(strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "plain",
                                                             colour = ink_muted, size = 6.2),
                   axis.text.y = ggplot2::element_text(colour = ink, size = 6.8),
                   axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
                   panel.spacing.y = ggplot2::unit(1.2, "mm"),
                   plot.margin = ggplot2::margin(4, 6, 4, 2))
}

## ============================================================
## 3. Panels and layouts
## ============================================================
hosp_breaks <- c(1, 2, 3, 5, 10, 20, 30)
pA <- age_panel(hosp_d, panel_a_profiles, age_levels, "A  Hospitalisation, by age group",
                "Hospitalisation risk (%, log scale)", hosp_breaks, "Hospitalised",
                child_reference_only = TRUE)
pB <- profile_panel(hosp_d, "75+", "B  Hospitalisation, aged ≥75 years",
                    "Hospitalisation risk (%, log scale)",
                    c(3, 10, 30), c(3, 60), "Hospitalised")
pC <- age_panel(mort_d, panel_c_profiles, age_levels[-1], "C  Death, by adult age group",
                "Death risk (%, log scale)", c(0.01, 0.03, 0.1, 0.3, 1, 3, 10), "Deaths")
pD <- profile_panel(mort_d, "75+", "D  Death, aged ≥75 years",
                    "Death risk (%, log scale)", c(0.3, 1, 3, 10, 30), c(0.3, 40), "Deaths")

title_txt <- paste0("Figure 3. Age-specific standardised absolute risks of hospitalisation and
",
                    "chikungunya-attributed death by multimorbidity profile")
annot <- patchwork::plot_annotation(
  title = title_txt,
  theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "Arial", face = "bold", size = 9,
                                                            colour = ink),
                         plot.background = ggplot2::element_rect(fill = "white", colour = NA))
)

## Candidate 1: rows = outcome (A B / C D).
cand1 <- (patchwork::wrap_elements(full = pA) | patchwork::wrap_elements(full = pB)) /
  (patchwork::wrap_elements(full = pC) | patchwork::wrap_elements(full = pD)) +
  patchwork::plot_layout(widths = c(1, 1.15)) + annot
## Candidate 2: columns = outcome (A C / B D).
cand2 <- (patchwork::wrap_elements(full = pA) | patchwork::wrap_elements(full = pC)) /
  (patchwork::wrap_elements(full = pB) | patchwork::wrap_elements(full = pD)) + annot

save_figure(cand1, "Figure3_candidate1_rows_by_outcome", width = 183, height = 190, dir = out_dir,
            formats = "png")
save_figure(cand2, "Figure3_candidate2_columns_by_outcome", width = 183, height = 190, dir = out_dir,
            formats = "png")
save_figure(cand1, "Figure3_combined_absolute_risks_FINAL", width = 183, height = 190, dir = out_dir)

## Plotted data and support table.
fig_data <- dplyr::bind_rows(
  hosp_d |> dplyr::filter(core_profile %in% panel_a_profiles) |> dplyr::mutate(panel = "A"),
  hosp_d |> dplyr::filter(age_group == "75+") |> dplyr::mutate(panel = "B"),
  mort_d |> dplyr::filter(core_profile %in% panel_c_profiles, age_group != "0-14") |>
    dplyr::mutate(panel = "C", model = "fs_original"),
  mort_d |> dplyr::filter(age_group == "75+") |> dplyr::mutate(panel = "D", model = "fs_original")
) |>
  dplyr::mutate(plotted = dplyr::case_when(panel == "A" ~ adequate & !(age_group == "0-14" & core_profile != "None"),
                                           panel == "C" ~ adequate, TRUE ~ TRUE),
                marker = dplyr::case_when(!plotted ~ "not plotted (<10 events/non-events, or 0-14 profile contrast)",
                                          adequate ~ "filled", TRUE ~ "open (sparse)")) |>
  dplyr::relocate(panel)
readr::write_csv(fig_data, file.path(out_dir, "Figure3_combined_absolute_risks_data.csv"))

support_table <- dplyr::bind_rows(
  hosp_d |> dplyr::select(outcome, age_group, core_profile, n_cases, n_events, adequate),
  mort_d |> dplyr::select(outcome, age_group, core_profile, n_cases, n_events, adequate)
) |>
  dplyr::mutate(n_nonevents = n_cases - n_events,
                support_category = dplyr::case_when(adequate ~ "Adequate (>=10 events and non-events)",
                                                    n_events >= 1 ~ "Sparse (1-9 events)",
                                                    TRUE ~ "No events"))
readr::write_csv(support_table, file.path(out_dir, "Figure3_combined_support_table.csv"))

## ============================================================
## 4. Supplementary figures
## ============================================================
heatmap_fig <- function(d, event_word) {
  hm <- d |> dplyr::mutate(
    cat = factor(dplyr::case_when(adequate ~ "≥10 events",
                                  n_events >= 1 ~ "1–9 events (sparse)",
                                  TRUE ~ "0 events (unsupported)"),
                 levels = c("≥10 events", "1–9 events (sparse)", "0 events (unsupported)")),
    profile_label = factor(profile_display[as.character(core_profile)], levels = rev(profile_display)),
    label = paste0(fmt_int(n_events), "\n", fmt_int(n_cases)))
  ggplot2::ggplot(hm, ggplot2::aes(x = age_group, y = profile_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = cat), colour = "white", linewidth = 0.9) +
    ggplot2::geom_tile(data = dplyr::filter(hm, cat != "≥10 events"), fill = NA, colour = "#9A9A9A",
                       linewidth = 0.25, linetype = "22", width = 0.93, height = 0.88) +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = cat == "≥10 events"),
                       size = 2.05, lineheight = 0.9, family = "Arial") +
    ggplot2::scale_fill_manual(values = c("#1F5FA8", "#C6DBEF", "#F4F4F4"), drop = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = ink), guide = "none") +
    ggplot2::scale_x_discrete(labels = age_labels, expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = "Age group (years)", y = NULL,
                  subtitle = paste0(event_word, " (top) and cases (bottom) per cell")) +
    theme_rev() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(), legend.position = "bottom",
                   legend.justification = "left", plot.subtitle = ggplot2::element_text(size = 7))
}

rr_forest_fig <- function(d, ages, x_lim, breaks, event_word) {
  sa <- d |> dplyr::filter(core_profile != "None", age_group %in% ages) |>
    dplyr::mutate(count = factor(profile_count[as.character(core_profile)], levels = 0:3),
                  profile_label = factor(profile_display[as.character(core_profile)],
                                         levels = rev(profile_display[-1])),
                  status = dplyr::case_when(adequate ~ "adequate", n_events >= 1 ~ "sparse", TRUE ~ "none"),
                  lo = pmax(rr_lower, x_lim[1]), hi = pmin(rr_upper, x_lim[2]),
                  trunc_hi = rr_upper > x_lim[2], trunc_lo = rr_lower < x_lim[1],
                  col = ifelse(status == "adequate", as.character(core_profile), "muted"))
  ggplot2::ggplot(dplyr::filter(sa, status != "none"), ggplot2::aes(y = profile_label)) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
    ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = profile_label, colour = col),
                          linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = rr, colour = col, shape = core_profile,
                                     fill = ifelse(status == "adequate", as.character(core_profile), "open")),
                        size = 1.7, stroke = 0.6) +
    ggplot2::geom_text(data = dplyr::filter(sa, status == "none"),
                       ggplot2::aes(x = 1.08, label = paste0("0 ", tolower(event_word))), hjust = 0,
                       size = 2, colour = ink_muted, fontface = "italic", family = "Arial") +
    ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
    ggplot2::scale_x_log10(limits = x_lim, breaks = breaks, labels = as.character(breaks)) +
    ggplot2::scale_colour_manual(values = c(profile_colours, muted = "#A6A6A6"), guide = "none") +
    ggplot2::scale_fill_manual(values = c(scales::alpha(profile_colours, 0.25), open = "white"),
                               guide = "none") +
    ggplot2::scale_shape_manual(values = profile_shapes, guide = "none") +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(x = "Standardised risk ratio vs None (log scale)", y = NULL,
                  subtitle = paste0("Age group (years). Filled: ≥10 ", tolower(event_word),
                                    " and non-events in cell; open grey: sparse; intervals clipped at axis limits.")) +
    theme_rev() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(3, "mm"),
                   panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                   plot.subtitle = ggplot2::element_text(size = 7))
}

hm_h <- heatmap_fig(hosp_d, "Hospitalisations")
save_figure(hm_h, "hospital_age_profile_support_heatmap", 140, 110, dir = out_dir, formats = c("pdf", "png"))
save_figure(hm_h, "Supp_hospital_age_profile_support", 140, 110, dir = out_dir, formats = c("pdf", "png"))
save_figure(heatmap_fig(mort_d, "Deaths"), "Supp_mortality_age_profile_support", 140, 110,
            dir = out_dir, formats = c("pdf", "png"))
save_figure(rr_forest_fig(hosp_d, age_levels, c(0.3, 20), c(0.5, 1, 2, 5, 10), "Hospitalisations"),
            "Supp_hospital_age_specific_RR", 183, 80, dir = out_dir, formats = c("pdf", "png"))
save_figure(rr_forest_fig(mort_d, age_levels, c(0.5, 100), c(1, 3, 10, 30, 100), "Deaths"),
            "Supp_mortality_age_specific_RR", 183, 80, dir = out_dir, formats = c("pdf", "png"))

## Robustness: RR in adequately supported cells across hospitalisation models.
rob_models <- c(H0 = "H0: no age interaction", H1 = "H1: original factor-smooth",
                H2 = "H2: global smooth + deviations", H3 = "H3: age-group random effect",
                H3_fixed = "H3: unpenalised age-group interaction", H2_k15 = "H2, k = 15",
                H2_sp_x4 = "H2, 4× smoother")
rob <- h_all |>
  dplyr::filter(model %in% names(rob_models), core_profile != "None", support_category == "Adequate") |>
  dplyr::mutate(model = factor(model, levels = names(rob_models)),
                age_group = factor(age_group, levels = age_levels),
                profile_label = factor(profile_display[core_profile], levels = rev(profile_display[-1])))
rob_cols <- c("#1A1A1A", "#D55E00", "#0072B2", "#E69F00", "#B8A300", "#56B4E9", "#CC79A7")
fig_rob <- ggplot2::ggplot(rob, ggplot2::aes(x = rr, y = profile_label, colour = model)) +
  ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
                          position = ggplot2::position_dodge(width = 0.8), linewidth = 0.35) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8), size = 0.9) +
  ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
  ggplot2::scale_x_log10(breaks = c(0.5, 1, 2, 5, 10), labels = c("0.5", "1", "2", "5", "10")) +
  ggplot2::scale_colour_manual(values = setNames(rob_cols, names(rob_models)), labels = rob_models) +
  ggplot2::labs(x = "Standardised risk ratio for hospitalisation vs None (log scale)", y = NULL,
                subtitle = "Cells with ≥10 hospitalisations and non-hospitalisations") +
  theme_rev() +
  ggplot2::theme(legend.position = "bottom", legend.justification = "left",
                 panel.spacing.x = ggplot2::unit(3, "mm"),
                 panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                 plot.subtitle = ggplot2::element_text(size = 7)) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))
save_figure(fig_rob, "Supp_hospital_age_interaction_robustness", 183, 115, dir = out_dir,
            formats = c("pdf", "png"))

## Continuous hospitalisation RR curves: original H1 vs H2, coloured by
## local event support (shows any artefactual peaks/troughs).
ca <- readr::read_csv(file.path(out_dir, "hospital_curve_local_support_audit.csv"), show_col_types = FALSE) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels[-1]),
                model = factor(dplyr::recode(model, H1 = "H1: original factor-smooth",
                                             H2 = "H2: global smooth + deviations")),
                support = factor(local_support, levels = c(">=10 events within +/-5 y",
                                                           "1-9 events within +/-5 y",
                                                           "No events within +/-5 y"))) |>
  dplyr::arrange(model, core_profile, age_years) |>
  dplyr::group_by(model, core_profile) |>
  dplyr::mutate(rr_next = dplyr::lead(rr), age_next = dplyr::lead(age_years)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(rr_next))
fig_curve <- ggplot2::ggplot(ca, ggplot2::aes(x = age_years, xend = age_next, y = rr, yend = rr_next,
                                              colour = support, linetype = model)) +
  ggplot2::geom_hline(yintercept = 1, linewidth = 0.3, colour = ink_muted) +
  ggplot2::geom_segment(linewidth = 0.6) +
  ggplot2::facet_wrap(~ core_profile, nrow = 2, labeller = ggplot2::as_labeller(profile_display)) +
  ggplot2::scale_y_log10(breaks = c(0.5, 1, 2, 5, 10), labels = c("0.5", "1", "2", "5", "10")) +
  ggplot2::scale_colour_manual(values = c("#1F5FA8", "#E69F00", "#B3B3B3"), drop = FALSE,
                               labels = c("≥10 events within ±5 y", "1–9 events within ±5 y",
                                          "No events within ±5 y")) +
  ggplot2::scale_linetype_manual(values = c("solid", "22")) +
  ggplot2::labs(x = "Age (years)", y = "Risk ratio for hospitalisation vs None (log scale)",
                subtitle = "Continuous age curves, standardised over the whole cohort's sex, year and state distribution") +
  theme_rev() +
  ggplot2::theme(legend.position = "bottom", legend.justification = "left", legend.box = "vertical",
                 panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                 plot.subtitle = ggplot2::element_text(size = 7))
save_figure(fig_curve, "Supp_hospital_RR_curve_validation", 183, 115, dir = out_dir, formats = c("pdf", "png"))

message("[53] Figure 3 (final) and supplementary figures written to ", out_dir)
