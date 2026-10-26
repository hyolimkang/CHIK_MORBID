# ---------------------------------------------------------------------------
# 49_figure3_age_absolute_risk.R
#
# Main-manuscript Figure 3 (age-specific ABSOLUTE mortality risk) and the
# supplementary age figures, built from the Figure 5 validation outputs
# (03_Output/validation/figure5_age_validation/, scripts 46-47). No model is
# refitted for the mortality figures: estimates are the within-age-group
# g-computation risks from the original age-interaction (fs) GAM, 2,000
# coefficient draws.
#
# Panel A: risk across adult age groups for profiles with >=10 deaths in at
#          least three adult age groups (None, HTN, DM+HTN).
# Panel B: risk by profile among adults aged >=75 years (all profiles; cells
#          with <10 deaths drawn open).
#
# Also writes a counts-only hospitalisation age x profile support table, to
# document the decision not to add a hospitalisation panel (no hospitalisation
# age-specific model has been validated).
#
# Output (03_Output/figure_revision/):
#   Figure3_age_specific_absolute_risk_main.pdf/.png/.svg
#   Supp_Figure_age_specific_mortality_RR.pdf/.png/.svg
#   Supp_Figure_age_support_heatmap.pdf/.png/.svg
#   Supp_Figure_age_RR_curve_validation.pdf/.png/.svg
#   figure3_plotted_values.csv, hospitalisation_age_profile_support_counts.csv
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

val_dir <- "03_Output/validation/figure5_age_validation"
min_deaths <- 10

est <- readr::read_csv(file.path(val_dir, "age_profile_standardised_estimates.csv"),
                       show_col_types = FALSE)
support <- readr::read_csv(file.path(val_dir, "age_profile_support.csv"),
                           show_col_types = FALSE) |>
  dplyr::filter(age_scheme == "primary")

stopifnot(sum(support$n_cases) == 983947, sum(support$n_deaths) == 1050)

est <- est |>
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels),
    adequate = n_deaths >= min_deaths
  )

## ============================================================
## Figure 3A - risk across adult age groups
## ============================================================
## Profiles shown: >=10 deaths in at least 3 of the 4 adult age groups, so
## the panel shows an age gradient rather than one or two isolated points.
## Within those profiles only adequately supported cells are drawn.
adult_levels <- age_levels[-1]
panel_a_profiles <- est |>
  dplyr::filter(age_group %in% adult_levels, adequate) |>
  dplyr::count(core_profile) |>
  dplyr::filter(n >= 3) |>
  dplyr::pull(core_profile)
panel_a_profiles <- intersect(core_profile_levels, panel_a_profiles)

fa <- est |>
  dplyr::filter(core_profile %in% panel_a_profiles, age_group %in% adult_levels, adequate) |>
  dplyr::mutate(core_profile = factor(core_profile, levels = panel_a_profiles),
                age_group = droplevels(age_group))

dodge <- ggplot2::position_dodge(width = 0.32)
risk_breaks <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10)
fmt_k <- function(n) ifelse(n >= 10000, paste0(round(n / 1000), "k"),
                    ifelse(n >= 1000, paste0(formatC(n / 1000, format = "f", digits = 1), "k"),
                           as.character(n)))
pct_lab <- function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")

end_labels <- fa |>
  dplyr::filter(age_group == "75+") |>
  dplyr::mutate(label = profile_display[as.character(core_profile)])

p3a <- ggplot2::ggplot(fa, ggplot2::aes(x = age_group, y = std_risk_pct, colour = core_profile,
                                        group = core_profile)) +
  ggplot2::geom_line(position = dodge, linewidth = 0.5, alpha = 0.55) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                          position = dodge, linewidth = 0.55) +
  ggplot2::geom_point(ggplot2::aes(shape = core_profile), position = dodge, size = 2,
                      fill = "white", stroke = 0.8) +
  ## Direct labels to the right of the last age group (x = 4 is "75+").
  ggplot2::geom_text(data = end_labels, ggplot2::aes(x = 4.32, label = label),
                     hjust = 0, size = 2.5,
                     family = "Arial", fontface = "bold", show.legend = FALSE) +
  ggplot2::scale_x_discrete(labels = age_labels, expand = ggplot2::expansion(add = c(0.4, 1.05))) +
  ggplot2::scale_y_log10(breaks = risk_breaks, labels = pct_lab) +
  ggplot2::scale_colour_manual(values = profile_colours[panel_a_profiles], guide = "none") +
  ggplot2::scale_shape_manual(values = profile_shapes[panel_a_profiles], guide = "none") +
  ggplot2::labs(title = "A  Across adult age groups", x = NULL,
                y = "Standardised risk of death (%, log scale)") +
  ggplot2::coord_cartesian(clip = "off") +
  theme_rev() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(4, 22, 0, 4)
  )

## Support strip: deaths / cases per age group for the plotted profiles. Cells
## not plotted (<10 deaths) keep their counts, greyed, so absence is explained.
strip_a <- support |>
  dplyr::filter(core_profile %in% panel_a_profiles, age_group %in% adult_levels) |>
  dplyr::mutate(
    core_profile = factor(core_profile, levels = rev(panel_a_profiles)),
    age_group = factor(age_group, levels = adult_levels),
    shown = n_deaths >= min_deaths,
    label = paste0(n_deaths, " / ", fmt_k(n_cases))
  )

p3a_strip <- ggplot2::ggplot(strip_a, ggplot2::aes(x = age_group, y = core_profile)) +
  ggplot2::geom_text(ggplot2::aes(label = label, colour = ifelse(shown, as.character(core_profile), "muted")),
                     size = 2.1, family = "Arial") +
  ggplot2::scale_colour_manual(values = c(profile_colours, muted = ink_muted), guide = "none") +
  ggplot2::scale_x_discrete(labels = age_labels, expand = ggplot2::expansion(add = c(0.4, 1.05))) +
  ggplot2::scale_y_discrete(labels = profile_display) +
  ggplot2::labs(x = "Age group (years)", y = NULL, subtitle = "Deaths / cases") +
  theme_rev() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 6.5, colour = ink_soft),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_text(size = 6.5, colour = ink_soft, margin = ggplot2::margin(b = 0)),
    plot.margin = ggplot2::margin(0, 4, 4, 4)
  )

## ============================================================
## Figure 3B - risk by profile, age >=75
## ============================================================
fb <- est |>
  dplyr::filter(age_group == "75+") |>
  dplyr::mutate(
    count = factor(profile_count[core_profile], levels = 0:3),
    count_label = factor(ifelse(count == "0", "", count_labels[as.character(count)]),
                         levels = c("", count_labels[-1])),
    profile_label = factor(profile_display[core_profile], levels = rev(profile_display)),
    sparse = n_deaths < min_deaths,
    events_text = paste0(n_deaths, " / ", fmt_int(n_cases))
  )
none_75 <- fb$std_risk_pct[fb$core_profile == "None"]

p3b <- ggplot2::ggplot(fb, ggplot2::aes(y = profile_label)) +
  ggplot2::geom_vline(xintercept = none_75, linewidth = 0.3, colour = ink_muted, linetype = "22") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = std_risk_lower_pct, xmax = std_risk_upper_pct,
                                       colour = count), linewidth = 0.55) +
  ggplot2::geom_point(ggplot2::aes(x = std_risk_pct, colour = count,
                                   fill = ifelse(sparse, "white", as.character(count))),
                      shape = 21, size = 2, stroke = 0.6) +
  ggplot2::geom_text(ggplot2::aes(x = 40, label = events_text,
                                  colour = ifelse(sparse, "muted", "soft")),
                     hjust = 1, size = 2.1, family = "Arial") +
  ## Column header drawn once, in the top (reference) facet only.
  ggplot2::geom_text(data = dplyr::filter(fb, core_profile == "None"),
                     ggplot2::aes(x = 40, y = Inf, label = "Deaths / cases"), hjust = 1,
                     vjust = -0.9, size = 2.1, family = "Arial", colour = ink_soft,
                     fontface = "bold", inherit.aes = FALSE) +
  ggplot2::facet_grid(count_label ~ ., scales = "free_y", space = "free_y", switch = "y") +
  ggplot2::scale_x_log10(breaks = c(0.3, 1, 3, 10, 30), labels = pct_lab,
                         limits = c(0.3, 40)) +
  ggplot2::scale_colour_manual(values = c(count_colours, muted = ink_muted, soft = ink_soft),
                               guide = "none") +
  ggplot2::scale_fill_manual(values = c(count_colours, white = "white"), guide = "none") +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(title = "B  Aged ≥75 years, by profile",
                x = "Standardised risk of death (%, log scale)", y = NULL) +
  theme_rev() +
  ggplot2::theme(
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "plain",
                                              colour = ink_muted, size = 6.5),
    axis.text.y = ggplot2::element_text(colour = ink, size = 7.2),
    axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
    panel.spacing.y = ggplot2::unit(1.4, "mm"),
    plot.margin = ggplot2::margin(4, 6, 4, 2)
  )

left <- p3a / p3a_strip + patchwork::plot_layout(heights = c(3.4, 0.62))
fig3 <- (left | p3b) + patchwork::plot_layout(widths = c(1, 1.15))

save_figure(fig3, "Figure3_age_specific_absolute_risk_main", width = 183, height = 95)

readr::write_csv(
  dplyr::bind_rows(
    dplyr::mutate(fa, panel = "A"),
    dplyr::mutate(fb, panel = "B")
  ) |>
    dplyr::select(panel, age_group, core_profile, n_cases, n_deaths, std_risk_pct,
                  std_risk_lower_pct, std_risk_upper_pct, rr, rr_lower, rr_upper,
                  estimate_status),
  file.path(fig_rev_dir, "figure3_plotted_values.csv")
)

## ============================================================
## Supplementary figure - age-group-specific mortality RR
## ============================================================
x_min <- 0.5; x_max <- 100
sa <- est |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(
    count = factor(profile_count[core_profile], levels = 0:3),
    profile_label = factor(profile_display[core_profile], levels = rev(profile_display[-1])),
    status = dplyr::case_when(
      n_deaths >= min_deaths ~ "adequate",
      n_deaths >= 1 ~ "sparse",
      TRUE ~ "none"
    ),
    lo = pmax(rr_lower, x_min), hi = pmin(rr_upper, x_max), trunc = rr_upper > x_max
  )

supp_rr <- ggplot2::ggplot(dplyr::filter(sa, status != "none"),
                           ggplot2::aes(y = profile_label)) +
  ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
  ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = profile_label,
                                     colour = ifelse(status == "adequate", as.character(count), "muted")),
                        linewidth = 0.5) +
  ggplot2::geom_segment(data = dplyr::filter(sa, status != "none", trunc),
                        ggplot2::aes(x = x_max * 0.8, xend = x_max, yend = profile_label),
                        colour = ink_muted, linewidth = 0.5,
                        arrow = ggplot2::arrow(length = ggplot2::unit(1.1, "mm"), type = "closed")) +
  ggplot2::geom_point(ggplot2::aes(x = rr,
                                   colour = ifelse(status == "adequate", as.character(count), "muted"),
                                   fill = ifelse(status == "adequate", as.character(count), "white")),
                      shape = 21, size = 1.7, stroke = 0.55) +
  ggplot2::geom_text(data = dplyr::filter(sa, status == "none"),
                     ggplot2::aes(x = 1.15, label = "0 deaths"), hjust = 0, size = 2,
                     colour = ink_muted, fontface = "italic", family = "Arial") +
  ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
  ggplot2::scale_x_log10(limits = c(x_min, x_max), breaks = c(1, 3, 10, 30, 100),
                         labels = c("1", "3", "10", "30", "100")) +
  ggplot2::scale_colour_manual(values = c(count_colours, muted = "#A6A6A6"), guide = "none") +
  ggplot2::scale_fill_manual(values = c(count_colours, white = "white"), guide = "none") +
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::labs(
    x = "Standardised risk ratio for death vs None (log scale)", y = NULL,
    subtitle = "Age group (years). Filled: ≥10 deaths in cell; open grey: 1–9 deaths; '0 deaths': not estimated."
  ) +
  theme_rev() +
  ggplot2::theme(panel.spacing.x = ggplot2::unit(3, "mm"),
                 panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                 plot.subtitle = ggplot2::element_text(size = 7))

save_figure(supp_rr, "Supp_Figure_age_specific_mortality_RR", width = 183, height = 80)

## ============================================================
## Supplementary figure - support heatmap
## ============================================================
hm <- support |>
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels),
    profile_label = factor(profile_display[core_profile], levels = rev(profile_display)),
    cat = factor(dplyr::case_when(n_deaths >= min_deaths ~ "≥10 deaths",
                                  n_deaths >= 1 ~ "1–9 deaths (sparse)",
                                  TRUE ~ "0 deaths (unsupported)"),
                 levels = c("≥10 deaths", "1–9 deaths (sparse)", "0 deaths (unsupported)")),
    label = paste0(n_deaths, "\n", fmt_int(n_cases))
  )

supp_hm <- ggplot2::ggplot(hm, ggplot2::aes(x = age_group, y = profile_label)) +
  ggplot2::geom_tile(ggplot2::aes(fill = cat), colour = "white", linewidth = 0.9) +
  ggplot2::geom_tile(data = dplyr::filter(hm, cat != "≥10 deaths"), fill = NA,
                     colour = "#9A9A9A", linewidth = 0.25, linetype = "22",
                     width = 0.93, height = 0.88) +
  ggplot2::geom_text(ggplot2::aes(label = label, colour = cat == "≥10 deaths"),
                     size = 2.05, lineheight = 0.9, family = "Arial") +
  ggplot2::scale_fill_manual(values = c("#1F5FA8", "#C6DBEF", "#F4F4F4"), drop = FALSE) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = ink), guide = "none") +
  ggplot2::scale_x_discrete(labels = age_labels, expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::labs(x = "Age group (years)", y = NULL,
                subtitle = "Chikungunya-attributed deaths (top) and confirmed cases (bottom) per cell") +
  theme_rev() +
  ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 legend.position = "bottom", legend.justification = "left",
                 plot.subtitle = ggplot2::element_text(size = 7))

save_figure(supp_hm, "Supp_Figure_age_support_heatmap", width = 140, height = 110)

## ============================================================
## Supplementary figure - original continuous RR curves vs local support
## ============================================================
curve <- readr::read_csv(file.path(val_dir, "original_curve_local_support_audit.csv"),
                         show_col_types = FALSE) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(
    core_profile = factor(core_profile, levels = core_profile_levels[-1]),
    support = factor(dplyr::case_when(
      local_support == ">=10 deaths within +/-5 y" ~ "≥10 deaths within ±5 y",
      local_support == "1-9 deaths within +/-5 y" ~ "1–9 deaths within ±5 y",
      TRUE ~ "No deaths within ±5 y"
    ), levels = c("≥10 deaths within ±5 y", "1–9 deaths within ±5 y",
                  "No deaths within ±5 y"))
  ) |>
  dplyr::arrange(core_profile, age_years) |>
  dplyr::group_by(core_profile) |>
  dplyr::mutate(rr_next = dplyr::lead(rr), age_next = dplyr::lead(age_years)) |>
  dplyr::ungroup()

gs <- readr::read_csv(file.path(val_dir, "fs_global_curve_points.csv"), show_col_types = FALSE) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels[-1]),
                series = "Alternative: shared age smooth + profile deviations")

supp_curve <- ggplot2::ggplot(curve, ggplot2::aes(x = age_years)) +
  ggplot2::geom_hline(yintercept = 1, linewidth = 0.3, colour = ink_muted) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(rr_lower, 0.05), ymax = pmin(rr_upper, 500)),
                       fill = "#5B9BD5", alpha = 0.13) +
  ggplot2::geom_segment(data = dplyr::filter(curve, !is.na(rr_next)),
                        ggplot2::aes(xend = age_next, y = rr, yend = rr_next, colour = support,
                                     linetype = support), linewidth = 0.7) +
  ggplot2::geom_line(data = gs, ggplot2::aes(y = rr, group = core_profile), colour = ink,
                     linewidth = 0.35, linetype = "dotdash") +
  ggplot2::facet_wrap(~ core_profile, nrow = 2,
                      labeller = ggplot2::as_labeller(profile_display)) +
  ggplot2::scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  ggplot2::coord_cartesian(ylim = c(0.05, 500)) +
  ggplot2::scale_colour_manual(values = c("#1F5FA8", "#E69F00", "#B3B3B3")) +
  ggplot2::scale_linetype_manual(values = c("solid", "solid", "22")) +
  ggplot2::labs(
    x = "Age (years)", y = "Risk ratio for death vs None (log scale)",
    subtitle = paste0("Original continuous curves (factor-smooth GAM) coloured by deaths in that profile within ±5 years; ",
                      "black dot-dash: alternative with a shared age smooth")
  ) +
  theme_rev() +
  ggplot2::theme(legend.position = "bottom", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                 plot.subtitle = ggplot2::element_text(size = 7))

save_figure(supp_curve, "Supp_Figure_age_RR_curve_validation", width = 183, height = 112)

## ============================================================
## Hospitalisation: counts-only age x profile support (documentation)
## ============================================================
## Same cohort rules as script 12's hosp_cohort. Counts only - no model.
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
hosp_support <- ind |>
  dplyr::filter(
    is_confirmed_chik, lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                  ~ .x %in% c("no", "yes")),
    hospitalised %in% c("no", "yes")
  ) |>
  dplyr::mutate(
    dm = diabetes == "yes", htn = hypertension == "yes", ckd = renal_disease == "yes",
    core_profile = dplyr::case_when(
      !dm & !htn & !ckd ~ "None", dm & !htn & !ckd ~ "DM", !dm & htn & !ckd ~ "HTN",
      !dm & !htn & ckd ~ "CKD", dm & htn & !ckd ~ "DM+HTN", dm & !htn & ckd ~ "DM+CKD",
      !dm & htn & ckd ~ "HTN+CKD", TRUE ~ "DM+HTN+CKD"),
    age_group = cut(age_years, c(0, 15, 40, 60, 75, 101), right = FALSE,
                    include.lowest = TRUE, labels = age_levels)
  ) |>
  dplyr::group_by(age_group, core_profile) |>
  dplyr::summarise(n_cases = dplyr::n(), n_hospitalised = sum(hospitalised == "yes"),
                   .groups = "drop") |>
  tidyr::complete(age_group, core_profile = core_profile_levels,
                  fill = list(n_cases = 0L, n_hospitalised = 0L)) |>
  dplyr::mutate(crude_risk_pct = 100 * n_hospitalised / n_cases,
                ge10_events = n_hospitalised >= 10)
rm(ind)
stopifnot(sum(hosp_support$n_cases) == 776072, sum(hosp_support$n_hospitalised) == 25363)
readr::write_csv(hosp_support, file.path(fig_rev_dir, "hospitalisation_age_profile_support_counts.csv"))
message(sprintf("[49] hospitalisation cells with >=10 events: %d / 40",
                sum(hosp_support$ge10_events)))

message("[49] Figure 3 and supplementary age figures written to ", fig_rev_dir)
