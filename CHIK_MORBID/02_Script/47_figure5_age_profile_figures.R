# ---------------------------------------------------------------------------
# 47_figure5_age_profile_figures.R
#
# Figures for the Figure 5 validation (see 46_figure5_age_profile_validation.R,
# which must be run first). Reads only the CSVs written by script 46 - no
# model refitting - and writes to 03_Output/validation/figure5_age_validation/:
#   Figure_5A_age_stratified_RR.pdf/.png/.svg
#   Figure_5B_age_stratified_absolute_risk.pdf/.png/.svg
#   Supplement_age_profile_support_heatmap.pdf/.png
#   Supplement_original_curve_support_overlay.pdf/.png
#   Supplement_age_profile_robustness.pdf/.png
# The original Figure 5 (03_Output/figures/fig3_profile_age_specific_relative_risk.*)
# is not touched.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

out_dir <- "03_Output/validation/figure5_age_validation"

core_profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD",
                         "HTN+CKD", "DM+HTN+CKD")
age_levels <- c("0-14", "15-39", "40-59", "60-74", "75+")
age_labels <- setNames(c("0–14", "15–39", "40–59", "60–74", "≥75"),
                       age_levels)

## Okabe-Ito (colour-blind safe); fixed per profile so a profile keeps its
## colour in every figure. Shapes give a redundant, non-colour encoding.
profile_colours <- c(
  "None" = "#5F5F5F", "DM" = "#E69F00", "HTN" = "#0072B2", "CKD" = "#56B4E9",
  "DM+HTN" = "#009E73", "DM+CKD" = "#F0E442", "HTN+CKD" = "#D55E00",
  "DM+HTN+CKD" = "#CC79A7"
)
profile_shapes <- c(
  "None" = 16, "DM" = 17, "HTN" = 15, "CKD" = 18, "DM+HTN" = 25,
  "DM+CKD" = 4, "HTN+CKD" = 23, "DM+HTN+CKD" = 21
)

theme_cid <- function(base_size = 8) {
  ggplot2::theme_classic(base_size = base_size, base_family = "Arial") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#1A1A1A"),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "#333333"),
      axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.5),
      legend.key.size = ggplot2::unit(3.5, "mm"),
      plot.margin = ggplot2::margin(4, 6, 4, 4),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

save_all <- function(plot, stem, width, height, svg = TRUE) {
  ggplot2::ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width,
                  height = height, units = "mm", device = grDevices::cairo_pdf)
  ggplot2::ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width,
                  height = height, units = "mm", dpi = 600, bg = "white",
                  device = ragg::agg_png)
  if (svg) {
    ggplot2::ggsave(file.path(out_dir, paste0(stem, ".svg")), plot, width = width,
                    height = height, units = "mm", device = svglite::svglite)
  }
}

est <- readr::read_csv(file.path(out_dir, "age_profile_standardised_estimates.csv"),
                       show_col_types = FALSE) |>
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels),
    core_profile = factor(core_profile, levels = core_profile_levels)
  )

support <- readr::read_csv(file.path(out_dir, "age_profile_support.csv"),
                           show_col_types = FALSE) |>
  dplyr::filter(age_scheme == "primary") |>
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels),
    core_profile = factor(core_profile, levels = core_profile_levels)
  )

## ============================================================
## Figure 5A - forest plot of within-age-group standardised RR
## ============================================================
## Reported (>=10 deaths in the profile cell): filled marker.
## Flagged (1-9 deaths): open marker, grey interval - shown for
## transparency but visibly de-emphasised.
## Suppressed (0 deaths): no estimate; the row is labelled "0 deaths" so
## an absent estimate can never be read as RR = 1 or as zero risk.
x_min <- 0.5
x_max <- 100

fa <- est |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(
    core_profile = droplevels(core_profile),
    status = dplyr::case_when(
      estimate_status == "Reported" ~ "≥10 deaths in cell",
      grepl("^Flagged", estimate_status) ~ "1–9 deaths in cell (sparse)",
      TRUE ~ "Suppressed"
    ),
    lo_plot = pmax(rr_lower, x_min),
    hi_plot = pmin(rr_upper, x_max),
    hi_trunc = rr_upper > x_max,
    lo_trunc = rr_lower < x_min
  )

fa_est <- dplyr::filter(fa, status != "Suppressed") |>
  dplyr::mutate(status = factor(status, levels = c("≥10 deaths in cell",
                                                   "1–9 deaths in cell (sparse)")))
fa_sup <- dplyr::filter(fa, status == "Suppressed")

fig5a <- ggplot2::ggplot(fa_est, ggplot2::aes(y = forcats::fct_rev(core_profile))) +
  ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = "#8C8C8C") +
  ggplot2::geom_segment(
    ggplot2::aes(x = lo_plot, xend = hi_plot, yend = forcats::fct_rev(core_profile),
                 colour = status),
    linewidth = 0.45
  ) +
  ggplot2::geom_segment(
    data = dplyr::filter(fa_est, hi_trunc),
    ggplot2::aes(x = x_max * 0.8, xend = x_max, yend = forcats::fct_rev(core_profile),
                 colour = status),
    linewidth = 0.45, show.legend = FALSE,
    arrow = ggplot2::arrow(length = ggplot2::unit(1.2, "mm"), type = "closed")
  ) +
  ggplot2::geom_point(ggplot2::aes(x = rr, shape = status, colour = status, fill = status),
                      size = 1.7, stroke = 0.5) +
  ggplot2::geom_text(
    data = fa_sup,
    ggplot2::aes(x = 1.15, y = forcats::fct_rev(core_profile), label = "0 deaths"),
    hjust = 0, size = 2.1, colour = "#8C8C8C", fontface = "italic", family = "Arial"
  ) +
  ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
  ggplot2::scale_x_log10(
    limits = c(x_min, x_max), breaks = c(1, 3, 10, 30, 100),
    labels = c("1", "3", "10", "30", "100"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_colour_manual(values = c("#1A1A1A", "#9A9A9A"), drop = FALSE) +
  ggplot2::scale_fill_manual(values = c("#1A1A1A", "white"), drop = FALSE) +
  ggplot2::scale_shape_manual(values = c(21, 21), drop = FALSE) +
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::labs(
    x = "Standardised risk ratio for death vs no DM, HTN or CKD (log scale)",
    y = NULL,
    subtitle = "Age group (years)"
  ) +
  theme_cid() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, margin = ggplot2::margin(b = 1)),
    panel.spacing.x = ggplot2::unit(3, "mm"),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = "#EDEDED")
  )

save_all(fig5a, "Figure_5A_age_stratified_RR", width = 180, height = 85)

## ============================================================
## Figure 5B - standardised absolute risk, supported cells only
## ============================================================
## A profile appears only in age groups where its own cell has >=10 deaths
## (the None reference always does). Unsupported cells are absent - not
## zero - and are shown as "-" in the count table beneath.
fb <- est |>
  dplyr::filter(estimate_status %in% c("Reported", "Reference")) |>
  dplyr::mutate(core_profile = droplevels(core_profile))

shown_profiles <- levels(fb$core_profile)
dodge <- ggplot2::position_dodge(width = 0.6)

fig5b_plot <- ggplot2::ggplot(
  fb,
  ggplot2::aes(x = age_group, y = std_risk_pct, colour = core_profile,
               shape = core_profile, fill = core_profile, group = core_profile)
) +
  ggplot2::geom_line(position = dodge, linewidth = 0.4, alpha = 0.6) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                          position = dodge, linewidth = 0.45) +
  ggplot2::geom_point(position = dodge, size = 1.8, stroke = 0.6) +
  ggplot2::scale_x_discrete(labels = age_labels, drop = FALSE) +
  ggplot2::scale_y_log10(
    breaks = c(0.01, 0.03, 0.1, 0.3, 1, 3, 10),
    labels = function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")
  ) +
  ggplot2::scale_colour_manual(values = profile_colours[shown_profiles]) +
  ggplot2::scale_fill_manual(values = profile_colours[shown_profiles]) +
  ggplot2::scale_shape_manual(values = profile_shapes[shown_profiles]) +
  ggplot2::labs(x = NULL, y = "Standardised risk of death (%, log scale)") +
  theme_cid() +
  ggplot2::theme(
    legend.position = "top", legend.justification = "left",
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = "#EDEDED"),
    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(4, 6, 0, 4)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))

fmt_n <- function(n) ifelse(n >= 10000, paste0(formatC(n / 1000, format = "f", digits = 0), "k"),
                     ifelse(n >= 1000, paste0(formatC(n / 1000, format = "f", digits = 1), "k"),
                            as.character(n)))

tab <- support |>
  dplyr::filter(core_profile %in% shown_profiles) |>
  dplyr::mutate(
    core_profile = factor(as.character(core_profile), levels = shown_profiles),
    shown = n_deaths >= 10,
    label = ifelse(shown, paste0(n_deaths, " / ", fmt_n(n_cases)), "–")
  )

fig5b_table <- ggplot2::ggplot(
  tab, ggplot2::aes(x = age_group, y = forcats::fct_rev(core_profile))
) +
  ggplot2::geom_text(ggplot2::aes(label = label, colour = shown), size = 2.2, family = "Arial") +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "#1A1A1A", `FALSE` = "#9A9A9A"), guide = "none") +
  ggplot2::scale_x_discrete(labels = age_labels, drop = FALSE, position = "bottom") +
  ggplot2::labs(x = "Age group (years)", y = NULL,
                subtitle = "Deaths / cases in each age group") +
  theme_cid() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 6.8, colour = "#333333"),
    plot.subtitle = ggplot2::element_text(size = 7, colour = "#4D4D4D", margin = ggplot2::margin(b = 1)),
    plot.margin = ggplot2::margin(0, 6, 4, 4)
  )

fig5b <- fig5b_plot / fig5b_table + patchwork::plot_layout(heights = c(3, 0.22 * length(shown_profiles) + 0.3))
save_all(fig5b, "Figure_5B_age_stratified_absolute_risk", width = 140, height = 120)

## ============================================================
## Supplement - empirical support heatmap
## ============================================================
hm <- support |>
  dplyr::mutate(
    support_category = factor(
      dplyr::recode(support_category, "Adequate" = "≥10 deaths",
                    "Sparse" = "1–9 deaths (sparse)", "No deaths" = "0 deaths (unsupported)"),
      levels = c("≥10 deaths", "1–9 deaths (sparse)", "0 deaths (unsupported)")),
    label = paste0(n_deaths, "\n", "n = ", format(n_cases, big.mark = ",", trim = TRUE))
  )

fig_hm <- ggplot2::ggplot(hm, ggplot2::aes(x = age_group, y = forcats::fct_rev(core_profile))) +
  ggplot2::geom_tile(ggplot2::aes(fill = support_category), colour = "white", linewidth = 0.8) +
  ggplot2::geom_tile(
    data = dplyr::filter(hm, support_category != "≥10 deaths"),
    fill = NA, colour = "#8C8C8C", linewidth = 0.3, linetype = "22", width = 0.94, height = 0.9
  ) +
  ggplot2::geom_text(ggplot2::aes(label = label, colour = support_category == "≥10 deaths"),
                     size = 2.1, lineheight = 0.9, family = "Arial") +
  ggplot2::scale_fill_manual(values = c("#2C6FA6", "#BCD7EC", "#F4F4F4"), drop = FALSE) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1A1A1A"), guide = "none") +
  ggplot2::scale_x_discrete(labels = age_labels, expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::labs(x = "Age group (years)", y = NULL,
                subtitle = "Chikungunya-attributed deaths (top) and confirmed cases (n) per cell") +
  theme_cid() +
  ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                 legend.position = "bottom", legend.justification = "left",
                 plot.subtitle = ggplot2::element_text(size = 7.5, colour = "#4D4D4D"))

save_all(fig_hm, "Supplement_age_profile_support_heatmap", width = 140, height = 110, svg = FALSE)

## ============================================================
## Supplement - original continuous curves coloured by local support
## ============================================================
curve <- readr::read_csv(file.path(out_dir, "original_curve_local_support_audit.csv"),
                         show_col_types = FALSE) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(
    core_profile = factor(core_profile, levels = core_profile_levels[-1]),
    support = factor(dplyr::case_when(
      local_support == ">=10 deaths within +/-5 y" ~ "≥10 deaths within ±5 y",
      local_support == "1-9 deaths within +/-5 y" ~ "1–9 deaths within ±5 y",
      TRUE ~ "0 deaths within ±5 y / outside death-age range"
    ), levels = c("≥10 deaths within ±5 y", "1–9 deaths within ±5 y",
                  "0 deaths within ±5 y / outside death-age range"))
  ) |>
  dplyr::arrange(core_profile, age_years) |>
  dplyr::group_by(core_profile) |>
  dplyr::mutate(run = cumsum(support != dplyr::lag(support, default = support[1]))) |>
  dplyr::ungroup()

## Extend each run by one age so adjacent segments join without gaps.
curve_seg <- curve |>
  dplyr::group_by(core_profile) |>
  dplyr::mutate(rr_next = dplyr::lead(rr), age_next = dplyr::lead(age_years)) |>
  dplyr::filter(!is.na(rr_next)) |>
  dplyr::ungroup()

gs_curve <- readr::read_csv(file.path(out_dir, "fs_global_curve_points.csv"),
                            show_col_types = FALSE) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels[-1]))

fig_curve <- ggplot2::ggplot(curve, ggplot2::aes(x = age_years)) +
  ggplot2::geom_hline(yintercept = 1, linewidth = 0.3, colour = "#8C8C8C") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(rr_lower, 0.05), ymax = pmin(rr_upper, 500)),
                       fill = "#0072B2", alpha = 0.12) +
  ggplot2::geom_segment(data = curve_seg,
                        ggplot2::aes(xend = age_next, y = rr, yend = rr_next, colour = support,
                                     linetype = support),
                        linewidth = 0.7) +
  ggplot2::geom_line(data = gs_curve, ggplot2::aes(y = rr), colour = "#1A1A1A",
                     linewidth = 0.4, linetype = "dotdash") +
  ggplot2::facet_wrap(~ core_profile, nrow = 2) +
  ggplot2::scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  ggplot2::coord_cartesian(ylim = c(0.05, 500)) +
  ggplot2::scale_colour_manual(values = c("#0072B2", "#E69F00", "#B0B0B0")) +
  ggplot2::scale_linetype_manual(values = c("solid", "solid", "22")) +
  ggplot2::labs(x = "Age (years)", y = "Risk ratio for death vs None (log scale)",
                subtitle = paste0("Original Figure 5 curves (fs model), segments shaded by local empirical support in that profile;\n",
                                  "black dot-dash = global-smooth alternative (shared s(age) + fs deviations, m = 1)")) +
  theme_cid() +
  ggplot2::theme(legend.position = "bottom", legend.justification = "left",
                 plot.subtitle = ggplot2::element_text(size = 7.5, colour = "#4D4D4D"))

save_all(fig_curve, "Supplement_original_curve_support_overlay", width = 180, height = 110, svg = FALSE)

## ============================================================
## Supplement - robustness of reported cells across specifications
## ============================================================
rob <- readr::read_csv(file.path(out_dir, "age_profile_robustness_estimates.csv"),
                       show_col_types = FALSE) |>
  dplyr::filter(age_scheme == "primary", core_profile != "None",
                model %in% c("fs_original", "no_interaction", "band_re",
                             "linear_interaction", "fs_global", "fs_sp_x4", "fs_age15plus"))

reported_cells <- est |>
  dplyr::filter(estimate_status == "Reported") |>
  dplyr::transmute(age_group = as.character(age_group), core_profile = as.character(core_profile))

model_labels <- c(
  fs_original = "Primary: factor-smooth (original)",
  fs_global = "Global s(age) + factor-smooth deviations",
  fs_sp_x4 = "Factor-smooth, 4× smoother",
  fs_age15plus = "Factor-smooth, ages ≥15 only",
  band_re = "Age-group × profile random effect",
  linear_interaction = "Linear age × profile",
  no_interaction = "No age interaction"
)
model_cols <- c("#1A1A1A", "#56B4E9", "#0072B2", "#009E73", "#E69F00", "#CC79A7", "#D55E00")

rob_plot <- rob |>
  dplyr::semi_join(reported_cells, by = c("age_group", "core_profile")) |>
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels),
    core_profile = factor(core_profile, levels = core_profile_levels[-1]),
    model = factor(model, levels = names(model_labels))
  )

fig_rob <- ggplot2::ggplot(rob_plot, ggplot2::aes(x = rr, y = forcats::fct_rev(core_profile),
                                                  colour = model)) +
  ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = "#8C8C8C") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
                          position = ggplot2::position_dodge(width = 0.75),
                          linewidth = 0.35) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.75), size = 1.1) +
  ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
  ggplot2::scale_x_log10(breaks = c(1, 3, 10, 30), labels = c("1", "3", "10", "30")) +
  ggplot2::scale_colour_manual(values = setNames(model_cols, names(model_labels)),
                               labels = model_labels, drop = TRUE) +
  ggplot2::scale_y_discrete(drop = TRUE) +
  ggplot2::labs(x = "Standardised risk ratio for death vs None (log scale)", y = NULL,
                subtitle = "Cells with ≥10 deaths only") +
  theme_cid() +
  ggplot2::theme(legend.position = "bottom", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = "#EDEDED"),
                 plot.subtitle = ggplot2::element_text(size = 7.5, colour = "#4D4D4D")) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

save_all(fig_rob, "Supplement_age_profile_robustness", width = 180, height = 100, svg = FALSE)

message("[47] figures written to ", out_dir)
