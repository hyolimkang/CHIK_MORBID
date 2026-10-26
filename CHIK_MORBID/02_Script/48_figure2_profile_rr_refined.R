# ---------------------------------------------------------------------------
# 48_figure2_profile_rr_refined.R
#
# Refined main-manuscript Figure 2: overall standardised RR vs None for the
# eight joint core-3 profiles, hospitalisation (A) and chikungunya-attributed
# death (B).
#
# No refitting. Reuses the validated, cached estimates from
# 12_core3_condition_specific_rr.R section 8 (refreshed by script 28 after the
# Gate 3.1 outcome correction):
#   01_Data/core_profile_rr_hosp.RData   (hospitalisation cohort, N = 776,072)
#   01_Data/core_profile_rr_death.RData  (mortality cohort,       N = 983,947)
# Each is a GAM g-computation estimate standardised over the outcome cohort's
# age x sex x year x state distribution, with 95% CIs from 2,000 parametric
# coefficient draws.
#
# Output (03_Output/figure_revision/): Figure2_profile_RR_refined.pdf/.png/.svg,
# figure2_plotted_values.csv. The original fig_core_profile_rr_*.pdf/.png in
# 03_Output/figures are not touched.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

load("01_Data/core_profile_rr_hosp.RData")
load("01_Data/core_profile_rr_death.RData")

## Guard against silently plotting a pre-Gate-3.1 cache.
stopifnot(sum(core_profile_rr_hosp$n_profile) == 776072,
          sum(core_profile_rr_hosp$n_events_profile) == 25363,
          sum(core_profile_rr_death$n_profile) == 983947,
          sum(core_profile_rr_death$n_events_profile) == 1050)

## Same screening heuristic as the Figure 5 validation: fewer than 10 events
## is drawn as an open marker (shown, but visibly de-emphasised).
sparse_events <- 10

fig2_data <- dplyr::bind_rows(
  dplyr::mutate(core_profile_rr_hosp, outcome = "Hospitalisation"),
  dplyr::mutate(core_profile_rr_death, outcome = "Chikungunya-attributed death")
) |>
  dplyr::mutate(
    core_profile = as.character(core_profile),
    count = factor(profile_count[core_profile], levels = 0:3),
    ## The reference row needs no group label (it would just repeat "None").
    count_label = factor(ifelse(count == "0", "", count_labels[as.character(count)]),
                         levels = c("", count_labels[-1])),
    profile_label = factor(profile_display[core_profile], levels = rev(profile_display)),
    sparse = n_events_profile < sparse_events,
    reference = core_profile == "None",
    rr_text = ifelse(reference, "1 (reference)", fmt_ci(rr, rr_lower, rr_upper)),
    events_text = paste0(fmt_int(n_events_profile), " / ", fmt_int(n_profile))
  )

readr::write_csv(
  fig2_data |> dplyr::select(outcome, core_profile, count, n_profile, n_events_profile,
                             standardised_risk, standardised_risk_lower,
                             standardised_risk_upper, rr, rr_lower, rr_upper, sparse),
  file.path(fig_rev_dir, "figure2_plotted_values.csv")
)

forest_panel <- function(outcome_name, title, breaks, limits, show_y = TRUE) {
  d <- dplyr::filter(fig2_data, outcome == outcome_name)
  ggplot2::ggplot(d, ggplot2::aes(y = profile_label)) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
    ggplot2::geom_linerange(
      data = dplyr::filter(d, !reference),
      ggplot2::aes(xmin = rr_lower, xmax = rr_upper, colour = count),
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = rr, colour = count, fill = ifelse(sparse, "white", as.character(count))),
      shape = 21, size = 2, stroke = 0.6
    ) +
    ggplot2::facet_grid(count_label ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_log10(breaks = breaks, labels = as.character(breaks),
                           limits = limits, expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_colour_manual(values = count_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = c(count_colours, white = "white"), guide = "none") +
    ggplot2::labs(title = title, x = "Standardised risk ratio vs None (log scale)", y = NULL) +
    theme_rev() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = if (show_y) ggplot2::element_text(angle = 0, hjust = 1, face = "plain",
                                                            colour = ink_muted, size = 6.8)
                          else ggplot2::element_blank(),
      axis.text.y = if (show_y) ggplot2::element_text(colour = ink, size = 7.5)
                    else ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1.6, "mm"),
      plot.margin = ggplot2::margin(4, 2, 4, 4)
    )
}

table_panel <- function(outcome_name, event_word) {
  d <- dplyr::filter(fig2_data, outcome == outcome_name)
  long <- dplyr::bind_rows(
    dplyr::transmute(d, profile_label, count_label, col = 1, text = rr_text, sparse),
    dplyr::transmute(d, profile_label, count_label, col = 2, text = events_text, sparse)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = col, y = profile_label, label = text)) +
    ggplot2::geom_text(ggplot2::aes(colour = ifelse(col == 2 | sparse, "muted", "ink")),
                       hjust = 0, size = 2.25, family = "Arial") +
    ggplot2::facet_grid(count_label ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = c(ink = ink, muted = ink_muted), guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(1, 2.75), breaks = c(1, 2),
      labels = c("RR (95% CI)", paste0(event_word, " / cases")),
      position = "top", expand = c(0, 0)
    ) +
    ggplot2::labs(title = " ", x = NULL, y = NULL) +
    theme_rev() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(hjust = 0, colour = ink_soft, size = 6.8,
                                              face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1.6, "mm"),
      plot.margin = ggplot2::margin(4, 4, 4, 2)
    )
}

p_a <- forest_panel("Hospitalisation", "A  Hospitalisation",
                    breaks = c(1, 1.5, 2, 3, 5), limits = c(0.95, 5.6))
t_a <- table_panel("Hospitalisation", "Hosp.")
p_b <- forest_panel("Chikungunya-attributed death", "B  Chikungunya-attributed death",
                    breaks = c(1, 2, 5, 10, 20), limits = c(0.95, 27))
t_b <- table_panel("Chikungunya-attributed death", "Deaths")

## Outcomes stacked (A over B), each with its own aligned numeric columns:
## side-by-side panels at journal width leave too little room for the text.
fig2 <- (p_a | t_a) / (p_b | t_b) +
  patchwork::plot_layout(widths = c(1.75, 1))

save_figure(fig2, "Figure2_profile_RR_refined", width = 170, height = 150)

message("[48] Figure 2 written to ", fig_rev_dir)
