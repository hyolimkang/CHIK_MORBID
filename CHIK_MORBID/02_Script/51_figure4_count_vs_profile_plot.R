# ---------------------------------------------------------------------------
# 51_figure4_count_vs_profile_plot.R
#
# Draws main-manuscript Figure 4 from the CSVs written by
# 50_figure4_count_vs_profile.R (run that first). No model fitting here.
#
# Panel A: standardised absolute risk by specific profile (model C, points)
#          against the comorbidity-count estimate (model B, grey band = 95%
#          CI, dashed line = estimate), hospitalisation and death in rows,
#          count groups in columns.
# Panel B: paired 5-fold cross-validated improvements: count vs any (B vs A),
#          profile vs count (C vs B), and profile vs count after excluding all
#          CKD-containing records (sensitivity).
#
# Output: 03_Output/figure_revision/Figure4_count_vs_profile_main.pdf/.png/.svg
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

## Short strip labels: the full outcome names are given in the caption.
outcome_labels <- c(hosp = "Hospitalisation", death = "Death")
count_labels[["3"]] <- "Three
conditions"
outcome_order <- unname(outcome_labels)
sparse_events <- 10

std <- readr::read_csv(file.path(fig_rev_dir, "model_standardised_risks.csv"),
                       show_col_types = FALSE)
metrics <- readr::read_csv(file.path(fig_rev_dir, "model_comparison_metrics.csv"),
                           show_col_types = FALSE)

## ------------------------------------------------------------
## Panel A
## ------------------------------------------------------------
pa_c <- std |>
  dplyr::filter(model == "C") |>
  dplyr::mutate(
    outcome_label = factor(outcome_labels[outcome], levels = outcome_order),
    count = factor(n_conditions, levels = 0:3),
    count_label = factor(count_labels[as.character(count)], levels = count_labels),
    profile_label = factor(profile_display[level], levels = profile_display),
    sparse = n_events < sparse_events
  )
pa_b <- std |>
  dplyr::filter(model == "B") |>
  dplyr::mutate(
    outcome_label = factor(outcome_labels[outcome], levels = outcome_order),
    count = factor(level, levels = 0:3),
    count_label = factor(count_labels[as.character(count)], levels = count_labels)
  )

pct_lab <- function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")

fig4a <- ggplot2::ggplot(pa_c, ggplot2::aes(x = profile_label)) +
  ## Count model: spans the whole count-group column.
  ggplot2::geom_rect(data = pa_b, inherit.aes = FALSE,
                     ggplot2::aes(xmin = -Inf, xmax = Inf,
                                  ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                     fill = "#E3E3E3") +
  ggplot2::geom_hline(data = pa_b, ggplot2::aes(yintercept = std_risk_pct),
                      colour = "#6E6E6E", linewidth = 0.45, linetype = "22") +
  ## Profile model.
  ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct,
                                       colour = count), linewidth = 0.6) +
  ggplot2::geom_point(ggplot2::aes(y = std_risk_pct, colour = count,
                                   fill = ifelse(sparse, "white", as.character(count))),
                      shape = 21, size = 2.1, stroke = 0.6) +
  ggplot2::facet_grid(outcome_label ~ count_label, scales = "free", space = "free_x") +
  ggplot2::scale_y_log10(labels = pct_lab, breaks = c(0.03, 0.1, 0.3, 1, 3, 10, 30)) +
  ggplot2::scale_colour_manual(values = count_colours, guide = "none") +
  ggplot2::scale_fill_manual(values = c(count_colours, white = "white"), guide = "none") +
  ggplot2::labs(
    title = "A  Standardised risk by comorbidity count and specific profile",
    subtitle = paste0("Points: specific-profile model (95% CI); open point: <10 events. ",
                      "Grey band and dashed line: comorbidity-count model (95% CI and estimate)."),
    x = NULL, y = "Standardised risk (%, log scale)"
  ) +
  theme_rev() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 6.8, colour = ink),
    strip.text.y = ggplot2::element_text(angle = 270, size = 7.2),
    strip.text.x = ggplot2::element_text(size = 7.2, colour = ink_soft),
    panel.spacing.x = ggplot2::unit(2, "mm"),
    panel.spacing.y = ggplot2::unit(3, "mm"),
    plot.subtitle = ggplot2::element_text(size = 6.8)
  )

## ------------------------------------------------------------
## Panel B
## ------------------------------------------------------------
cmp_levels <- c("Count vs any condition", "Profile vs count",
                "Profile vs count, no CKD records")

pb <- metrics |>
  dplyr::filter(
    type != "pooled out-of-fold metric",
    metric %in% c("log_loss_relative_pct", "brier_relative_pct", "auroc"),
    (grepl(":primary", analysis) & comparison %in% c("B vs A", "C vs B")) |
      (grepl(":excluding_ckd", analysis) & comparison == "C vs B")
  ) |>
  dplyr::mutate(
    outcome = sub(":.*", "", analysis),
    outcome_label = factor(outcome_labels[outcome], levels = outcome_order),
    comparison_label = factor(dplyr::case_when(
      comparison == "B vs A" ~ cmp_levels[1],
      grepl("excluding", analysis) ~ cmp_levels[3],
      TRUE ~ cmp_levels[2]), levels = rev(cmp_levels)),
    metric_label = factor(dplyr::recode(metric,
      log_loss_relative_pct = "Log loss reduction (%)",
      brier_relative_pct = "Brier score reduction (%)",
      auroc = "AUROC gain (×100)"),
      levels = c("Log loss reduction (%)", "Brier score reduction (%)", "AUROC gain (×100)")),
    k = ifelse(metric == "auroc", 100, 1),
    est = value * k, lo = lower * k, hi = upper * k,
    sensitivity = grepl("excluding", analysis)
  )

cmp_cols <- setNames(c("#5B9BD5", "#0B2E59", "#0B2E59"), cmp_levels)

fig4b <- ggplot2::ggplot(pb, ggplot2::aes(y = comparison_label)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = ink_muted) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi, colour = comparison_label),
                          linewidth = 0.6) +
  ggplot2::geom_point(ggplot2::aes(x = est, colour = comparison_label,
                                   fill = ifelse(sensitivity, "white", as.character(comparison_label))),
                      shape = 21, size = 2, stroke = 0.6) +
  ggplot2::facet_grid(outcome_label ~ metric_label, scales = "free_x") +
  ggplot2::scale_colour_manual(values = cmp_cols, guide = "none") +
  ggplot2::scale_fill_manual(values = c(cmp_cols, white = "white"), guide = "none") +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::labs(
    title = "B  Incremental predictive performance, 5-fold cross-validation",
    subtitle = "Paired out-of-fold differences with 95% CI. Values right of 0 favour the more detailed representation.",
    x = "Improvement", y = NULL
  ) +
  theme_rev() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(colour = ink, size = 6.8),
    axis.ticks.y = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, size = 7.2),
    strip.text.x = ggplot2::element_text(size = 7.2, colour = ink_soft),
    panel.spacing.x = ggplot2::unit(4, "mm"),
    plot.margin = ggplot2::margin(4, 12, 4, 4),
    panel.spacing.y = ggplot2::unit(3, "mm"),
    plot.subtitle = ggplot2::element_text(size = 6.8)
  )

## wrap_elements: lay the panels out independently, so panel B's row labels
## do not push panel A's axis inwards.
fig4 <- patchwork::wrap_elements(full = fig4a) / patchwork::wrap_elements(full = fig4b) +
  patchwork::plot_layout(heights = c(2.2, 1))
save_figure(fig4, "Figure4_count_vs_profile_main", width = 183, height = 185)

message("[51] Figure 4 written to ", fig_rev_dir)
