# ---------------------------------------------------------------------------
# 57_within_count_figures.R
#
# Figure 4 candidates for the within-count analysis (inputs from script 56):
#   Candidate A - profile-specific standardised risks against the
#                 comorbidity-count model (dashed line, grey 95% CI band),
#                 one- and two-condition profiles only.
#   Candidate B - direct within-count contrasts: standardised risk
#                 difference (percentage points) with 95% CI, forest plot.
# Panels: A Hospitalisation, B Chikungunya-attributed death. No overall
# title. PDF, SVG and 600 dpi PNG.
#
# Output: 03_Output/figure_revision/within_count/
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")
out_dir <- file.path(fig_rev_dir, "within_count")
sparse_events <- 10

## Counts/levels are categories, not numbers.
risks <- readr::read_csv(file.path(out_dir, "within_count_standardised_risks.csv"), show_col_types = FALSE,
                         col_types = readr::cols(n_conditions = "c", level = "c"))
con <- readr::read_csv(file.path(out_dir, "within_count_direct_contrasts.csv"), show_col_types = FALSE)

outcome_titles <- c(hosp = "Hospitalisation", death = "Chikungunya-attributed death")
group_levels <- c("One condition", "Two conditions")
pct_lab <- function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")

panel_theme <- function() {
  theme_rev(base_size = 8) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 11, colour = ink),
      plot.tag.position = "topleft",
      plot.title = ggplot2::element_text(face = "bold", size = 8.6, colour = ink,
                                         margin = ggplot2::margin(b = 5)),
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(face = "plain", size = 7.6, colour = ink_soft),
      plot.margin = ggplot2::margin(5, 8, 4, 5)
    )
}

## ============================================================
## Candidate A - profile risks versus the count model
## ============================================================
cand_a_panel <- function(o, tag, breaks) {
  prof <- risks |>
    dplyr::filter(outcome == o, model == "profile", n_conditions %in% c("1", "2")) |>
    dplyr::mutate(group = factor(ifelse(n_conditions == "1", group_levels[1], group_levels[2]),
                                 levels = group_levels),
                  ## two-line labels so three two-condition profiles fit side by side
                  profile_label = factor(sub(" + ", " +\n", profile_display[level], fixed = TRUE),
                                         levels = sub(" + ", " +\n", profile_display, fixed = TRUE)),
                  sparse = n_events < sparse_events)
  cnt <- risks |>
    dplyr::filter(outcome == o, model == "count", level %in% c("1", "2")) |>
    dplyr::mutate(group = factor(ifelse(level == "1", group_levels[1], group_levels[2]), levels = group_levels))
  ggplot2::ggplot(prof, ggplot2::aes(x = profile_label)) +
    ggplot2::geom_rect(data = cnt, inherit.aes = FALSE,
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = lower_pct, ymax = upper_pct),
                       fill = "#E6E6E6") +
    ggplot2::geom_hline(data = cnt, ggplot2::aes(yintercept = std_risk_pct),
                        linetype = "22", linewidth = 0.4, colour = "#6E6E6E") +
    ## Direct label so the band is not mistaken for the no-condition group.
    ggplot2::geom_text(data = dplyr::filter(cnt, group == group_levels[1]), inherit.aes = FALSE,
                       ggplot2::aes(x = Inf, y = upper_pct, label = "Count model"),
                       hjust = 1.05, vjust = -0.5, size = 2.1, colour = "#6E6E6E", family = "Arial") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower_pct, ymax = upper_pct, colour = n_conditions,
                                         linetype = ifelse(sparse, "22", "solid")), linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(y = std_risk_pct, colour = n_conditions,
                                     fill = ifelse(sparse, "open", n_conditions)),
                        shape = 21, size = 2, stroke = 0.6) +
    ggplot2::facet_grid(. ~ group, scales = "free_x", space = "free_x") +
    ggplot2::scale_y_log10(breaks = breaks, labels = pct_lab) +
    ggplot2::scale_colour_manual(values = count_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = c(count_colours, open = "white"), guide = "none") +
    ggplot2::scale_linetype_identity() +
    ggplot2::labs(title = paste0(tag, "  ", outcome_titles[[o]]), x = NULL,
                  y = "Standardised risk (%, log scale)") +
    panel_theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
                   axis.text.x = ggplot2::element_text(colour = ink, size = 7.6),
                   panel.spacing.x = ggplot2::unit(4, "mm"))
}

ca <- (cand_a_panel("hosp", "A", c(3, 5, 10, 15, 20)) |
         cand_a_panel("death", "B", c(0.1, 0.2, 0.3, 0.5, 1, 2))) +
  patchwork::plot_layout(widths = c(1, 1))
save_figure(ca, "Figure4_candidate_A_count_vs_profile_risks", width = 183, height = 82, dir = out_dir)

## ============================================================
## Candidate B - direct within-count contrasts (risk difference)
## ============================================================
con_lab <- function(a, b) paste0(profile_display[a], " vs ", profile_display[b])
## Per-row decimals (hospitalisation 1 dp, death 2 dp); formatC takes one digits value.
fmt_dp <- function(x, dp) mapply(function(v, k) formatC(v, format = "f", digits = k), x, dp)
con <- con |>
  dplyr::mutate(
    group = factor(group, levels = group_levels),
    label = con_lab(profile_a, profile_b),
    ## Pre-specified display order within each group.
    label = factor(label, levels = rev(c(con_lab("DM", "HTN"), con_lab("CKD", "HTN"), con_lab("CKD", "DM"),
                                         con_lab("DM+CKD", "DM+HTN"), con_lab("HTN+CKD", "DM+HTN"),
                                         con_lab("HTN+CKD", "DM+CKD")))),
    dp = ifelse(outcome == "death", 2, 1),
    rd_text = paste0(fmt_dp(rd_pp, dp), " (", fmt_dp(rd_lower_pp, dp), " to ", fmt_dp(rd_upper_pp, dp), ")"),
    rd_text = gsub("-", "−", rd_text)
  )

## One direction cue in the top facet: "first profile lower / higher" read
## left and right of zero (the axis title names first-minus-second).
cue_data <- function(d, label, x, hjust) {
  data.frame(group = factor(group_levels[1], levels = group_levels), x = x, y = Inf,
             label = label, hjust = hjust)
}

cand_b_panel <- function(o, tag, show_labels) {
  d <- dplyr::filter(con, outcome == o)
  rng <- range(c(0, d$rd_lower_pp, d$rd_upper_pp))
  forest <- ggplot2::ggplot(d, ggplot2::aes(y = label)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.35, colour = ink_muted) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = rd_lower_pp, xmax = rd_upper_pp,
                                         colour = ifelse(sparse, "muted", "ink"),
                                         linetype = ifelse(sparse, "22", "solid")), linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(x = rd_pp, colour = ifelse(sparse, "muted", "ink"),
                                     fill = ifelse(sparse, "white", "#1F5FA8")),
                        shape = 21, size = 2, stroke = 0.6) +
    ## Direction cue, drawn once (top facet only) on either side of RD = 0.
    ggplot2::geom_text(data = cue_data(d, "← first lower", 0, 1.1), inherit.aes = FALSE,
                       ggplot2::aes(x = x, y = y, label = label, hjust = hjust), vjust = -0.7,
                       size = 2, colour = ink_muted, family = "Arial") +
    ggplot2::geom_text(data = cue_data(d, "first higher →", 0, -0.1), inherit.aes = FALSE,
                       ggplot2::aes(x = x, y = y, label = label, hjust = hjust), vjust = -0.7,
                       size = 2, colour = ink_muted, family = "Arial") +
    ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = c(ink = "#1F5FA8", muted = "#9A9A9A"), guide = "none") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.06)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Risk difference, first minus second\nprofile (percentage points)", y = NULL) +
    panel_theme() +
    ggplot2::theme(
      axis.text.y = if (show_labels) ggplot2::element_text(colour = ink, size = 7.4) else ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 270, size = 7.2, colour = ink_soft),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
      panel.spacing.y = ggplot2::unit(3, "mm"),
      plot.margin = ggplot2::margin(14, 2, 4, 5)
    )
  txt <- ggplot2::ggplot(d, ggplot2::aes(x = 0, y = label, label = rd_text)) +
    ggplot2::geom_text(ggplot2::aes(colour = ifelse(sparse, "muted", "ink")), hjust = 0, size = 2.2,
                       family = "Arial") +
    ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = c(ink = ink, muted = "#9A9A9A"), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0), position = "top",
                                breaks = 0, labels = "RD (95% CI)") +
    ggplot2::labs(x = NULL, y = NULL) +
    panel_theme() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x.top = ggplot2::element_text(hjust = 0, size = 6.8, face = "bold", colour = ink_soft),
                   strip.text.y = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   panel.spacing.y = ggplot2::unit(3, "mm"),
                   plot.margin = ggplot2::margin(14, 6, 4, 2))
  ## The panel title spans forest + text column, so it is never clipped by
  ## the narrower forest alone.
  patchwork::wrap_elements(full = (forest | txt) +
                             patchwork::plot_layout(widths = c(if (show_labels) 2.1 else 1.35, 0.95))) +
    ggplot2::labs(title = paste0(tag, "  ", outcome_titles[[o]])) +
    ggplot2::theme(plot.title = ggplot2::element_text(family = "Arial", face = "bold", size = 8.6,
                                                      colour = ink, margin = ggplot2::margin(l = 4, t = 2)))
}

cb <- (cand_b_panel("hosp", "A", TRUE) | cand_b_panel("death", "B", FALSE)) +
  patchwork::plot_layout(widths = c(1.25, 1))
save_figure(cb, "Figure4_candidate_B_within_count_contrasts", width = 183, height = 78, dir = out_dir)

message("[57] candidate figures written to ", out_dir)
