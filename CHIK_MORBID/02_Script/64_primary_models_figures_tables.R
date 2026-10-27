# ---------------------------------------------------------------------------
# 64_primary_models_figures_tables.R
#
# Main Figures 1-3, Table 2 and the three title/legend Word files, drawn
# ONLY from the canonical primary-model estimates written by script 62
# (hospitalisation_H2, mortality_H0). Visual designs are those of the
# previous final figures (scripts 48, 54, 58) and Table 2 (script 60); only
# data sources, axis ranges (widened where estimates require) and legends
# change. Nothing here refits or alters an estimate.
#
# Output: 03_Output/primary_models_H2_H0_final/{figures,tables,legends,supplementary}
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(officer)
  library(flextable)
})

source("02_Script/figure_revision_theme.R")
source("02_Script/primary_models_functions.R")
fig_dir <- file.path(pm_dir, "figures"); tab_dir <- file.path(pm_dir, "tables"); leg_dir <- file.path(pm_dir, "legends")
for (d in c(fig_dir, tab_dir, leg_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
log64 <- function(...) pm_log("64", ...)

out_lab <- c(hospitalisation_H2 = "Hospitalisation", mortality_H0 = "Chikungunya-attributed death")
risks <- readr::read_csv(file.path(pm_dir, "estimates", "overall_standardised_risks.csv"), show_col_types = FALSE)
rrs <- readr::read_csv(file.path(pm_dir, "estimates", "overall_standardised_RRs.csv"), show_col_types = FALSE)
age_est <- readr::read_csv(file.path(pm_dir, "supplementary", "age_specific_profile_estimates.csv"), show_col_types = FALSE)
rds <- readr::read_csv(file.path(pm_dir, "estimates", "within_count_standardised_RDs.csv"), show_col_types = FALSE)

overall <- dplyr::inner_join(risks, dplyr::select(rrs, model_id, core_profile, rr, rr_lower, rr_upper),
                             by = c("model_id", "core_profile"))
stopifnot(nrow(overall) == 16, setequal(unique(overall$model_id), names(out_lab)),
          all(table(overall$model_id) == 8))

## Axis helper: keep the previous window unless an interval would not fit.
widen_log <- function(old, lo, hi) c(if (min(lo) < old[1]) min(lo) * 0.92 else old[1],
                                     if (max(hi) > old[2]) max(hi) * 1.08 else old[2])

## ============================================================
## Figure 1 - overall standardised RRs (design: script 48)
## ============================================================
sparse_events <- 10
fig1_data <- overall |>
  dplyr::mutate(
    outcome = out_lab[model_id],
    count = factor(profile_count[core_profile], levels = 0:3),
    count_label = factor(ifelse(count == "0", "", count_labels[as.character(count)]),
                         levels = c("", count_labels[-1])),
    profile_label = factor(profile_display[core_profile], levels = rev(profile_display)),
    sparse = n_events < sparse_events,
    reference = core_profile == "None",
    rr_text = ifelse(reference, "1 (reference)", fmt_ci(rr, rr_lower, rr_upper)),
    events_text = paste0(fmt_int(n_events), " / ", fmt_int(n_cases)))

forest_panel <- function(outcome_name, title, breaks, limits, show_y = TRUE) {
  d <- dplyr::filter(fig1_data, outcome == outcome_name)
  stopifnot(min(d$rr_lower) >= limits[1], max(d$rr_upper) <= limits[2])
  ggplot2::ggplot(d, ggplot2::aes(y = profile_label)) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
    ggplot2::geom_linerange(data = dplyr::filter(d, !reference),
                            ggplot2::aes(xmin = rr_lower, xmax = rr_upper, colour = count), linewidth = 0.55) +
    ggplot2::geom_point(ggplot2::aes(x = rr, colour = count, fill = ifelse(sparse, "white", as.character(count))),
                        shape = 21, size = 2, stroke = 0.6) +
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
      axis.text.y = if (show_y) ggplot2::element_text(colour = ink, size = 7.5) else ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1.6, "mm"), plot.margin = ggplot2::margin(4, 2, 4, 4))
}
table_panel <- function(outcome_name, event_word) {
  d <- dplyr::filter(fig1_data, outcome == outcome_name)
  long <- dplyr::bind_rows(
    dplyr::transmute(d, profile_label, count_label, col = 1, text = rr_text, sparse),
    dplyr::transmute(d, profile_label, count_label, col = 2, text = events_text, sparse))
  ggplot2::ggplot(long, ggplot2::aes(x = col, y = profile_label, label = text)) +
    ggplot2::geom_text(ggplot2::aes(colour = ifelse(col == 2 | sparse, "muted", "ink")),
                       hjust = 0, size = 2.25, family = "Arial") +
    ggplot2::facet_grid(count_label ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = c(ink = ink, muted = ink_muted), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(1, 2.75), breaks = c(1, 2),
                                labels = c("RR (95% CI)", paste0(event_word, " / cases")),
                                position = "top", expand = c(0, 0)) +
    ggplot2::labs(title = " ", x = NULL, y = NULL) +
    theme_rev() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(hjust = 0, colour = ink_soft, size = 6.8, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(), strip.text.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1.6, "mm"), plot.margin = ggplot2::margin(4, 4, 4, 2))
}
h1 <- dplyr::filter(fig1_data, model_id == "hospitalisation_H2")
m1 <- dplyr::filter(fig1_data, model_id == "mortality_H0")
lim_h1 <- widen_log(c(0.95, 5.6), h1$rr_lower, h1$rr_upper)
lim_m1 <- widen_log(c(0.95, 27), m1$rr_lower, m1$rr_upper)
brk_h1 <- c(1, 1.5, 2, 3, 5, 7, 10); brk_h1 <- brk_h1[brk_h1 <= lim_h1[2]]
brk_m1 <- c(1, 2, 5, 10, 20, 50); brk_m1 <- brk_m1[brk_m1 <= lim_m1[2]]
p_a <- forest_panel("Hospitalisation", "A  Hospitalisation", breaks = brk_h1, limits = lim_h1)
t_a <- table_panel("Hospitalisation", "Hosp.")
p_b <- forest_panel("Chikungunya-attributed death", "B  Chikungunya-attributed death", breaks = brk_m1, limits = lim_m1)
t_b <- table_panel("Chikungunya-attributed death", "Deaths")
fig1 <- (p_a | t_a) / (p_b | t_b) + patchwork::plot_layout(widths = c(1.75, 1))
save_figure(fig1, "Figure1_Overall_RR_FINAL", width = 170, height = 150, dir = fig_dir)
fig1_plotted <- fig1_data |>
  dplyr::select(outcome, model_id, core_profile, n_cases, n_events, risk, risk_lower, risk_upper,
                rr, rr_lower, rr_upper, sparse, rr_text, events_text)
readr::write_csv(fig1_plotted, file.path(fig_dir, "Figure1_plotted_values.csv"))
log64("Figure 1 axes: hosp %.2f-%.2f, death %.2f-%.2f", lim_h1[1], lim_h1[2], lim_m1[1], lim_m1[2])

## ============================================================
## Figure 2 - age-specific absolute risks (design: script 54, combined)
## ============================================================
min_events <- 10
exclude_child_profiles <- FALSE   # unchanged display policy (script 54)
fig2_cells <- age_est |>
  dplyr::transmute(model_id, outcome = out_lab[model_id], age_group, core_profile, n_cases, n_events,
                   n_nonevents, std_risk_pct, std_risk_lower_pct, std_risk_upper_pct) |>
  dplyr::mutate(shown = core_profile == "None" |
                  (n_events >= min_events & n_nonevents >= min_events &
                     !(exclude_child_profiles & age_group == "0-14")),
                age_group = factor(age_group, levels = age_levels),
                core_profile = factor(core_profile, levels = core_profile_levels))
pal <- profile_colours
shp <- c("None" = 21, "DM" = 24, "HTN" = 22, "CKD" = 23, "DM+HTN" = 25, "DM+CKD" = 4, "HTN+CKD" = 23, "DM+HTN+CKD" = 21)
fill_solid <- c("None" = TRUE, "DM" = FALSE, "HTN" = FALSE, "CKD" = FALSE, "DM+HTN" = FALSE,
                "DM+CKD" = FALSE, "HTN+CKD" = TRUE, "DM+HTN+CKD" = TRUE)
st <- list(line_w = 0.35, line_alpha = 0.4, ci_w = 0.4, ci_alpha = 0.75, pt = 1.55, stroke = 0.55)
risk_panel <- function(d, tag, y_lab, breaks, limits, title) {
  pd <- dplyr::filter(d, shown) |> droplevels()
  stopifnot(min(pd$std_risk_lower_pct) >= limits[1], max(pd$std_risk_upper_pct) <= limits[2])
  pd <- pd |> dplyr::group_by(core_profile) |> dplyr::arrange(age_group, .by_group = TRUE) |>
    dplyr::mutate(run = cumsum(c(1, diff(as.integer(age_group)) != 1))) |> dplyr::ungroup()
  offsets <- setNames(seq(-0.3, 0.3, length.out = 8), core_profile_levels)
  pd <- dplyr::mutate(pd, x = as.integer(age_group) + offsets[as.character(core_profile)])
  profs <- levels(pd$core_profile)
  ggplot2::ggplot(pd, ggplot2::aes(x = x, y = std_risk_pct, colour = core_profile, shape = core_profile,
                                   group = core_profile)) +
    ggplot2::geom_line(ggplot2::aes(group = interaction(core_profile, run)), linewidth = st$line_w, alpha = st$line_alpha) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                            linewidth = st$ci_w, alpha = st$ci_alpha) +
    ggplot2::geom_point(ggplot2::aes(fill = core_profile), size = st$pt, stroke = st$stroke) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_levels), labels = unname(age_labels[age_levels]),
                                limits = c(0.55, length(age_levels) + 0.45), expand = c(0, 0)) +
    ggplot2::scale_y_log10(breaks = breaks, limits = limits,
                           labels = function(x) paste0(format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE), "%")) +
    ggplot2::scale_colour_manual(values = pal[profs], labels = profile_display[profs], name = NULL) +
    ggplot2::scale_shape_manual(values = shp[profs], labels = profile_display[profs], name = NULL) +
    ggplot2::scale_fill_manual(values = ifelse(fill_solid[profs], pal[profs], "white"),
                               labels = profile_display[profs], name = NULL) +
    ggplot2::labs(tag = tag, title = title, x = "Age group (years)", y = y_lab) +
    theme_rev(base_size = 8.5) +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 7.4), legend.key.width = ggplot2::unit(4.5, "mm"),
      axis.text.x = ggplot2::element_text(size = 8.5, colour = ink, face = "bold", margin = ggplot2::margin(t = 3)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, colour = "#E6E6E6"),
      panel.grid.minor.y = ggplot2::element_blank())
}
h2c <- dplyr::filter(fig2_cells, model_id == "hospitalisation_H2")
m2c <- dplyr::filter(fig2_cells, model_id == "mortality_H0")
sh <- dplyr::filter(h2c, shown); sm <- dplyr::filter(m2c, shown)
lim_h2 <- widen_log(c(1.8, 36), sh$std_risk_lower_pct, sh$std_risk_upper_pct)
lim_m2 <- widen_log(c(0.02, 14), sm$std_risk_lower_pct, sm$std_risk_upper_pct)
brk_h2 <- c(1, 2, 3, 5, 10, 20, 30, 50); brk_h2 <- brk_h2[brk_h2 >= lim_h2[1] & brk_h2 <= lim_h2[2]]
brk_m2 <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30); brk_m2 <- brk_m2[brk_m2 >= lim_m2[1] & brk_m2 <= lim_m2[2]]
cA <- risk_panel(h2c, "A", "Hospitalisation risk (%, log scale)", brk_h2, lim_h2,
                 "Hospitalisation risk by age group and multimorbidity profile")
cB <- risk_panel(m2c, "B", "Death risk (%, log scale)", brk_m2, lim_m2,
                 "Death risk by age group and multimorbidity profile")
fig2 <- (cA | cB) + patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 8.1, colour = ink, margin = ggplot2::margin(b = 6)),
    plot.title.position = "plot", plot.tag.position = "topleft",
    plot.tag = ggplot2::element_text(face = "bold", size = 12, colour = ink, margin = ggplot2::margin(r = 2)),
    legend.position = "bottom", legend.justification = "center", legend.box.margin = ggplot2::margin(t = 2),
    legend.text = ggplot2::element_text(size = 7.6, margin = ggplot2::margin(r = 6)),
    plot.margin = ggplot2::margin(6, 8, 4, 6)) &
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 0.45)))
save_figure(fig2, "Figure2_Age_Specific_Risks_FINAL", width = 183, height = 95, dir = fig_dir)
readr::write_csv(fig2_cells |> dplyr::mutate(panel = ifelse(model_id == "hospitalisation_H2", "A", "B"), .before = 1),
                 file.path(fig_dir, "Figure2_cells_and_display_status.csv"))
log64("Figure 2 axes: hosp %.3f-%.1f, death %.3f-%.2f; cells shown A = %d, B = %d",
      lim_h2[1], lim_h2[2], lim_m2[1], lim_m2[2], sum(h2c$shown), sum(m2c$shown))

## ============================================================
## Figure 3 - within-count RDs (design: script 58)
## ============================================================
rows <- tibble::tribble(
  ~profile_a, ~profile_b, ~group,           ~y,
  NA,         NA,         "One condition",  8,
  "DM",       "HTN",      "One condition",  7,
  "CKD",      "HTN",      "One condition",  6,
  "CKD",      "DM",       "One condition",  5,
  NA,         NA,         "Two conditions", 3.4,
  "DM+CKD",   "DM+HTN",   "Two conditions", 2.4,
  "HTN+CKD",  "DM+HTN",   "Two conditions", 1.4,
  "HTN+CKD",  "DM+CKD",   "Two conditions", 0.4)
contrast_rows <- dplyr::filter(rows, !is.na(profile_a))
d3 <- rds |>
  dplyr::mutate(outcome = ifelse(model_id == "hospitalisation_H2", "hosp", "death")) |>
  dplyr::inner_join(contrast_rows, by = c("profile_a", "profile_b", "group"))
stopifnot(nrow(d3) == 12, all(table(d3$outcome) == 6))
## RD = risk(first) - risk(second) from the canonical unrounded overall risks.
rk <- dplyr::transmute(risks, model_id, core_profile, risk_pct = 100 * risk)
chk3 <- d3 |>
  dplyr::left_join(dplyr::rename(rk, profile_a = core_profile, ra = risk_pct), by = c("model_id", "profile_a")) |>
  dplyr::left_join(dplyr::rename(rk, profile_b = core_profile, rb = risk_pct), by = c("model_id", "profile_b"))
stopifnot(max(abs(chk3$rd_pp - (chk3$ra - chk3$rb))) < 1e-10)
stopifnot(identical(d3$sparse, pmin(d3$n_events_a, d3$n_events_b) < 10))
dp <- function(o) ifelse(o == "death", 2, 1)
fmt <- function(x, k) mapply(function(v, kk) formatC(v, format = "f", digits = kk), x, k)
d3 <- d3 |> dplyr::mutate(k = dp(outcome),
                          rd_text_ascii = paste0(fmt(rd_pp, k), " (", fmt(rd_lower_pp, k), " to ", fmt(rd_upper_pp, k), ")"),
                          rd_text = gsub("-", "−", rd_text_ascii))

y_lim <- c(-0.25, 8.6); blue <- "#1F4E89"; grey_sparse <- "#9A9A9A"
lab <- function(a, b) paste0(gsub("+", " + ", a, fixed = TRUE), " vs ", gsub("+", " + ", b, fixed = TRUE))
heading_rows <- dplyr::filter(rows, is.na(profile_a))
label_rows <- contrast_rows |> dplyr::mutate(label = lab(profile_a, profile_b))
base_theme <- function() {
  ggplot2::theme_void(base_family = "Arial") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 8.6, colour = ink, hjust = 0,
                                                      margin = ggplot2::margin(b = 4)),
                   plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
p_labels <- ggplot2::ggplot() +
  ggplot2::geom_text(data = heading_rows, ggplot2::aes(x = 0, y = y, label = group), hjust = 0, size = 2.75,
                     fontface = "bold", colour = ink_soft, family = "Arial") +
  ggplot2::geom_text(data = label_rows, ggplot2::aes(x = 0.06, y = y, label = label), hjust = 0, size = 2.75,
                     colour = ink, family = "Arial") +
  ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
  ggplot2::labs(title = "Profile comparison") + base_theme() +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 6, 6))
nice_limits <- function(lo, hi, step, old) c(min(old[1], floor(lo / step) * step), max(old[2], ceiling(hi / step) * step))
outcome_panel <- function(o, title, limits, breaks) {
  text_x <- limits[2] + 0.057 * diff(limits)
  pd <- dplyr::filter(d3, outcome == o) |>
    dplyr::mutate(col = ifelse(sparse, grey_sparse, blue), fill = ifelse(sparse, "white", blue))
  stopifnot(min(pd$rd_lower_pp) >= limits[1], max(pd$rd_upper_pp) <= limits[2])
  ggplot2::ggplot(pd, ggplot2::aes(y = y)) +
    ggplot2::geom_vline(xintercept = breaks, linewidth = 0.2, colour = "#EBEBEB") +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "#7A7A7A") +
    ggplot2::geom_segment(ggplot2::aes(x = rd_lower_pp, xend = rd_upper_pp, yend = y, colour = col), linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = rd_pp, colour = col, fill = fill), shape = 21, size = 1.9, stroke = 0.6) +
    ggplot2::geom_text(ggplot2::aes(x = text_x, label = rd_text, colour = col), hjust = 0, size = 2.6, family = "Arial") +
    ggplot2::annotate("text", x = text_x, y = heading_rows$y[1], label = "RD (95% CI)", hjust = 0,
                      size = 2.6, fontface = "bold", colour = ink_soft, family = "Arial") +
    ggplot2::scale_colour_identity() + ggplot2::scale_fill_identity() +
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
      axis.title.x = ggplot2::element_text(size = 7.6, colour = ink, margin = ggplot2::margin(t = 4), lineheight = 1.05),
      plot.margin = ggplot2::margin(6, 85, 6, 4))
}
dh <- dplyr::filter(d3, outcome == "hosp"); dm <- dplyr::filter(d3, outcome == "death")
lim_h3 <- nice_limits(min(dh$rd_lower_pp), max(dh$rd_upper_pp), 2, c(-4, 10))
lim_m3 <- nice_limits(min(dm$rd_lower_pp), max(dm$rd_upper_pp), 0.5, c(-1, 1.5))
p_hosp <- outcome_panel("hosp", "Hospitalisation", lim_h3, seq(lim_h3[1], lim_h3[2], 2))
p_death <- outcome_panel("death", "Chikungunya-attributed death", lim_m3, seq(lim_m3[1], lim_m3[2], 0.5))
fig3 <- (p_labels | p_hosp | p_death) + patchwork::plot_layout(widths = c(0.62, 1, 1))
save_figure(fig3, "Figure3_Within_Count_RD_FINAL", width = 250, height = 84, dir = fig_dir)
readr::write_csv(d3 |> dplyr::transmute(outcome = out_lab[model_id], model_id, group, comparison = lab(profile_a, profile_b),
                                        profile_a, profile_b, risk_a_pct, risk_b_pct, rd_pp, rd_lower_pp, rd_upper_pp,
                                        rd_lower_simult_pp, rd_upper_simult_pp, rr, rr_lower, rr_upper,
                                        n_cases_a, n_events_a, n_cases_b, n_events_b,
                                        sparse_either_profile_lt10_events = sparse, displayed_text = rd_text_ascii),
                 file.path(fig_dir, "Figure3_plotted_values.csv"))
log64("Figure 3 axes: hosp %g to %g, death %g to %g", lim_h3[1], lim_h3[2], lim_m3[1], lim_m3[2])

## ============================================================
## Table 2 (design: script 60)
## ============================================================
profile_labels <- c(None = "None (no DM, HTN or CKD)", DM = "DM only", HTN = "HTN only", CKD = "CKD only",
                    `DM+HTN` = "DM + HTN", `DM+CKD` = "DM + CKD", `HTN+CKD` = "HTN + CKD", `DM+HTN+CKD` = "DM + HTN + CKD")
pct <- function(x) sprintf("%.2f", 100 * x)
t2v <- overall |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels),
                model_id = factor(model_id, levels = names(out_lab))) |>
  dplyr::arrange(model_id, core_profile) |>
  dplyr::mutate(outcome_label = out_lab[as.character(model_id)],
                profile_label = profile_labels[as.character(core_profile)],
                risk_text = paste0(pct(risk), " (", pct(risk_lower), "–", pct(risk_upper), ")"),
                rr_text = ifelse(core_profile == "None", "1.00 (reference)", sprintf("%.2f (%.2f–%.2f)", rr, rr_lower, rr_upper)))
readr::write_csv(t2v |> dplyr::transmute(outcome = outcome_label, model_id = as.character(model_id),
                                         core_profile = as.character(core_profile), profile_label, n_cases, n_events,
                                         std_risk_pct = 100 * risk, std_risk_lower_pct = 100 * risk_lower,
                                         std_risk_upper_pct = 100 * risk_upper, rr, rr_lower, rr_upper,
                                         risk_display = risk_text, rr_display = rr_text),
                 file.path(tab_dir, "Table2_Joint_Profiles_Validated.csv"))
font_family <- "Arial"
t2 <- data.frame(outcome = t2v$outcome_label, profile = t2v$profile_label, risk = t2v$risk_text, rr = t2v$rr_text,
                 stringsAsFactors = FALSE)
ft2 <- flextable(t2) |>
  set_header_labels(outcome = "Outcome", profile = "Joint core-3 profile",
                    risk = "Population-standardised risk,\n% (95% CI)", rr = "Standardised RR vs None\n(95% CI)")
ft2 <- merge_v(ft2, j = 1, part = "body") |> border_remove()
thin_rule <- fp_border(color = "#C9D1D5", width = 0.65); header_rule <- fp_border(color = "#7E8A90", width = 1.0)
ft2 <- ft2 |> hline_top(part = "header", border = header_rule) |> hline_bottom(part = "header", border = header_rule) |>
  hline(i = seq_len(nrow(t2)), part = "body", border = thin_rule) |> hline(i = 8, part = "body", border = header_rule) |>
  bg(part = "header", bg = "#EEF1F3") |> bg(part = "body", bg = "#FFFFFF") |> color(part = "all", color = "#263238") |>
  font(part = "all", fontname = font_family) |> fontsize(part = "header", size = 8.5) |> fontsize(part = "body", size = 8.25) |>
  bold(part = "header", bold = TRUE) |> bold(j = 1:2, part = "body", bold = TRUE) |>
  align(align = "center", part = "all") |> align(j = 2, align = "left", part = "all") |> valign(part = "all", valign = "center") |>
  padding(part = "all", padding.top = 3, padding.bottom = 3, padding.left = 5, padding.right = 5) |>
  height(part = "header", height = 0.42) |> height(part = "body", height = 0.28)
for (j in 1:4) ft2 <- width(ft2, j = j, width = c(1.25, 1.95, 2.05, 1.70)[j])
ft2 <- set_table_properties(ft2, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))
dmckd_deaths <- t2v$n_events[t2v$model_id == "mortality_H0" & t2v$core_profile == "DM+CKD"]
note2 <- paste0(
  "The eight profiles are mutually exclusive combinations of diabetes mellitus (DM), hypertension (HTN) and ",
  "chronic kidney disease (CKD). Risks are from outcome-specific penalised logistic generalised additive models ",
  "with profile, sex, notification year and state of residence (random effect). The hospitalisation model includes ",
  "a common smooth function of age plus penalised profile-specific age deviations; the mortality model includes a ",
  "common smooth function of age without profile-by-age interaction. Risks were standardised by g-computation to the ",
  "observed joint distribution of exact age, sex, year and state in each outcome cohort (hospitalisation, N = 776,072 ",
  "with 25,363 hospitalisations; chikungunya-attributed death, N = 983,947 with 1,050 deaths). Standardised risk ratios ",
  "(RR) compare each profile with None. 95% CIs for risks and RRs are from the same 4,000 parametric draws of the model ",
  "coefficients. Observed cases and events per profile are given in Table 1; estimates for DM + CKD in the mortality ",
  "analysis are based on ", dmckd_deaths, " deaths.")
doc <- read_docx()
doc <- body_set_default_section(doc, prop_section(page_margins = page_mar(top = 0.65, bottom = 0.65, left = 0.60, right = 0.60)))
doc <- body_add_fpar(doc, fpar(ftext("Table 2. ", prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
                               ftext("Population-standardised risks of hospitalisation and chikungunya-attributed death by joint comorbidity profile",
                                     prop = fp_text(font.family = font_family, font.size = 10))))
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft2, align = "center")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc, fpar(ftext("Note. ", prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)),
                               ftext(note2, prop = fp_text(font.family = font_family, font.size = 8))))
print(doc, target = file.path(tab_dir, "Table2_Joint_Profiles_FINAL.docx"))
flextable::save_as_image(ft2, path = file.path(tab_dir, "Table2_Joint_Profiles_FINAL.png"), res = 200)

## ============================================================
## Supplementary age-specific RR figures (design: script 53)
## ============================================================
supp_dir <- file.path(pm_dir, "supplementary")
rr_forest_fig <- function(d, x_lim, breaks, event_word) {
  sa <- d |> dplyr::filter(core_profile != "None") |>
    dplyr::mutate(age_group = factor(age_group, levels = age_levels),
                  core_profile = factor(core_profile, levels = core_profile_levels),
                  adequate = n_events >= 10 & n_nonevents >= 10,
                  profile_label = factor(profile_display[as.character(core_profile)], levels = rev(profile_display[-1])),
                  status = dplyr::case_when(adequate ~ "adequate", n_events >= 1 ~ "sparse", TRUE ~ "none"),
                  lo = pmax(rr_lower, x_lim[1]), hi = pmin(rr_upper, x_lim[2]),
                  col = ifelse(status == "adequate", as.character(core_profile), "muted"))
  ggplot2::ggplot(dplyr::filter(sa, status != "none"), ggplot2::aes(y = profile_label)) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.3, colour = ink_muted) +
    ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = profile_label, colour = col), linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = rr, colour = col, shape = core_profile,
                                     fill = ifelse(status == "adequate", as.character(core_profile), "open")),
                        size = 1.7, stroke = 0.6) +
    ggplot2::geom_text(data = dplyr::filter(sa, status == "none"),
                       ggplot2::aes(x = 1.08, label = paste0("0 ", tolower(event_word))), hjust = 0,
                       size = 2, colour = ink_muted, fontface = "italic", family = "Arial") +
    ggplot2::facet_grid(. ~ age_group, labeller = ggplot2::as_labeller(age_labels)) +
    ggplot2::scale_x_log10(limits = x_lim, breaks = breaks, labels = as.character(breaks)) +
    ggplot2::scale_colour_manual(values = c(profile_colours, muted = "#A6A6A6"), guide = "none") +
    ggplot2::scale_fill_manual(values = c(scales::alpha(profile_colours, 0.25), open = "white"), guide = "none") +
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
save_figure(rr_forest_fig(dplyr::filter(age_est, model_id == "hospitalisation_H2"), c(0.3, 20), c(0.5, 1, 2, 5, 10),
                          "Hospitalisations"), "Supp_hospital_age_specific_RR_H2", 183, 80, dir = supp_dir, formats = c("pdf", "png"))
save_figure(rr_forest_fig(dplyr::filter(age_est, model_id == "mortality_H0"), c(0.5, 100), c(1, 3, 10, 30, 100),
                          "Deaths"), "Supp_mortality_age_specific_RR_H0", 183, 80, dir = supp_dir, formats = c("pdf", "png"))

## ============================================================
## Legends (Word): title + legend only
## ============================================================
write_legend <- function(title, legend, path) {
  body_text <- officer::fp_text(font.family = "Arial", font.size = 11)
  bold_text <- officer::fp_text(font.family = "Arial", font.size = 11, bold = TRUE)
  para <- officer::fp_par(text.align = "justify", line_spacing = 1.5, padding.bottom = 10)
  doc <- officer::read_docx() |>
    officer::body_add_fpar(officer::fpar(officer::ftext(title, bold_text), fp_p = para)) |>
    officer::body_add_fpar(officer::fpar(officer::ftext(legend, body_text), fp_p = para))
  doc <- officer::cursor_begin(doc)
  if (nchar(officer::docx_summary(doc)$text[1]) == 0) doc <- officer::body_remove(doc)
  print(doc, target = path)
}
models_text <- paste0(
  "Risks were estimated from outcome-specific penalised logistic generalised additive models including the eight ",
  "mutually exclusive profiles, sex, notification year and state of residence (random effect). The hospitalisation ",
  "model included a common smooth function of age plus penalised profile-specific age deviations; the mortality model ",
  "included a common smooth function of age without profile-by-age interaction. ")
sparse_m <- fig1_data |> dplyr::filter(model_id == "mortality_H0", sparse)
leg1 <- paste0(
  "Points show population-standardised risk ratios (RR) for each profile relative to cases with none of the three ",
  "conditions (None), among confirmed chikungunya cases notified in Brazil in 2017–2025: (A) hospitalisation ",
  "(n = 776,072; 25,363 hospitalisations); (B) chikungunya-attributed death (n = 983,947; 1,050 deaths). Profiles are ",
  "mutually exclusive combinations of diabetes mellitus (DM), hypertension (HTN) and chronic kidney disease (CKD), ",
  "grouped by number of conditions. ", models_text,
  "For each profile, every case in the outcome cohort was assigned that profile with their observed age, sex, year and ",
  "state, and predicted risks were averaged (g-computation); RRs are ratios of these standardised risks. Horizontal ",
  "bars are 95% confidence intervals from 4,000 parametric draws of the model coefficients, with all profiles evaluated ",
  "on the same draws. Columns give RR (95% CI) and observed events / cases. ",
  if (nrow(sparse_m) > 0) paste0("Open markers denote profiles with fewer than 10 outcome events (",
                                 paste0(profile_display[sparse_m$core_profile], ", ", sparse_m$n_events, " deaths", collapse = "; "),
                                 "). ") else "",
  "Horizontal axes are logarithmic and differ between panels.")
write_legend(paste0("Figure 1. Overall population-standardised risk ratios of hospitalisation and chikungunya-attributed ",
                    "death by multimorbidity profile"), leg1, file.path(leg_dir, "Figure1_Title_and_Legend.docx"))

never_shown <- fig2_cells |> dplyr::group_by(core_profile) |> dplyr::summarise(any = any(shown), .groups = "drop") |>
  dplyr::filter(!any) |> dplyr::pull(core_profile) |> as.character()
leg2 <- paste0(
  "(A) Standardised risk of hospitalisation by age group among 776,072 cases with recorded hospitalisation status ",
  "(25,363 hospitalisations). (B) Standardised risk of chikungunya-attributed death by age group among 983,947 cases ",
  "with recorded outcome (1,050 deaths). Profiles are the mutually exclusive combinations of diabetes (DM), ",
  "hypertension (HTN) and chronic kidney disease (CKD); None denotes cases with none of these conditions and is shown in ",
  "every age group as the reference trajectory. ", models_text,
  "Estimates come from the same fitted models as Figure 1 and Table 2. Risks were standardised by g-computation within ",
  "each age group: every profile was assigned the observed joint distribution of exact age, sex, notification year and ",
  "state of residence of all cases in that age group. Only age × profile cells with at least 10 outcome events and at ",
  "least 10 non-events are displayed",
  if (length(never_shown)) paste0("; ", paste(profile_display[never_shown], collapse = ", "),
                                  " did not meet this threshold in any age group and is not shown") else "",
  ". Lines connect estimates in adjacent displayed age groups only. Case and event counts for all age × profile cells, ",
  "with estimates for sparse cells not displayed here, are provided in Supplementary Table S[X]. Error bars denote 95% ",
  "confidence intervals from 4,000 parametric draws of the model coefficients. Vertical axes are logarithmic and differ ",
  "in scale between panels.")
write_legend(paste0("Figure 2. Age-specific population-standardised absolute risks of hospitalisation and ",
                    "chikungunya-attributed death by multimorbidity profile"), leg2,
             file.path(leg_dir, "Figure2_Title_and_Legend.docx"))

sparse3 <- d3 |> dplyr::filter(sparse)
leg3 <- paste0(
  "Points show standardised risk differences (RD) between pairs of profiles with the same number of conditions, among ",
  "confirmed chikungunya cases notified in Brazil in 2017–2025, for hospitalisation (n = 776,072; 25,363 ",
  "hospitalisations) and chikungunya-attributed death (n = 983,947; 1,050 deaths). Six contrasts were pre-specified: ",
  "among single-condition profiles, DM vs HTN, CKD vs HTN and CKD vs DM; among two-condition profiles, DM+CKD vs DM+HTN, ",
  "HTN+CKD vs DM+HTN and HTN+CKD vs DM+CKD. RD is the standardised risk of the first-listed profile minus that of the ",
  "second-listed profile, in percentage points; positive values indicate higher risk in the first-listed profile. ",
  "RDs are differences between the overall standardised risks reported in Table 2, from the same fitted models. ",
  models_text,
  "Horizontal bars are pointwise 95% confidence intervals from 4,000 draws of the model coefficients, with both risks in ",
  "each contrast computed from the same draw; they are not adjusted for multiple comparisons (Bonferroni-simultaneous ",
  "intervals are given in Supplementary Table S[X]). ",
  if (nrow(sparse3) > 0) paste0("Open grey circles mark contrasts involving a profile with fewer than 10 outcome events (",
                                nrow(sparse3), " contrast", ifelse(nrow(sparse3) > 1, "s", ""),
                                " involving DM+CKD in the mortality analysis), which are imprecise. ") else "",
  "DM, diabetes mellitus; HTN, hypertension; CKD, chronic kidney disease.")
write_legend(paste0("Figure 3. Within-count differences in population-standardised risks of hospitalisation and ",
                    "chikungunya-attributed death"), leg3, file.path(leg_dir, "Figure3_Title_and_Legend.docx"))

## ============================================================
## Cross-figure consistency
## ============================================================
t2chk <- readr::read_csv(file.path(tab_dir, "Table2_Joint_Profiles_Validated.csv"), show_col_types = FALSE)
f1chk <- readr::read_csv(file.path(fig_dir, "Figure1_plotted_values.csv"), show_col_types = FALSE)
f2chk <- readr::read_csv(file.path(fig_dir, "Figure2_cells_and_display_status.csv"), show_col_types = FALSE)
f3chk <- readr::read_csv(file.path(fig_dir, "Figure3_plotted_values.csv"), show_col_types = FALSE)
j <- dplyr::inner_join(f1chk, t2chk, by = c("model_id", "core_profile"), suffix = c("_f1", "_t2"))
f3j <- f3chk |>
  dplyr::left_join(dplyr::transmute(t2chk, model_id, profile_a = core_profile, ta = std_risk_pct), by = c("model_id", "profile_a")) |>
  dplyr::left_join(dplyr::transmute(t2chk, model_id, profile_b = core_profile, tb = std_risk_pct), by = c("model_id", "profile_b"))
f2j <- f2chk |> dplyr::inner_join(dplyr::select(age_est, model_id, age_group, core_profile, est = std_risk_pct,
                                                lo = std_risk_lower_pct, hi = std_risk_upper_pct),
                                  by = c("model_id", "age_group", "core_profile"))
cross <- tibble::tribble(
  ~check, ~n_compared, ~max_abs_difference,
  "Figure 1 vs Table 2: risk (%)", nrow(j), max(abs(100 * j$risk - j$std_risk_pct)),
  "Figure 1 vs Table 2: risk CI (%)", nrow(j), max(abs(c(100 * j$risk_lower - j$std_risk_lower_pct, 100 * j$risk_upper - j$std_risk_upper_pct))),
  "Figure 1 vs Table 2: RR and CI", nrow(j), max(abs(c(j$rr_f1 - j$rr_t2, j$rr_lower_f1 - j$rr_lower_t2, j$rr_upper_f1 - j$rr_upper_t2))),
  "Figure 3 RD = Table 2 risk(a) - risk(b) (pp)", nrow(f3j), max(abs(f3j$rd_pp - (f3j$ta - f3j$tb))),
  "Figure 3 risks = Table 2 risks (%)", nrow(f3j), max(abs(c(f3j$risk_a_pct - f3j$ta, f3j$risk_b_pct - f3j$tb))),
  "Figure 2 cells = canonical age-specific estimates", nrow(f2j),
  max(abs(c(f2j$std_risk_pct - f2j$est, f2j$std_risk_lower_pct - f2j$lo, f2j$std_risk_upper_pct - f2j$hi))),
  "Table 2 RR = risk / risk(None), unrounded", nrow(t2chk),
  max(abs((t2chk |> dplyr::group_by(model_id) |> dplyr::mutate(r = std_risk_pct / std_risk_pct[core_profile == "None"]) |>
             dplyr::ungroup() |> dplyr::mutate(d = rr - r))$d))
) |> dplyr::mutate(pass = max_abs_difference < 1e-10)
readr::write_csv(cross, file.path(pm_dir, "validation", "cross_figure_consistency.csv"))
print(cross)
stopifnot(all(cross$pass))
log64("done")
