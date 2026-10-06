# ---------------------------------------------------------------------------
# 33_combined_profile_overlay_brazil.R
#
# DRAFT / exploratory - all 8 calibrated DM/HTN/CKD profile curves (32_'s
# output) overlaid on ONE panel instead of 8 small multiples. Values span
# ~3 orders of magnitude (None ~100%->8%, DM+CKD peaking under 1%), so a
# linear y-axis would flatten every rare profile to an invisible line near
# zero - uses a log10 y-axis instead (same convention 12_'s Figure 3 already
# uses for multi-magnitude RR comparisons).
#
# Needs: 01_Data/brazil_clustering_ratio_r.RData (from 32_)
# Output: 03_Output/figures/fig_profile_prevalence_brazil_calibrated_overlay_PROTOTYPE.png/.pdf
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(scales)})
options(scipen = 999)

if (!file.exists("01_Data/brazil_clustering_ratio_r.RData")) {
  stop("Run 32_clustering_ratio_r_brazil.R first.")
}
load("01_Data/brazil_clustering_ratio_r.RData")  # calibrated_ui, r_table

fig_dir <- "03_Output/figures"
font_family <- "Arial"
core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

profile_colours <- c(
  "None" = "#999999", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

overlay_data <- calibrated_ui |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    profile = factor(profile, levels = core_profile_levels),
    ## log scale needs strictly positive values; floor at 0.01% (below the
    ## resolution of this whole exercise anyway) rather than drop true zeros
    p_point_floored = pmax(p_point, 0.0001),
    p_lower_floored  = pmax(p_lower, 0.0001),
    p_upper_floored  = pmax(p_upper, 0.0001)
  )

fig_overlay <- ggplot2::ggplot(overlay_data, ggplot2::aes(x = age_mid, y = p_point_floored, colour = profile, fill = profile)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = p_lower_floored, ymax = p_upper_floored), colour = NA, alpha = 0.12) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_colour_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 20)) +
  ggplot2::scale_y_log10(labels = scales::label_percent(), breaks = scales::log_breaks(n = 6)) +
  ggplot2::labs(x = "Age (years)", y = "Calibrated prevalence (log scale, 95% UI)", colour = NULL, fill = NULL) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2), fill = "none") +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left",
    axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333")
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_calibrated_overlay_PROTOTYPE.png"),
                 fig_overlay, width = 183, height = 120, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_calibrated_overlay_PROTOTYPE.pdf"),
                 fig_overlay, width = 183, height = 120, units = "mm", device = grDevices::cairo_pdf)

message("[33] Saved: fig_profile_prevalence_brazil_calibrated_overlay_PROTOTYPE.png/.pdf")
message("[33] DONE.")
