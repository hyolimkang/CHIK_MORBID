# ---------------------------------------------------------------------------
# 37_profile_burden_share_absolute_brazil.R
#
# DRAFT / exploratory - absolute-count companion to 34_ Part 11's
# fig_profile_burden_share_calibrated_brazil_PROTOTYPE (100%-stacked share
# of age-specific outcome burden). Same data, same profile colours/order,
# but ABSOLUTE allocated events instead of share - so magnitude (not just
# composition) is visible, same rationale as 36_'s absolute companion to
# 35_'s synthesis figure.
#
# Needs: 01_Data/brazil_profile_burden_allocation.RData (alloc_hosp,
#        alloc_death - from 34_)
# Output: 03_Output/figures/fig_profile_burden_share_calibrated_brazil_absolute_PROTOTYPE.png/.pdf
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(scales)})
options(scipen = 999)

fig_dir <- "03_Output/figures"
font_family <- "Arial"
core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

profile_colours <- c(
  "None" = "#999999", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

load("01_Data/brazil_profile_burden_allocation.RData")  # alloc_hosp, alloc_death

make_age_label <- function(x) paste0(x, "-", x + 9)

burden_absolute_data <- dplyr::bind_rows(
  alloc_hosp |> dplyr::transmute(age_start10, profile, allocated = allocated_hosp, outcome = "A    Hospitalisation"),
  alloc_death |> dplyr::transmute(age_start10, profile, allocated = allocated_death, outcome = "B    Death")
) |>
  dplyr::mutate(
    age_mid = age_start10 + 5,
    profile = factor(profile, levels = rev(core_profile_levels)),
    outcome = factor(outcome, levels = c("A    Hospitalisation", "B    Death"))
  )

fig_burden_share_absolute <- ggplot2::ggplot(burden_absolute_data, ggplot2::aes(x = age_mid, y = allocated, fill = profile)) +
  ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.12) +
  ggplot2::facet_wrap(~outcome, ncol = 2, scales = "free_y") +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(burden_absolute_data$age_start10)) + 5,
    labels = make_age_label(sort(unique(burden_absolute_data$age_start10))),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(
    title = "Age-specific distribution of severe chikungunya burden across comorbidity profiles (absolute numbers)",
    x = "Age band (years)", y = "Allocated events (Brazil)", fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "bottom",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    plot.title = ggplot2::element_text(face = "bold", size = 10.5, family = font_family)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_share_calibrated_brazil_absolute_PROTOTYPE.png"),
                 fig_burden_share_absolute, width = 220, height = 110, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_share_calibrated_brazil_absolute_PROTOTYPE.pdf"),
                 fig_burden_share_absolute, width = 220, height = 110, units = "mm", device = grDevices::cairo_pdf)

message("[37] Saved: fig_profile_burden_share_calibrated_brazil_absolute_PROTOTYPE.png/.pdf")
message("[37] DONE.")
