# ---------------------------------------------------------------------------
# 35_synthesis_figure_profile_to_burden_brazil.R
#
# DRAFT / exploratory - a 3-panel synthesis figure (Brazil only, calibrated
# clustering scenario) connecting:
#   A. the age distribution of symptomatic chikungunya cases,
#   B. the age-specific background DM/HTN/CKD joint-profile composition
#      (32_'s SINAN-calibrated P_cal(profile, age)), and
#   C. the age-specific composition of severe-outcome burden - each
#      profile's share of the RR-weighted allocation from 34_, i.e.
#      allocated_outcome(profile, age) / total_outcome(age).
# Two versions (hospitalisation, death) share panels A and B; only panel C
# differs.
#
# Needs: 01_Data/brazil_profile_burden_allocation.RData (alloc_hosp,
#        alloc_death - from 34_), 01_Data/country_age_burden_overall.rds
# Output: 03_Output/figures/fig_profile_prevalence_to_hospitalisation_burden_brazil_PROTOTYPE.png
#         03_Output/figures/fig_profile_prevalence_to_death_burden_brazil_PROTOTYPE.png
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(ggplot2); library(scales); library(patchwork)
})
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

age_breaks <- sort(unique(alloc_hosp$age_start10))
age_mid_of <- function(x) x + 5
age_label_of <- function(x) paste0(x, "-", x + 9)

## ============================================================
## Panel A data: symptomatic cases by age (same 9 10-year bands, same
## source as the known hosp/death totals in 34_)
## ============================================================

burden_raw <- readRDS("01_Data/country_age_burden_overall.rds")

panel_a_data <- burden_raw |>
  dplyr::filter(country == "Brazil") |>
  dplyr::transmute(
    age_start10 = as.numeric(stringr::str_extract(as.character(age_group), "[0-9]+")),
    symptomatic = symptomatic_med
  ) |>
  dplyr::filter(age_start10 %in% age_breaks) |>
  dplyr::mutate(age_mid = age_mid_of(age_start10)) |>
  dplyr::arrange(age_start10)

## ============================================================
## Panel B data: calibrated profile prevalence (pi_point is identical
## whichever of alloc_hosp/alloc_death it's read from - both trace back to
## the same 32_ calibration)
## ============================================================

panel_b_data <- alloc_hosp |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = pi_point
  )

## ============================================================
## Panel C data: each profile's share of allocated burden within age
## (allocated_outcome / total_outcome - sums to 1 within each age band by
## construction, see 34_'s derivation)
## ============================================================

panel_c_hosp <- alloc_hosp |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = allocated_hosp / total_hosp
  )

panel_c_death <- alloc_death |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = allocated_death / total_death
  )

## ============================================================
## Shared theme / x-scale so all 3 panels line up exactly
## ============================================================

theme_synthesis <- function(base_size = 9.5) {
  ggplot2::theme_classic(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size, hjust = 0, colour = "#1A1A1A"),
      axis.title = ggplot2::element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "#333333"),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.size = grid::unit(3.2, "mm"),
      plot.margin = ggplot2::margin(4, 10, 2, 4)
    )
}

shared_x <- ggplot2::scale_x_continuous(
  breaks = age_mid_of(age_breaks), labels = age_label_of(age_breaks),
  limits = range(age_mid_of(age_breaks)) + c(-5, 5), expand = c(0, 0)
)

## ============================================================
## Panel builders
## ============================================================

build_panel_a <- function() {
  ggplot2::ggplot(panel_a_data, ggplot2::aes(x = age_mid, y = symptomatic)) +
    ggplot2::geom_area(fill = "#B8C4CC", colour = "#5B6B75", linewidth = 0.5) +
    shared_x +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = NULL, y = "Symptomatic cases", title = "A    Symptomatic cases") +
    theme_synthesis() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10, colour = "#1A1A1A", margin = ggplot2::margin(b = 4)),
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()
    )
}

build_panel_b <- function() {
  ggplot2::ggplot(panel_b_data, ggplot2::aes(x = age_mid, y = value, fill = profile)) +
    ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
    shared_x +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Share of population", fill = NULL, title = "B    Joint-profile prevalence") +
    theme_synthesis() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10, colour = "#1A1A1A", margin = ggplot2::margin(b = 4)),
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = TRUE))
}

build_panel_c <- function(data, outcome_label) {
  ggplot2::ggplot(data, ggplot2::aes(x = age_mid, y = value, fill = profile)) +
    ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
    shared_x +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Age band (years)", y = "Share of allocated burden", fill = NULL,
      title = paste0("C    Profile share of severe-outcome burden (", outcome_label, ")")
    ) +
    theme_synthesis() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10, colour = "#1A1A1A", margin = ggplot2::margin(b = 4)),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

panel_a <- build_panel_a()
panel_b <- build_panel_b()

assemble <- function(panel_c, outfile_stub) {
  fig <- panel_a / panel_b / panel_c +
    patchwork::plot_layout(heights = c(0.8, 1, 1)) +
    patchwork::plot_annotation(
      title = "From background comorbidity prevalence to severe chikungunya burden",
      subtitle = "Brazil, calibrated clustering scenario (independence/maximum-clustering mixture weighted by SINAN-derived r)",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13, family = font_family, colour = "#1A1A1A"),
        plot.subtitle = ggplot2::element_text(size = 9, family = font_family, colour = "#4D4D4D", margin = ggplot2::margin(b = 6))
      )
    )

  ggplot2::ggsave(file.path(fig_dir, paste0(outfile_stub, ".png")), fig,
                   width = 170, height = 230, units = "mm", dpi = 600, bg = "white")
  ggplot2::ggsave(file.path(fig_dir, paste0(outfile_stub, ".pdf")), fig,
                   width = 170, height = 230, units = "mm", device = grDevices::cairo_pdf)
  fig
}

assemble(build_panel_c(panel_c_hosp, "hospitalisation"), "fig_profile_prevalence_to_hospitalisation_burden_brazil_PROTOTYPE")
assemble(build_panel_c(panel_c_death, "death"), "fig_profile_prevalence_to_death_burden_brazil_PROTOTYPE")

message("[35] Saved: fig_profile_prevalence_to_hospitalisation_burden_brazil_PROTOTYPE.png/.pdf")
message("[35] Saved: fig_profile_prevalence_to_death_burden_brazil_PROTOTYPE.png/.pdf")
message("[35] DONE.")
