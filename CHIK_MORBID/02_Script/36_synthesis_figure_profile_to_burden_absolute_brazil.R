# ---------------------------------------------------------------------------
# 36_synthesis_figure_profile_to_burden_absolute_brazil.R
#
# DRAFT / exploratory - absolute-count companion to 35_'s 3-panel synthesis
# figure. 35_'s panels B and C are 100%-stacked (composition/share); this
# version shows the same two panels in ABSOLUTE numbers instead, in the same
# area-plot style as panel A, so magnitude (not just composition) is visible
# - e.g. whether a profile's rising SHARE of burden at old age still
# corresponds to a rising or a shrinking absolute case count.
#   A. Symptomatic cases (identical to 35_)
#   B. Number of people per DM/HTN/CKD profile = P_cal(profile,age) x
#      Brazil population(age) - NOT a share, absolute headcount
#   C. Allocated hospitalisation/death events per profile (34_'s
#      allocated_hosp/allocated_death directly - already absolute)
#
# Needs: 01_Data/brazil_profile_burden_allocation.RData (alloc_hosp,
#        alloc_death - from 34_), 01_Data/country_age_burden_overall.rds,
#        01_Data/gbd_pop.csv
# Output: 03_Output/figures/fig_profile_prevalence_to_hospitalisation_burden_brazil_absolute_PROTOTYPE.png/.pdf
#         03_Output/figures/fig_profile_prevalence_to_death_burden_brazil_absolute_PROTOTYPE.png/.pdf
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
## Panel A data: symptomatic cases (same as 35_)
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
## Panel B data: ABSOLUTE headcount per profile = prevalence x population
## ============================================================

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

pop_raw <- read.csv("01_Data/gbd_pop.csv")

pop_bra_10y <- pop_raw |>
  dplyr::filter(measure_name == "Population", metric_name == "Number", location_name == "Brazil") |>
  dplyr::mutate(
    age_clean = stringr::str_remove(age_name, " years| year"),
    age_start5 = dplyr::case_when(
      stringr::str_detect(age_clean, "^<")  ~ 0,
      stringr::str_detect(age_clean, "\\+") ~ as.numeric(stringr::str_remove(age_clean, "\\+")),
      TRUE ~ as.numeric(stringr::str_extract(age_clean, "^[0-9]+"))
    )
  ) |>
  dplyr::filter(age_clean %in% keep_age) |>
  dplyr::mutate(age_start10 = pmin(floor(age_start5 / 10) * 10, 80)) |>
  dplyr::filter(age_start10 %in% age_breaks) |>
  dplyr::group_by(age_start10) |>
  dplyr::summarise(population = sum(val), .groups = "drop")

panel_b_data <- alloc_hosp |>
  dplyr::left_join(pop_bra_10y, by = "age_start10") |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = pi_point * population
  )

## ============================================================
## Panel C data: ABSOLUTE allocated events per profile (34_'s output,
## already absolute - no normalisation needed)
## ============================================================

panel_c_hosp <- alloc_hosp |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = allocated_hosp
  )

panel_c_death <- alloc_death |>
  dplyr::transmute(
    age_start10, age_mid = age_mid_of(age_start10),
    profile = factor(profile, levels = rev(core_profile_levels)),
    value = allocated_death
  )

## ============================================================
## Shared theme / x-scale
## ============================================================

theme_synthesis <- function(base_size = 9.5) {
  ggplot2::theme_classic(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
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
## Panel builders (absolute y-scales throughout)
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
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "People (count)", fill = NULL, title = "B    Joint-profile prevalence (absolute)") +
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
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      x = "Age band (years)", y = "Allocated events", fill = NULL,
      title = paste0("C    Profile allocated severe-outcome burden (", outcome_label, ", absolute)")
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
      title = "From background comorbidity prevalence to severe chikungunya burden (absolute numbers)",
      subtitle = "Brazil, calibrated clustering scenario - same data as the 100%-stacked version, shown as counts instead of shares",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12.5, family = font_family, colour = "#1A1A1A"),
        plot.subtitle = ggplot2::element_text(size = 9, family = font_family, colour = "#4D4D4D", margin = ggplot2::margin(b = 6))
      )
    )

  ggplot2::ggsave(file.path(fig_dir, paste0(outfile_stub, ".png")), fig,
                   width = 200, height = 230, units = "mm", dpi = 600, bg = "white")
  ggplot2::ggsave(file.path(fig_dir, paste0(outfile_stub, ".pdf")), fig,
                   width = 200, height = 230, units = "mm", device = grDevices::cairo_pdf)
  fig
}

assemble(build_panel_c(panel_c_hosp, "hospitalisation"), "fig_profile_prevalence_to_hospitalisation_burden_brazil_absolute_PROTOTYPE")
assemble(build_panel_c(panel_c_death, "death"), "fig_profile_prevalence_to_death_burden_brazil_absolute_PROTOTYPE")

message("[36] Saved: fig_profile_prevalence_to_hospitalisation_burden_brazil_absolute_PROTOTYPE.png/.pdf")
message("[36] Saved: fig_profile_prevalence_to_death_burden_brazil_absolute_PROTOTYPE.png/.pdf")
message("[36] DONE.")
