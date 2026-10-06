# ---------------------------------------------------------------------------
# 34_profile_burden_allocation_brazil.R
#
# DRAFT / exploratory - Brazil-only prototype: allocates the ALREADY-KNOWN
# total hospitalisations/deaths per age band (country_age_burden_overall.rds,
# an existing, separate chikungunya burden-by-country-age estimate - NOT
# produced by this DM/HTN/CKD exploration) across the 8 DM/HTN/CKD profiles,
# using RR-weighted shares - rather than multiplying the aggregate rate by
# each profile's RR directly (which would double-count/distort the already-
# fixed total).
#
# Why RR-weighted allocation, not "RR x overall rate":
# If everyone's risk is baseline_risk x RR_profile (the model this whole
# analysis already assumes - core_profile_rr_hosp/death's RR is vs None),
# then for a fixed age band with population N and known total events T:
#   T = sum_k( N * pi_k * baseline_risk * RR_k ) = baseline_risk * N * sum_k(pi_k * RR_k)
#   => baseline_risk = T / (N * sum_k(pi_k * RR_k))
#   => allocated_k = N * pi_k * baseline_risk * RR_k = T * (pi_k * RR_k) / sum_j(pi_j * RR_j)
# baseline_risk cancels out algebraically - the allocation only needs
# profile prevalence (pi_k) and the (age-pooled, profile-specific, NOT
# age-varying - per the user's explicit request) RR_k, not any assumption
# about the absolute baseline risk itself or its transportability.
#
# Age handling: country_age_burden_overall.rds reports 10-year bands,
# [0,10)..[80,90) (9 bands, nothing above 89y) for Brazil. The calibrated
# profile prevalence (32_, 5-year grid) is population-weighted (GBD Brazil
# population by 5-year band) up into those same 9 bands before allocating.
#
# Needs: 01_Data/country_age_burden_overall.rds
#        01_Data/brazil_clustering_ratio_r.RData (from 32_, calibrated_ui)
#        01_Data/core_profile_rr_hosp.RData, core_profile_rr_death.RData (from 12_)
#        01_Data/gbd_pop.csv (Brazil population by 5-year age band, for weighting)
# Output: 03_Output/tables/brazil_profile_burden_allocation_PROTOTYPE.xlsx
#         03_Output/figures/fig_profile_burden_allocation_brazil_PROTOTYPE.png/.pdf
#         03_Output/figures/fig_profile_burden_none_vs_comorbid_brazil_PROTOTYPE.png/.pdf
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(ggplot2); library(scales); library(openxlsx)
})
options(scipen = 999)

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"
font_family <- "Arial"
core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

## ============================================================
## 1. Known totals: Brazil hospitalisations/deaths by 10-year age band
## ============================================================

burden_raw <- readRDS("01_Data/country_age_burden_overall.rds")

burden_bra <- burden_raw |>
  dplyr::filter(country == "Brazil") |>
  dplyr::transmute(
    age_group,
    age_start10 = as.numeric(stringr::str_extract(as.character(age_group), "[0-9]+")),
    total_hosp = hospitalisation_med,
    total_death = fatal_med
  ) |>
  dplyr::arrange(age_start10)

cat("Brazil known totals by 10-year age band (country_age_burden_overall.rds):\n")
print(as.data.frame(burden_bra), row.names = FALSE)

## ============================================================
## 2. Profile prevalence (32_'s r-calibrated estimate), population-weighted
##    up from the 5-year GBD grid to these 10-year bands
## ============================================================

load("01_Data/brazil_clustering_ratio_r.RData")  # calibrated_ui (5-year grid, point/lower/upper)

pop_raw <- read.csv("01_Data/gbd_pop.csv")

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

pop_bra <- pop_raw |>
  dplyr::filter(measure_name == "Population", metric_name == "Number", location_name == "Brazil") |>
  dplyr::mutate(age_clean = stringr::str_remove(age_name, " years| year")) |>
  dplyr::filter(age_clean %in% keep_age) |>
  dplyr::group_by(age_clean) |>
  dplyr::summarise(population = sum(val), .groups = "drop")

profile_5y <- calibrated_ui |>
  dplyr::select(age_clean, age_start, age_end, profile, p_point, p_lower, p_upper) |>
  dplyr::left_join(pop_bra, by = "age_clean") |>
  dplyr::mutate(
    ## Map each 5-year band to the 10-year band it belongs to (floor to
    ## nearest 10, matching country_age_burden_overall's [0,10)..[80,90)).
    age_start10 = pmin(floor(age_start / 10) * 10, 80)
  )

## Population-weighted mean prevalence within each 10-year band x profile.
profile_10y <- profile_5y |>
  dplyr::filter(age_start10 <= 80) |>
  dplyr::group_by(age_start10, profile) |>
  dplyr::summarise(
    pi_point = stats::weighted.mean(p_point, population),
    pi_lower = stats::weighted.mean(p_lower, population),
    pi_upper = stats::weighted.mean(p_upper, population),
    .groups = "drop"
  )

chk <- profile_10y |> dplyr::group_by(age_start10) |> dplyr::summarise(s = sum(pi_point))
cat("\nSum of 8 profile prevalences per 10-year band (should be ~1):\n")
print(as.data.frame(chk))

## ============================================================
## 3. Profile-specific RR (pooled across age, per the user's request -
##    no age-varying RR is used here), from 12_'s core_profile_rr_hosp/death
## ============================================================

load("01_Data/core_profile_rr_hosp.RData")   # core_profile_rr_hosp
load("01_Data/core_profile_rr_death.RData")  # core_profile_rr_death

rr_hosp <- core_profile_rr_hosp |> dplyr::select(profile = core_profile, rr_hosp = rr, rr_hosp_lower = rr_lower, rr_hosp_upper = rr_upper)
rr_death <- core_profile_rr_death |> dplyr::select(profile = core_profile, rr_death = rr, rr_death_lower = rr_lower, rr_death_upper = rr_upper)

## ============================================================
## 4. RR-weighted allocation of the known totals across the 8 profiles
## ============================================================

allocate <- function(pi_k, rr_k, total) {
  w <- pi_k * rr_k
  total * w / sum(w)
}

alloc_hosp <- profile_10y |>
  dplyr::left_join(rr_hosp, by = "profile") |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_hosp), by = "age_start10") |>
  dplyr::group_by(age_start10) |>
  dplyr::mutate(
    allocated_hosp = allocate(pi_point, rr_hosp, dplyr::first(total_hosp)),
    allocated_hosp_lo = allocate(pi_lower, rr_hosp_lower, dplyr::first(total_hosp)),
    allocated_hosp_hi = allocate(pi_upper, rr_hosp_upper, dplyr::first(total_hosp))
  ) |>
  dplyr::ungroup()

alloc_death <- profile_10y |>
  dplyr::left_join(rr_death, by = "profile") |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_death), by = "age_start10") |>
  dplyr::group_by(age_start10) |>
  dplyr::mutate(
    allocated_death = allocate(pi_point, rr_death, dplyr::first(total_death)),
    allocated_death_lo = allocate(pi_lower, rr_death_lower, dplyr::first(total_death)),
    allocated_death_hi = allocate(pi_upper, rr_death_upper, dplyr::first(total_death))
  ) |>
  dplyr::ungroup()

chk_hosp <- alloc_hosp |> dplyr::group_by(age_start10) |> dplyr::summarise(s = sum(allocated_hosp), t = dplyr::first(total_hosp))
cat("\nAllocated hospitalisations sum vs known total, per age band (should match):\n")
print(as.data.frame(chk_hosp))

save(alloc_hosp, alloc_death, file = "01_Data/brazil_profile_burden_allocation.RData")

## ============================================================
## 5. Table export
## ============================================================

alloc_table <- alloc_hosp |>
  dplyr::select(age_start10, profile, pi_point, rr_hosp, allocated_hosp, allocated_hosp_lo, allocated_hosp_hi, total_hosp) |>
  dplyr::left_join(
    alloc_death |> dplyr::select(age_start10, profile, rr_death, allocated_death, allocated_death_lo, allocated_death_hi, total_death),
    by = c("age_start10", "profile")
  ) |>
  dplyr::arrange(age_start10, factor(profile, levels = core_profile_levels))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "profile_burden_allocation")
openxlsx::writeDataTable(wb, "profile_burden_allocation", alloc_table, withFilter = TRUE)
openxlsx::setColWidths(wb, "profile_burden_allocation", cols = 1:ncol(alloc_table), widths = "auto")
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_profile_burden_allocation_PROTOTYPE.xlsx"), overwrite = TRUE)

## ============================================================
## 6. Figures
## ============================================================

profile_colours <- c(
  "None" = "#999999", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

make_age_label <- function(x) paste0(x, "-", x + 9)

## ---- 6a. Stacked bars of allocated burden by profile, age, outcome ----

plot_alloc <- dplyr::bind_rows(
  alloc_hosp |> dplyr::transmute(age_start10, profile, allocated = allocated_hosp, outcome = "Hospitalisation"),
  alloc_death |> dplyr::transmute(age_start10, profile, allocated = allocated_death, outcome = "Death")
) |>
  dplyr::mutate(
    age_label = factor(make_age_label(age_start10), levels = make_age_label(sort(unique(age_start10)))),
    profile = factor(profile, levels = rev(core_profile_levels)),
    outcome = factor(outcome, levels = c("Hospitalisation", "Death"))
  )

fig_alloc <- ggplot2::ggplot(plot_alloc, ggplot2::aes(x = age_label, y = allocated, fill = profile)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(x = "Age band (years)", y = "Allocated events (Brazil)", fill = NULL) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "top",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_allocation_brazil_PROTOTYPE.png"),
                 fig_alloc, width = 160, height = 160, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_allocation_brazil_PROTOTYPE.pdf"),
                 fig_alloc, width = 160, height = 160, units = "mm", device = grDevices::cairo_pdf)

## ---- 6b. None vs any-comorbidity (sum of other 7), by age, both outcomes ----

plot_none_vs_comorbid <- plot_alloc |>
  dplyr::mutate(group2 = ifelse(profile == "None", "No underlying condition", "Any DM/HTN/CKD")) |>
  dplyr::group_by(age_label, outcome, group2) |>
  dplyr::summarise(allocated = sum(allocated), .groups = "drop") |>
  dplyr::mutate(group2 = factor(group2, levels = c("No underlying condition", "Any DM/HTN/CKD")))

group2_colours <- c("No underlying condition" = "#999999", "Any DM/HTN/CKD" = "#C44E52")

fig_none_vs_comorbid <- ggplot2::ggplot(plot_none_vs_comorbid, ggplot2::aes(x = age_label, y = allocated, fill = group2)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  ggplot2::scale_fill_manual(values = group2_colours) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(x = "Age band (years)", y = "Allocated events (Brazil)", fill = NULL) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "top",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_none_vs_comorbid_brazil_PROTOTYPE.png"),
                 fig_none_vs_comorbid, width = 160, height = 160, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_none_vs_comorbid_brazil_PROTOTYPE.pdf"),
                 fig_none_vs_comorbid, width = 160, height = 160, units = "mm", device = grDevices::cairo_pdf)

message("[34] Saved: brazil_profile_burden_allocation_PROTOTYPE.xlsx")
message("[34] Saved: fig_profile_burden_allocation_brazil_PROTOTYPE.png/.pdf")
message("[34] Saved: fig_profile_burden_none_vs_comorbid_brazil_PROTOTYPE.png/.pdf")
message("[34] DONE.")
