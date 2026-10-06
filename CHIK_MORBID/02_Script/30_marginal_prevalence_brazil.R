# ---------------------------------------------------------------------------
# 30_marginal_prevalence_brazil.R
#
# DRAFT / exploratory - the single most basic descriptive step in the
# GBD-background-prevalence extension: background prevalence of DM, HTN,
# CKD BY AGE, Brazil, each condition on its own - no clustering/independence/
# comonotonic assumption at all, because a single condition's own marginal
# prevalence doesn't involve any assumption about how conditions co-occur.
# (Those assumptions only start to matter once you ask about JOINT profiles
# - see 29_background_prevalence_core3_brazil.R Parts C/D for that.)
#
# Reuses bg_prev_brazil3 from 29_'s cache rather than re-deriving it, so the
# exact same GBD/NCD-RisC extraction + HTN sex-weighting + age extrapolation
# logic is guaranteed to match (see 29_ Part B's header comment for the full
# method and its caveats - same HTN logit-quadratic extrapolation, SINAN-sex-
# ratio weighting, etc.).
#
# Needs: 01_Data/brazil_profile_prevalence_two_extremes.RData (from 29_)
# Output: 03_Output/tables/brazil_marginal_prevalence_PROTOTYPE.xlsx
#         03_Output/figures/fig_marginal_prevalence_brazil_PROTOTYPE.png
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales); library(openxlsx)
})
options(scipen = 999)

if (!file.exists("01_Data/brazil_profile_prevalence_two_extremes.RData")) {
  stop("Run 29_background_prevalence_core3_brazil.R first - it builds and caches bg_prev_brazil3.")
}
load("01_Data/brazil_profile_prevalence_two_extremes.RData")  # brings in bg_prev_brazil3

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"

## ---- table: wide (age x DM/HTN/CKD) and long ----

marginal_prevalence_brazil <- bg_prev_brazil3 |>
  dplyr::select(age_band = age_clean, age_start, age_end, DM, HTN, CKD) |>
  dplyr::mutate(
    DM_pct  = sprintf("%.2f%%", DM  * 100),
    HTN_pct = sprintf("%.2f%%", HTN * 100),
    CKD_pct = sprintf("%.2f%%", CKD * 100)
  )

cat("Brazil marginal prevalence by age band (DM/HTN/CKD, independent of any clustering assumption):\n")
print(as.data.frame(marginal_prevalence_brazil |> dplyr::select(age_band, DM_pct, HTN_pct, CKD_pct)),
      row.names = FALSE)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "marginal_prevalence")
openxlsx::writeDataTable(wb, "marginal_prevalence", marginal_prevalence_brazil, withFilter = TRUE)
openxlsx::setColWidths(wb, "marginal_prevalence", cols = 1:8, widths = "auto")
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_marginal_prevalence_PROTOTYPE.xlsx"), overwrite = TRUE)

## ---- figure: one line per condition ----

marginal_long <- marginal_prevalence_brazil |>
  dplyr::select(age_start, age_end, DM, HTN, CKD) |>
  tidyr::pivot_longer(c(DM, HTN, CKD), names_to = "condition", values_to = "prevalence") |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    condition = factor(condition, levels = c("DM", "HTN", "CKD"))
  )

condition_colours <- c("DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868")

fig_marginal_prevalence_brazil <- ggplot2::ggplot(
  marginal_long, ggplot2::aes(x = age_mid, y = prevalence, colour = condition)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 1.3) +
  ggplot2::scale_colour_manual(values = condition_colours) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(
    title = "Brazil background prevalence by age - DM, HTN, CKD (marginal, each condition on its own)",
    subtitle = "DM/CKD: GBD 2023. HTN: NCD-RisC 2019, sex-weighted by SINAN's own male proportion by age band,\nlogit-quadratic extrapolated below 30y/above 79y (see 29_ Part B). No clustering/independence assumption involved.",
    x = "Age (years)", y = "Prevalence", colour = NULL
  ) +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(legend.position = "top", legend.justification = "left")

ggplot2::ggsave(file.path(fig_dir, "fig_marginal_prevalence_brazil_PROTOTYPE.png"),
                fig_marginal_prevalence_brazil, width = 190, height = 130, units = "mm", dpi = 300, bg = "white")

message("[30] Saved table: 03_Output/tables/brazil_marginal_prevalence_PROTOTYPE.xlsx")
message("[30] Saved figure: 03_Output/figures/fig_marginal_prevalence_brazil_PROTOTYPE.png")
message("[30] DONE.")
