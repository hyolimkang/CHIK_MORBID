# ---------------------------------------------------------------------------
# 32_clustering_ratio_r_brazil.R
#
# DRAFT / exploratory - estimates a single clustering-ratio r(age) per 5-year
# age band, the mixture weight between 29_'s two theoretical bounds:
#
#   P_calibrated(profile) = (1-r) * P_independence(profile) + r * P_maxclustering(profile)
#
# r=0 reproduces pure independence, r=1 reproduces pure max clustering
# (comonotonic), and any r in [0,1] is guaranteed to still sum to 1 across
# the 8 profiles (convex combination of two valid distributions - this is a
# 2-point Frechet-copula mixture, unlike the earlier per-cell r_k x
# renormalise attempt in the chat history, which could and did break for
# the rare DM+HTN+CKD cell).
#
# r is estimated FROM SINAN ITSELF (self-consistent: SINAN's own marginals
# feed both the independence/max-clustering bounds AND the "observed" value
# used to solve for r), matching on P(2+ conditions) - chosen because it
# pools the two 2-way cells and the triple cell together, diluting the
# single-cell sampling noise that made the rare DM+HTN+CKD-specific ratio so
# unstable in the earlier (now superseded) r_k attempt:
#
#   r = (P_obs(2+) - P_indep(2+)) / (P_maxclust(2+) - P_indep(2+))
#
# The resulting r is then TRANSPORTED to the Brazil GBD/NCD-RisC-based
# bounds from 29_ (general-population marginals) - same transportability
# assumption 05_corr_matrix.R/08_copula_simulation.R already make (SINAN
# correlation structure as a proxy for general-population structure).
#
# A bootstrap (resample SINAN rows with replacement within each age band)
# gives r's own 95% UI, which is then propagated through the mixture formula
# to the 8 calibrated profile prevalences - the single-curve-plus-ribbon
# analogue of the PDF's last slide (points + smooth trend + 95% CI band).
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds,
#        01_Data/brazil_profile_prevalence_two_extremes.RData (from 29_)
# Output: 03_Output/figures/fig_clustering_ratio_r_brazil_PROTOTYPE.png/.pdf
#         03_Output/figures/fig_profile_prevalence_brazil_calibrated_ui_PROTOTYPE.png/.pdf
#         03_Output/tables/brazil_clustering_ratio_r_PROTOTYPE.xlsx
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales); library(openxlsx)
})
options(scipen = 999)

set.seed(20261005)

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"
font_family <- "Arial"

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
age_band_levels <- c("0-19","20-39","40-59","60-79","80+")

load("01_Data/brazil_profile_prevalence_two_extremes.RData")  # bg_prev_brazil3, profile_prev_brazil, max_clustering_brazil

## ============================================================
## 1. Rebuild SINAN individual-level cohort (needed for bootstrap resampling)
## ============================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
core_cols <- c("diabetes", "hypertension", "renal_disease")

sinan3 <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::mutate(
    dm  = as.integer(diabetes == "yes"),
    htn = as.integer(hypertension == "yes"),
    ckd = as.integer(renal_disease == "yes"),
    age_band_profile = cut(age_years, breaks = c(0, 20, 40, 60, 80, 101),
                            labels = age_band_levels, right = FALSE, include.lowest = TRUE)
  )

rm(ind); gc(verbose = FALSE)

## ============================================================
## 2. r(age band): point estimate + bootstrap 95% UI
## ============================================================

condition_order <- c("DM", "HTN", "CKD")
pair_label <- function(a, b) paste(condition_order[condition_order %in% c(a, b)], collapse = "+")

## Given 3 marginals, return P(2+ conditions) under independence and under
## max clustering (comonotonic) - the two bounds r interpolates between.
p2plus_bounds <- function(p_dm, p_htn, p_ckd) {
  p_indep_2plus <- 1 - ((1-p_dm)*(1-p_htn)*(1-p_ckd)) -
    (p_dm*(1-p_htn)*(1-p_ckd)) - ((1-p_dm)*p_htn*(1-p_ckd)) - ((1-p_dm)*(1-p_htn)*p_ckd)
  p <- sort(c(DM = p_dm, HTN = p_htn, CKD = p_ckd))
  p_min <- p[[1]]; p_mid <- p[[2]]; p_max <- p[[3]]
  p_maxclust_2plus <- (p_mid - p_min) + p_min   # (largest+middle only) + (all three)
  c(indep = p_indep_2plus, maxclust = p_maxclust_2plus)
}

estimate_r <- function(d) {
  p_dm <- mean(d$dm); p_htn <- mean(d$htn); p_ckd <- mean(d$ckd)
  obs_2plus <- mean((d$dm + d$htn + d$ckd) >= 2)
  bounds <- p2plus_bounds(p_dm, p_htn, p_ckd)
  (obs_2plus - bounds["indep"]) / (bounds["maxclust"] - bounds["indep"])
}

r_point <- sinan3 |>
  dplyr::group_by(age_band_profile) |>
  dplyr::group_modify(~ tibble::tibble(r = estimate_r(.x), n = nrow(.x))) |>
  dplyr::ungroup()

cat("r(age band) point estimate (SINAN self-consistent, matched on P(2+ conditions)):\n")
print(as.data.frame(r_point), row.names = FALSE, digits = 4)

n_boot <- 1000
boot_r <- vector("list", length(age_band_levels))
names(boot_r) <- age_band_levels

for (band in age_band_levels) {
  d_band <- sinan3 |> dplyr::filter(age_band_profile == band)
  n_band <- nrow(d_band)
  r_draws <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    d_resampled <- d_band[sample.int(n_band, n_band, replace = TRUE), ]
    r_draws[b] <- estimate_r(d_resampled)
  }
  boot_r[[band]] <- r_draws
}

r_ui <- purrr::imap_dfr(boot_r, ~ tibble::tibble(
  age_band_profile = factor(.y, levels = age_band_levels),
  r_lower = stats::quantile(.x, 0.025, na.rm = TRUE),
  r_upper = stats::quantile(.x, 0.975, na.rm = TRUE)
))

r_table <- r_point |>
  dplyr::mutate(age_band_profile = factor(age_band_profile, levels = age_band_levels)) |>
  dplyr::left_join(r_ui, by = "age_band_profile") |>
  dplyr::arrange(age_band_profile)

cat("\nr(age band) with bootstrap 95% UI (", n_boot, " resamples):\n", sep = "")
print(as.data.frame(r_table), row.names = FALSE, digits = 4)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "clustering_ratio_r")
openxlsx::writeDataTable(wb, "clustering_ratio_r", r_table, withFilter = TRUE)
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_clustering_ratio_r_PROTOTYPE.xlsx"), overwrite = TRUE)

## ---- figure: r(age), PDF-last-slide style (points + ribbon) ----

fig_r <- ggplot2::ggplot(r_table, ggplot2::aes(x = age_band_profile, y = r, group = 1)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = r_lower, ymax = r_upper), fill = "#4C72B0", alpha = 0.18) +
  ggplot2::geom_line(colour = "#4C72B0", linewidth = 0.9) +
  ggplot2::geom_point(colour = "#4C72B0", size = 2) +
  ggplot2::geom_hline(yintercept = c(0, 1), linetype = "dashed", linewidth = 0.3, colour = "#999999") +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(x = "Age band", y = "Clustering ratio r\n(0 = independence, 1 = maximum clustering)") +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"))

ggplot2::ggsave(file.path(fig_dir, "fig_clustering_ratio_r_brazil_PROTOTYPE.png"),
                 fig_r, width = 120, height = 90, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_clustering_ratio_r_brazil_PROTOTYPE.pdf"),
                 fig_r, width = 120, height = 90, units = "mm", device = grDevices::cairo_pdf)

message("[32] r(age) computed and plotted.")

## ============================================================
## 3. Apply r (point, lower, upper) to the Brazil GBD/NCD-RisC bounds
## ============================================================

max_clustering_row <- function(p_dm, p_htn, p_ckd) {
  p <- c(DM = p_dm, HTN = p_htn, CKD = p_ckd)
  ord <- names(sort(p)); smallest <- ord[1]; middle <- ord[2]; largest <- ord[3]
  p_min <- p[[smallest]]; p_mid <- p[[middle]]; p_max <- p[[largest]]
  out <- setNames(rep(0, length(core_profile_levels)), core_profile_levels)
  out["None"] <- 1 - p_max; out[largest] <- p_max - p_mid
  out[pair_label(largest, middle)] <- p_mid - p_min; out["DM+HTN+CKD"] <- p_min
  out
}

age_to_band <- bg_prev_brazil3 |>
  dplyr::distinct(age_clean, age_start, age_end) |>
  dplyr::mutate(age_band_profile = cut(age_start, breaks = c(0, 20, 40, 60, 80, Inf),
                                        labels = age_band_levels, right = FALSE, include.lowest = TRUE))

indep_long <- profile_prev_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "p_indep")

max_long <- max_clustering_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "p_maxclust")

calibrated_ui <- indep_long |>
  dplyr::left_join(max_long, by = c("age_clean", "profile")) |>
  dplyr::left_join(age_to_band, by = "age_clean") |>
  dplyr::left_join(r_table |> dplyr::select(age_band_profile, r, r_lower, r_upper), by = "age_band_profile") |>
  dplyr::mutate(
    p_point = (1 - r) * p_indep + r * p_maxclust,
    ## r_lower/r_upper bracket r itself; since the mixture is linear in r,
    ## the profile-prevalence UI bounds are whichever end of r pushes that
    ## profile's value further from p_indep (direction depends on the sign
    ## of (p_maxclust - p_indep) for that profile).
    p_from_rlo = (1 - r_lower) * p_indep + r_lower * p_maxclust,
    p_from_rhi = (1 - r_upper) * p_indep + r_upper * p_maxclust,
    p_lower = pmin(p_from_rlo, p_from_rhi),
    p_upper = pmax(p_from_rlo, p_from_rhi)
  )

chk <- calibrated_ui |> dplyr::group_by(age_clean) |> dplyr::summarise(s = sum(p_point))
cat("\nCalibrated point-estimate sums per age band (should be 1):\n"); print(range(chk$s))

save(r_table, calibrated_ui, file = "01_Data/brazil_clustering_ratio_r.RData")

## ---- figure: calibrated profile prevalence, small multiples with 95% UI ----

calibrated_plot <- calibrated_ui |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    profile = factor(profile, levels = core_profile_levels)
  )

profile_colours <- c(
  "None" = "#BFBFBF", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

fig_calibrated_ui <- ggplot2::ggplot(calibrated_plot, ggplot2::aes(x = age_mid, y = p_point, colour = profile, fill = profile)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = p_lower, ymax = p_upper), colour = NA, alpha = 0.2) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_wrap(~profile, ncol = 4, scales = "free_y") +
  ggplot2::scale_colour_manual(values = profile_colours) +
  ggplot2::scale_fill_manual(values = profile_colours) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 40)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::labs(x = "Age (years)", y = "Calibrated prevalence (95% UI from r)") +
  ggplot2::theme_classic(base_size = 9, base_family = font_family) +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold", size = 8.5),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333")
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_calibrated_ui_PROTOTYPE.png"),
                 fig_calibrated_ui, width = 183, height = 110, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_calibrated_ui_PROTOTYPE.pdf"),
                 fig_calibrated_ui, width = 183, height = 110, units = "mm", device = grDevices::cairo_pdf)

message("[32] Saved: fig_clustering_ratio_r_brazil_PROTOTYPE.png/.pdf")
message("[32] Saved: fig_profile_prevalence_brazil_calibrated_ui_PROTOTYPE.png/.pdf")
message("[32] Saved: brazil_clustering_ratio_r_PROTOTYPE.xlsx")
message("[32] DONE.")
