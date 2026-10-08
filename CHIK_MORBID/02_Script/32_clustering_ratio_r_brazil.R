# ---------------------------------------------------------------------------
# 32_clustering_ratio_r_brazil.R
#
# DRAFT / exploratory - estimates the CLUSTERING WEIGHT, lambda(age), per
# 5-year age band: the mixture weight between 29_'s two theoretical bounds:
#
#   P_cal(profile, age) =
#     [1 - lambda_b(age)] * P_ind(profile, age)
#     + lambda_b(age) * P_max(profile, age)
#
# lambda = 0 reproduces independence, lambda = 1 reproduces maximum
# clustering (comonotonic), and any lambda in [0,1] is guaranteed to still
# sum to 1 across the 8 profiles (convex combination of two valid
# distributions - this is a 2-point Frechet-copula mixture, unlike the
# earlier per-cell ratio x renormalise attempt in the chat history, which
# could and did break for the rare DM+HTN+CKD cell).
#
# lambda is NOT a simple observed/expected ratio - it is estimated FROM
# SINAN ITSELF (self-consistent: SINAN's own marginals feed both the
# independence/max-clustering bounds AND the "observed" value used to
# locate lambda between them), matching on P(2+ conditions) - chosen
# because it pools the two 2-way cells and the triple cell together,
# diluting the single-cell sampling noise that made the rare
# DM+HTN+CKD-specific ratio so unstable in the earlier (superseded) attempt:
#
#   lambda_b = [P_obs(2+ | b) - P_ind(2+ | b)] / [P_max(2+ | b) - P_ind(2+ | b)]
#
# The resulting lambda is then TRANSPORTED to the Brazil GBD/NCD-RisC-based
# bounds from 29_ (general-population marginals) - same transportability
# assumption 05_corr_matrix.R/08_copula_simulation.R already make (SINAN
# correlation structure as a proxy for general-population structure).
#
# A bootstrap (resample SINAN rows with replacement within each age band)
# gives lambda's own 95% UI, which is then propagated through the mixture
# formula to the 8 calibrated profile prevalences. The raw per-draw bootstrap
# lambda values are also saved (lambda_draws) for the draw-based burden
# propagation in 34_.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds,
#        01_Data/brazil_profile_prevalence_two_extremes.RData (from 29_)
# Output: 03_Output/figures/fig_clustering_ratio_r_brazil_PROTOTYPE.png/.pdf
#         03_Output/figures/fig_profile_prevalence_brazil_calibrated_ui_PROTOTYPE.png/.pdf
#         03_Output/tables/brazil_clustering_ratio_r_PROTOTYPE.xlsx
#         01_Data/brazil_clustering_ratio_r.RData (lambda_table, calibrated_ui)
#         01_Data/brazil_clustering_lambda_draws.RData (lambda_draws, for 34_)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2); library(scales); library(openxlsx)
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
## 2. lambda(age band): point estimate + bootstrap 95% UI + raw draws
## ============================================================

condition_order <- c("DM", "HTN", "CKD")
pair_label <- function(a, b) paste(condition_order[condition_order %in% c(a, b)], collapse = "+")

## Given 3 marginals, return P(2+ conditions) under independence and under
## max clustering (comonotonic) - the two bounds lambda interpolates between.
p2plus_bounds <- function(p_dm, p_htn, p_ckd) {
  p_indep_2plus <- 1 - ((1-p_dm)*(1-p_htn)*(1-p_ckd)) -
    (p_dm*(1-p_htn)*(1-p_ckd)) - ((1-p_dm)*p_htn*(1-p_ckd)) - ((1-p_dm)*(1-p_htn)*p_ckd)
  p <- sort(c(DM = p_dm, HTN = p_htn, CKD = p_ckd))
  p_min <- p[[1]]; p_mid <- p[[2]]; p_max <- p[[3]]
  p_maxclust_2plus <- (p_mid - p_min) + p_min   # (largest+middle only) + (all three)
  c(indep = p_indep_2plus, maxclust = p_maxclust_2plus)
}

estimate_lambda <- function(d) {
  p_dm <- mean(d$dm); p_htn <- mean(d$htn); p_ckd <- mean(d$ckd)
  obs_2plus <- mean((d$dm + d$htn + d$ckd) >= 2)
  bounds <- p2plus_bounds(p_dm, p_htn, p_ckd)
  (obs_2plus - bounds["indep"]) / (bounds["maxclust"] - bounds["indep"])
}

lambda_point_raw <- sinan3 |>
  dplyr::group_by(age_band_profile) |>
  dplyr::group_modify(~ tibble::tibble(lambda_raw = estimate_lambda(.x), n = nrow(.x))) |>
  dplyr::ungroup() |>
  dplyr::mutate(lambda = pmin(pmax(lambda_raw, 0), 1))

cat("lambda(age band) point estimate (SINAN self-consistent, matched on P(2+ conditions)):\n")
print(as.data.frame(lambda_point_raw), row.names = FALSE, digits = 4)

n_boot <- 1000
boot_lambda_raw <- vector("list", length(age_band_levels))
names(boot_lambda_raw) <- age_band_levels

for (band in age_band_levels) {
  d_band <- sinan3 |> dplyr::filter(age_band_profile == band)
  n_band <- nrow(d_band)
  lambda_draws_band <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    d_resampled <- d_band[sample.int(n_band, n_band, replace = TRUE), ]
    lambda_draws_band[b] <- estimate_lambda(d_resampled)
  }
  boot_lambda_raw[[band]] <- lambda_draws_band
}

## Tidy, draw-level object - the raw (unbounded) bootstrap estimate AND its
## [0,1]-clamped counterpart, kept side by side so the clamping's impact is
## auditable rather than silently applied.
lambda_draws <- purrr::imap_dfr(boot_lambda_raw, ~ tibble::tibble(
  draw = seq_along(.x),
  age_band_profile = factor(.y, levels = age_band_levels),
  lambda_raw = .x,
  lambda = pmin(pmax(.x, 0), 1)
))

n_out_of_range <- sum(lambda_draws$lambda_raw < 0 | lambda_draws$lambda_raw > 1)
prop_out_of_range <- n_out_of_range / nrow(lambda_draws)
cat(sprintf(
  "\nRaw bootstrap lambda draws outside [0,1]: %d / %d (%.3f%%)\n",
  n_out_of_range, nrow(lambda_draws), 100 * prop_out_of_range
))
out_of_range_by_band <- lambda_draws |>
  dplyr::group_by(age_band_profile) |>
  dplyr::summarise(
    n_out = sum(lambda_raw < 0 | lambda_raw > 1),
    pct_out = 100 * mean(lambda_raw < 0 | lambda_raw > 1),
    .groups = "drop"
  )
cat("By age band:\n")
print(as.data.frame(out_of_range_by_band), row.names = FALSE, digits = 3)
cat(if (prop_out_of_range < 0.01) {
  "Out-of-range draws are negligible (<1%) - the [0,1]-clamped `lambda` column is used throughout; `lambda_raw` is kept for audit only.\n"
} else {
  "NOTE: a non-trivial share of raw bootstrap draws fall outside [0,1] - clamped `lambda` is still used for the mixture propagation (clamping is the documented, explicit choice here), but this should be flagged when interpreting the UI width.\n"
})

save(lambda_draws, file = "01_Data/brazil_clustering_lambda_draws.RData")

## 95% UI from the CLAMPED draws (the same quantity actually propagated
## downstream, so the reported UI is consistent with what 34_ uses).
lambda_ui <- lambda_draws |>
  dplyr::group_by(age_band_profile) |>
  dplyr::summarise(
    lambda_lower = stats::quantile(lambda, 0.025, na.rm = TRUE),
    lambda_upper = stats::quantile(lambda, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

lambda_table <- lambda_point_raw |>
  dplyr::mutate(age_band_profile = factor(age_band_profile, levels = age_band_levels)) |>
  dplyr::left_join(lambda_ui, by = "age_band_profile") |>
  dplyr::arrange(age_band_profile)

cat("\nlambda(age band) with bootstrap 95% UI (", n_boot, " resamples, clamped to [0,1]):\n", sep = "")
print(as.data.frame(lambda_table), row.names = FALSE, digits = 4)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "clustering_weight_lambda")
openxlsx::writeDataTable(wb, "clustering_weight_lambda", lambda_table, withFilter = TRUE)
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_clustering_ratio_r_PROTOTYPE.xlsx"), overwrite = TRUE)

## ---- figure: lambda(age) ----

fig_lambda <- ggplot2::ggplot(lambda_table, ggplot2::aes(x = age_band_profile, y = lambda, group = 1)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lambda_lower, ymax = lambda_upper), fill = "#4C72B0", alpha = 0.18) +
  ggplot2::geom_line(colour = "#4C72B0", linewidth = 0.9) +
  ggplot2::geom_point(colour = "#4C72B0", size = 2) +
  ggplot2::geom_hline(yintercept = c(0, 1), linetype = "dashed", linewidth = 0.3, colour = "#999999") +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(
    title = "Age-specific clustering weight calibrated to the observed prevalence of multimorbidity (≥2 conditions)",
    x = "Age band",
    y = "Clustering weight, lambda\n(0 = independence; 1 = maximum clustering)"
  ) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
    plot.title = ggplot2::element_text(face = "bold", size = 10.5, family = font_family)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_clustering_ratio_r_brazil_PROTOTYPE.png"),
                 fig_lambda, width = 150, height = 95, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_clustering_ratio_r_brazil_PROTOTYPE.pdf"),
                 fig_lambda, width = 150, height = 95, units = "mm", device = grDevices::cairo_pdf)

message("[32] lambda(age) computed and plotted.")

## ============================================================
## 3. Apply lambda (point, lower, upper) to the Brazil GBD/NCD-RisC bounds
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
  dplyr::left_join(lambda_table |> dplyr::select(age_band_profile, lambda, lambda_lower, lambda_upper), by = "age_band_profile") |>
  dplyr::mutate(
    p_point = (1 - lambda) * p_indep + lambda * p_maxclust,
    ## lambda_lower/lambda_upper bracket lambda itself; since the mixture is
    ## linear in lambda, the profile-prevalence UI bounds are whichever end
    ## of lambda pushes that profile's value further from p_indep (direction
    ## depends on the sign of (p_maxclust - p_indep) for that profile).
    p_from_lambda_lo = (1 - lambda_lower) * p_indep + lambda_lower * p_maxclust,
    p_from_lambda_hi = (1 - lambda_upper) * p_indep + lambda_upper * p_maxclust,
    p_lower = pmin(p_from_lambda_lo, p_from_lambda_hi),
    p_upper = pmax(p_from_lambda_lo, p_from_lambda_hi)
  )

## ---- Validation: row sums and marginal reconstruction ----

chk_sum <- calibrated_ui |> dplyr::group_by(age_clean) |> dplyr::summarise(s = sum(p_point), .groups = "drop")
cat("\n[VALIDATION] Calibrated profile-prevalence sums per age band (expect exactly 1):\n")
print(range(chk_sum$s))
stopifnot(all(abs(chk_sum$s - 1) < 1e-8))

reconstruct_marginal <- function(condition_name) {
  calibrated_ui |>
    dplyr::filter(grepl(condition_name, profile, fixed = TRUE)) |>
    dplyr::group_by(age_clean) |>
    dplyr::summarise(reconstructed = sum(p_point), .groups = "drop")
}

marginal_check <- bg_prev_brazil3 |>
  dplyr::select(age_clean, DM, HTN, CKD) |>
  dplyr::left_join(reconstruct_marginal("DM") |> dplyr::rename(DM_reconstructed = reconstructed), by = "age_clean") |>
  dplyr::left_join(reconstruct_marginal("HTN") |> dplyr::rename(HTN_reconstructed = reconstructed), by = "age_clean") |>
  dplyr::left_join(reconstruct_marginal("CKD") |> dplyr::rename(CKD_reconstructed = reconstructed), by = "age_clean") |>
  dplyr::mutate(
    DM_diff = DM - DM_reconstructed,
    HTN_diff = HTN - HTN_reconstructed,
    CKD_diff = CKD - CKD_reconstructed
  )

cat("\n[VALIDATION] Reconstructed marginal (sum of profiles containing each condition) vs source marginal:\n")
cat("Max abs diff - DM:", max(abs(marginal_check$DM_diff)),
    " HTN:", max(abs(marginal_check$HTN_diff)),
    " CKD:", max(abs(marginal_check$CKD_diff)), "\n")
stopifnot(
  max(abs(marginal_check$DM_diff)) < 1e-8,
  max(abs(marginal_check$HTN_diff)) < 1e-8,
  max(abs(marginal_check$CKD_diff)) < 1e-8
)
cat("[VALIDATION] PASSED: calibrated profile mixture exactly reproduces the source DM/HTN/CKD marginals (both bounds share the same marginals by construction, and any convex combination of them does too).\n")

save(lambda_table, calibrated_ui, file = "01_Data/brazil_clustering_ratio_r.RData")

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
  ggplot2::labs(x = "Age (years)", y = "Calibrated prevalence (95% UI from lambda)") +
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
message("[32] Saved: brazil_clustering_lambda_draws.RData (", nrow(lambda_draws), " draw-level rows)")
message("[32] DONE.")
