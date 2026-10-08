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
# baseline_risk cancels out algebraically.
#
# The burden decomposition assumes that the pooled profile-specific relative
# risks are transportable across age; age-specific RR is not used in this
# prototype (core_profile_rr_hosp_by_age / core_profile_rr_death_by_age and
# the continuous age-varying profile-RR curve are deliberately NOT used
# here).
#
# Age handling (FIXED from the earlier version of this script, which mapped
# the 90-94 and 95+ profile-prevalence bands into the 80-89 group by
# capping age_start10 at 80 - that silently mixed 90+ population into the
# 80-89 estimate). Burden age bands are now the 10 explicit groups 0-9,
# 10-19, ..., 80-89, 90+. country_age_burden_overall.rds turns out to have
# NO 90+ row at all for any country (checked below, Part 1) - so the 90+
# profile-prevalence group is still constructed correctly (population-
# weighted from the 90-94 and 95+ GBD bands) but is dropped from the burden
# ALLOCATION itself for lack of a matching total, with an explicit warning
# rather than silently fabricating one.
#
# Uncertainty: this version REMOVES the earlier allocated_*_lo/_hi columns
# (built by independently combining pi_lower/rr_lower and pi_upper/rr_upper)
# because that is not a coherent joint 95% UI - profile prevalence
# components share a denominator, profile RRs are jointly dependent through
# one fitted model, and marginal lower/upper endpoints paired like that
# don't correspond to any real joint draw. Instead, Part 6 propagates
# uncertainty via actual Monte Carlo draws: lambda_draws (32_'s bootstrap)
# and core_profile_rr_hosp/death_draws (12_'s coefficient draws), combined
# INDEPENDENTLY draw-by-draw (two unrelated uncertainty sources), holding
# the known total_a fixed (see Part 6's header for why).
#
# Needs: 01_Data/country_age_burden_overall.rds
#        01_Data/brazil_clustering_ratio_r.RData (calibrated_ui, from 32_)
#        01_Data/brazil_clustering_lambda_draws.RData (lambda_draws, from 32_)
#        01_Data/core_profile_rr_hosp.RData, core_profile_rr_death.RData (from 12_)
#        01_Data/core_profile_rr_hosp_draws.RData, core_profile_rr_death_draws.RData (from 12_)
#        01_Data/gbd_pop.csv (Brazil population by 5-year age band, for weighting)
#        01_Data/brazil_profile_prevalence_two_extremes.RData (bg_prev_brazil3, from 29_)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(openxlsx); library(patchwork)
})
options(scipen = 999)

set.seed(20261006)

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"
font_family <- "Arial"
core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
lambda_band_levels <- c("0-19","20-39","40-59","60-79","80+")

## Chronological burden age bands and which broad lambda band each belongs
## to (every 10y band falls entirely inside exactly one lambda band, so this
## mapping is exact, not an approximation).
burden_age_bands <- tibble::tibble(
  age_start10 = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
  lambda_band = factor(
    c("0-19","0-19","20-39","20-39","40-59","40-59","60-79","60-79","80+","80+"),
    levels = lambda_band_levels
  )
)
make_age_label <- function(x) ifelse(x == 90, "90+", paste0(x, "-", x + 9))

## ============================================================
## Part 1: known totals - inspect country_age_burden_overall.rds for 90+
## (Case A/B/C) before assuming anything.
## ============================================================

burden_raw <- readRDS("01_Data/country_age_burden_overall.rds")

cat("[Part 1] names(burden_raw):\n"); print(names(burden_raw))
cat("\n[Part 1] unique age_group (whole file, all countries):\n")
print(sort(unique(as.character(burden_raw$age_group))))

## NOTE: checking the age_start (first number in the label, e.g. 80 for
## "[80,90)"), not a substring match on the label text - "[80,90)" itself
## contains the characters "90" as its upper bound, which would wrongly
## match a naive grepl("90", ...) even though it is not a 90+ group.
has_90plus_row <- any(as.numeric(stringr::str_extract(as.character(burden_raw$age_group), "[0-9]+")) >= 90)

burden_bra_all <- burden_raw |>
  dplyr::filter(country == "Brazil") |>
  dplyr::transmute(
    age_group,
    age_start10 = as.numeric(stringr::str_extract(as.character(age_group), "[0-9]+")),
    total_symptomatic = symptomatic_med,
    total_hosp = hospitalisation_med,
    total_death = fatal_med
  ) |>
  dplyr::arrange(age_start10)

cat("\n[Part 1] Brazil rows found in country_age_burden_overall.rds:\n")
print(as.data.frame(burden_bra_all), row.names = FALSE)

burden_case <- if (has_90plus_row) {
  if (all(c("total_hosp", "total_death") %in% names(burden_bra_all)) &&
      !anyNA(burden_bra_all$total_hosp[burden_bra_all$age_start10 >= 90])) {
    "A"
  } else {
    "B"
  }
} else {
  "C"
}

cat(sprintf("\n[Part 1] Case %s applies: ", burden_case))
if (burden_case == "C") {
  cat(
    "no 90+ row exists for ANY country (confirmed above) - total symptomatic\n",
    "cases, hospitalisations and deaths for age 90+ are simply unavailable in\n",
    "this upstream burden file. NOT fabricating a 90+ absolute burden.\n",
    "The burden allocation below is restricted to ages 0-89; the 90+\n",
    "profile-prevalence group is still constructed (Part 2) for completeness\n",
    "but is excluded from the allocation itself for lack of a matching total.\n"
  )
}
## (Case A/B branches are implemented for completeness/documentation even
## though Case C is what actually occurs for this upstream file - if a
## future version of country_age_burden_overall.rds adds a 90+ row, this
## script will take Case A automatically; Case B's rate-carry-forward logic
## is written out below for the same reason, guarded so it never silently
## runs when Case C applies.)

burden_bra <- burden_bra_all |> dplyr::filter(age_start10 <= 80)

if (burden_case == "B") {
  rate_hosp_80_89 <- burden_bra_all$total_hosp[burden_bra_all$age_start10 == 80] /
    burden_bra_all$total_symptomatic[burden_bra_all$age_start10 == 80]
  rate_death_80_89 <- burden_bra_all$total_death[burden_bra_all$age_start10 == 80] /
    burden_bra_all$total_symptomatic[burden_bra_all$age_start10 == 80]
  symptomatic_90plus <- burden_bra_all$total_symptomatic[burden_bra_all$age_start10 == 90]
  burden_90plus <- tibble::tibble(
    age_start10 = 90,
    total_symptomatic = symptomatic_90plus,
    total_hosp = symptomatic_90plus * rate_hosp_80_89,
    total_death = symptomatic_90plus * rate_death_80_89
  )
  burden_bra <- dplyr::bind_rows(burden_bra, burden_90plus)
  cat("[Part 1] Case B: 90+ hospitalisation/death extrapolated using the 80-89 RATE (not the absolute count) applied to 90+ symptomatic cases.\n")
}
if (burden_case == "A") {
  burden_bra <- burden_bra_all
  cat("[Part 1] Case A: 90+ hospitalisation/death burden taken directly from the upstream file.\n")
}

cat("\n[Part 1] Final burden_bra (totals actually used for allocation):\n")
print(as.data.frame(burden_bra), row.names = FALSE)

## ============================================================
## Part 2: profile prevalence, 10-year burden bands (BUG FIXED: no capping
## at 80 - 90-94/95+ now form their own 90+ group instead of leaking into
## 80-89).
## ============================================================

load("01_Data/brazil_clustering_ratio_r.RData")       # calibrated_ui, lambda_table
load("01_Data/brazil_profile_prevalence_two_extremes.RData")  # bg_prev_brazil3 (source marginals)

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

pop_raw <- read.csv("01_Data/gbd_pop.csv")

pop_bra <- pop_raw |>
  dplyr::filter(measure_name == "Population", metric_name == "Number", location_name == "Brazil") |>
  dplyr::mutate(age_clean = stringr::str_remove(age_name, " years| year")) |>
  dplyr::filter(age_clean %in% keep_age) |>
  dplyr::group_by(age_clean) |>
  dplyr::summarise(population = sum(val), .groups = "drop")

profile_5y <- calibrated_ui |>
  dplyr::select(age_clean, age_start, age_end, profile, p_indep, p_maxclust, p_point, p_lower, p_upper) |>
  dplyr::left_join(pop_bra, by = "age_clean") |>
  dplyr::mutate(
    ## FIXED: true decade floor, no pmin(..., 80) cap. 90-94 and 95+ both
    ## floor to 90, forming their own 90+ group rather than being folded
    ## into 80-89.
    age_start10 = floor(age_start / 10) * 10
  )

## Population-weighted mean of p_indep/p_maxclust (precomputed ONCE here,
## reused for every Monte Carlo draw in Part 6 - see that section's header
## for why this is valid: population-weighted aggregation is linear, so
## aggregate(lambda-mixture) == lambda-mixture(aggregate), letting the
## expensive weighted-mean step happen a single time instead of per draw).
profile_10y_bounds <- profile_5y |>
  dplyr::group_by(age_start10, profile) |>
  dplyr::summarise(
    p_indep_10y = stats::weighted.mean(p_indep, population),
    p_maxclust_10y = stats::weighted.mean(p_maxclust, population),
    .groups = "drop"
  ) |>
  dplyr::left_join(burden_age_bands, by = "age_start10")

## Point-estimate profile prevalence per 10y band (for the point-estimate
## allocation path and the heatmap/share figures).
profile_10y <- profile_5y |>
  dplyr::group_by(age_start10, profile) |>
  dplyr::summarise(
    pi_point = stats::weighted.mean(p_point, population),
    pi_lower = stats::weighted.mean(p_lower, population),
    pi_upper = stats::weighted.mean(p_upper, population),
    .groups = "drop"
  )

chk_sum10 <- profile_10y |> dplyr::group_by(age_start10) |> dplyr::summarise(s = sum(pi_point), .groups = "drop")
cat("\n[VALIDATION][Part 2] Sum of 8 profile prevalences per 10-year band (expect 1), now including the corrected 90+ group:\n")
print(as.data.frame(chk_sum10), row.names = FALSE, digits = 6)
stopifnot(all(abs(chk_sum10$s - 1) < 1e-6))

## Reconstructed marginal check at the 10-year-band level too (32_ already
## validates this at the 5-year level; repeating it post-aggregation guards
## against an aggregation-step bug specifically).
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
  dplyr::mutate(age_start10 = floor(age_start5 / 10) * 10) |>
  dplyr::group_by(age_start10) |>
  dplyr::summarise(population = sum(val), .groups = "drop")

marginal_source_10y <- bg_prev_brazil3 |>
  dplyr::left_join(pop_bra, by = "age_clean") |>
  dplyr::mutate(age_start10 = floor(age_start / 10) * 10) |>
  dplyr::group_by(age_start10) |>
  dplyr::summarise(
    DM = stats::weighted.mean(DM, population),
    HTN = stats::weighted.mean(HTN, population),
    CKD = stats::weighted.mean(CKD, population),
    .groups = "drop"
  )

reconstruct_marginal_10y <- function(condition_name) {
  profile_10y |>
    dplyr::filter(grepl(condition_name, profile, fixed = TRUE)) |>
    dplyr::group_by(age_start10) |>
    dplyr::summarise(reconstructed = sum(pi_point), .groups = "drop")
}

marginal_check_10y <- marginal_source_10y |>
  dplyr::left_join(reconstruct_marginal_10y("DM") |> dplyr::rename(DM_reconstructed = reconstructed), by = "age_start10") |>
  dplyr::left_join(reconstruct_marginal_10y("HTN") |> dplyr::rename(HTN_reconstructed = reconstructed), by = "age_start10") |>
  dplyr::left_join(reconstruct_marginal_10y("CKD") |> dplyr::rename(CKD_reconstructed = reconstructed), by = "age_start10") |>
  dplyr::mutate(
    DM_diff = DM - DM_reconstructed, HTN_diff = HTN - HTN_reconstructed, CKD_diff = CKD - CKD_reconstructed
  )
cat("\n[VALIDATION][Part 2] Reconstructed marginal (10y-band level) vs source marginal, max abs diff:\n")
cat("DM:", max(abs(marginal_check_10y$DM_diff)),
    " HTN:", max(abs(marginal_check_10y$HTN_diff)),
    " CKD:", max(abs(marginal_check_10y$CKD_diff)), "\n")
stopifnot(
  max(abs(marginal_check_10y$DM_diff)) < 1e-6,
  max(abs(marginal_check_10y$HTN_diff)) < 1e-6,
  max(abs(marginal_check_10y$CKD_diff)) < 1e-6
)

## ============================================================
## Part 3: pooled profile-specific RR (from 12_'s core_profile_rr_hosp/death)
## ============================================================

load("01_Data/core_profile_rr_hosp.RData")   # core_profile_rr_hosp
load("01_Data/core_profile_rr_death.RData")  # core_profile_rr_death

rr_hosp <- core_profile_rr_hosp |> dplyr::select(profile = core_profile, rr_hosp = rr, rr_hosp_lower = rr_lower, rr_hosp_upper = rr_upper)
rr_death <- core_profile_rr_death |> dplyr::select(profile = core_profile, rr_death = rr, rr_death_lower = rr_lower, rr_death_upper = rr_upper)

## ============================================================
## Part 4: point-estimate RR-weighted allocation of the known totals
## ============================================================

allocate <- function(pi_k, rr_k, total) {
  w <- pi_k * rr_k
  total * w / sum(w)
}

alloc_hosp <- profile_10y |>
  dplyr::filter(age_start10 %in% burden_bra$age_start10) |>
  dplyr::left_join(rr_hosp, by = "profile") |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_hosp, total_symptomatic), by = "age_start10") |>
  dplyr::group_by(age_start10) |>
  dplyr::mutate(
    allocated_hosp = allocate(pi_point, rr_hosp, dplyr::first(total_hosp)),
    symptomatic_profile = total_symptomatic * pi_point
  ) |>
  dplyr::ungroup()

alloc_death <- profile_10y |>
  dplyr::filter(age_start10 %in% burden_bra$age_start10) |>
  dplyr::left_join(rr_death, by = "profile") |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_death, total_symptomatic), by = "age_start10") |>
  dplyr::group_by(age_start10) |>
  dplyr::mutate(
    allocated_death = allocate(pi_point, rr_death, dplyr::first(total_death)),
    symptomatic_profile = total_symptomatic * pi_point
  ) |>
  dplyr::ungroup()

chk_hosp <- alloc_hosp |> dplyr::group_by(age_start10) |> dplyr::summarise(s = sum(allocated_hosp), t = dplyr::first(total_hosp), .groups = "drop")
chk_death <- alloc_death |> dplyr::group_by(age_start10) |> dplyr::summarise(s = sum(allocated_death), t = dplyr::first(total_death), .groups = "drop")
cat("\n[VALIDATION][Part 4] Allocated hospitalisations sum vs known total, per age band:\n")
print(as.data.frame(chk_hosp), row.names = FALSE)
cat("[VALIDATION][Part 4] Allocated deaths sum vs known total, per age band:\n")
print(as.data.frame(chk_death), row.names = FALSE)
stopifnot(
  all(abs(chk_hosp$s - chk_hosp$t) < 1e-6 * pmax(1, chk_hosp$t)),
  all(abs(chk_death$s - chk_death$t) < 1e-6 * pmax(1, chk_death$t))
)

## ============================================================
## Part 5: profile-specific outcome burden per symptomatic case (point
## estimate) - zero denominators are NA, never 0.
## ============================================================

alloc_hosp <- alloc_hosp |>
  dplyr::mutate(
    hosp_rate_profile = ifelse(symptomatic_profile > 0, allocated_hosp / symptomatic_profile, NA_real_),
    hosp_rate_100k = hosp_rate_profile * 1e5
  )
alloc_death <- alloc_death |>
  dplyr::mutate(
    death_rate_profile = ifelse(symptomatic_profile > 0, allocated_death / symptomatic_profile, NA_real_),
    death_rate_100k = death_rate_profile * 1e5
  )

save(alloc_hosp, alloc_death, file = "01_Data/brazil_profile_burden_allocation.RData")

## ============================================================
## Part 6: draw-based uncertainty propagation (replaces the old, invalid
## allocated_*_lo/_hi built from independent pi_lower/rr_lower and
## pi_upper/rr_upper marginal endpoints).
##
## Two INDEPENDENT Monte Carlo sources are combined draw-by-draw:
##   - lambda_draws (32_'s SINAN bootstrap, 1000 draws per broad age band)
##   - core_profile_rr_hosp_draws / core_profile_rr_death_draws (12_'s
##     MASS::mvrnorm coefficient draws - all 8 profiles from the SAME
##     coefficient draw, so the RR vector used in one Monte Carlo
##     replicate is internally coherent, not 8 independently-sampled CIs)
## For each replicate b: pick one lambda draw per broad age band and one RR
## draw (all 8 profiles), compute pi_ag^(b), then allocate exactly as in
## Part 4 but with total_a held FIXED at its point estimate (country_age_
## burden_overall.rds has only _med/_lo/_hi, not real draws - per the
## explicit instruction behind this script, lower/upper endpoints are not
## used to invent a distribution for the total). Aggregating the 5-year
## p_indep/p_maxclust to 10-year bands is linear, so it was already done
## ONCE in Part 2 (profile_10y_bounds) rather than redone on every draw.
##
## 95% UI reflects uncertainty in clustering and profile-specific relative
## risks, conditional on the estimated total age-specific burden.
## ============================================================

load("01_Data/brazil_clustering_lambda_draws.RData")        # lambda_draws
load("01_Data/core_profile_rr_hosp_draws.RData")             # core_profile_rr_hosp_draws
load("01_Data/core_profile_rr_death_draws.RData")            # core_profile_rr_death_draws

bounds_wide <- profile_10y_bounds |>
  dplyr::filter(age_start10 %in% burden_bra$age_start10) |>
  dplyr::mutate(profile = factor(profile, levels = core_profile_levels))

B_final <- 1000

run_draw_allocation <- function(lambda_draws, rr_draws_tidy, totals, total_col, seed) {
  set.seed(seed)
  n_lambda <- max(lambda_draws$draw)
  n_rr <- max(rr_draws_tidy$draw)
  lambda_pick <- sample.int(n_lambda, B_final, replace = TRUE)
  rr_pick <- sample.int(n_rr, B_final, replace = TRUE)

  purrr::map_dfr(seq_len(B_final), function(b) {
    lambda_b <- lambda_draws |>
      dplyr::filter(draw == lambda_pick[b]) |>
      dplyr::select(lambda_band = age_band_profile, lambda)
    rr_b <- rr_draws_tidy |>
      dplyr::filter(draw == rr_pick[b]) |>
      dplyr::select(profile, rr)

    bounds_wide |>
      dplyr::left_join(lambda_b, by = "lambda_band") |>
      dplyr::left_join(rr_b, by = "profile") |>
      dplyr::mutate(pi_b = (1 - lambda) * p_indep_10y + lambda * p_maxclust_10y) |>
      dplyr::left_join(totals, by = "age_start10") |>
      dplyr::group_by(age_start10) |>
      dplyr::mutate(
        w = pi_b * rr,
        allocated = .data[[total_col]] * w / sum(w)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(draw_id = b) |>
      dplyr::select(draw_id, age_start10, profile, pi_b, rr, allocated)
  })
}

draws_hosp <- run_draw_allocation(
  lambda_draws, core_profile_rr_hosp_draws,
  burden_bra |> dplyr::select(age_start10, total_hosp, total_symptomatic),
  "total_hosp", seed = 101
)
draws_death <- run_draw_allocation(
  lambda_draws, core_profile_rr_death_draws,
  burden_bra |> dplyr::select(age_start10, total_death, total_symptomatic),
  "total_death", seed = 202
)

## Per-draw sum check: every single replicate must reproduce the known total
## exactly (the allocation formula guarantees this algebraically; verifying
## it numerically catches any join/bookkeeping bug).
chk_draw_hosp <- draws_hosp |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_hosp), by = "age_start10") |>
  dplyr::group_by(draw_id, age_start10) |>
  dplyr::summarise(s = sum(allocated), t = dplyr::first(total_hosp), .groups = "drop") |>
  dplyr::mutate(ok = abs(s - t) < 1e-6 * pmax(1, t))
cat("\n[VALIDATION][Part 6] Every draw's allocated hospitalisations reproduce the known total (hosp):",
    sum(chk_draw_hosp$ok), "/", nrow(chk_draw_hosp), "\n")
stopifnot(all(chk_draw_hosp$ok))

chk_draw_death <- draws_death |>
  dplyr::left_join(burden_bra |> dplyr::select(age_start10, total_death), by = "age_start10") |>
  dplyr::group_by(draw_id, age_start10) |>
  dplyr::summarise(s = sum(allocated), t = dplyr::first(total_death), .groups = "drop") |>
  dplyr::mutate(ok = abs(s - t) < 1e-6 * pmax(1, t))
cat("[VALIDATION][Part 6] Every draw's allocated deaths reproduce the known total (death):",
    sum(chk_draw_death$ok), "/", nrow(chk_draw_death), "\n")
stopifnot(all(chk_draw_death$ok))

## Draw-level rate per 100,000 symptomatic cases (symptomatic_profile^(b) =
## total_symptomatic x the SAME per-draw pi_b, so numerator and denominator
## of the rate come from one internally-consistent draw).
draws_hosp <- draws_hosp |>
  dplyr::mutate(
    symptomatic_profile_b = pi_b * burden_bra$total_symptomatic[match(age_start10, burden_bra$age_start10)],
    hosp_rate_100k_b = ifelse(symptomatic_profile_b > 0, allocated / symptomatic_profile_b * 1e5, NA_real_)
  )
draws_death <- draws_death |>
  dplyr::mutate(
    symptomatic_profile_b = pi_b * burden_bra$total_symptomatic[match(age_start10, burden_bra$age_start10)],
    death_rate_100k_b = ifelse(symptomatic_profile_b > 0, allocated / symptomatic_profile_b * 1e5, NA_real_)
  )

## Median / 95% UI per age x profile, from the draws.
alloc_hosp_ui <- draws_hosp |>
  dplyr::group_by(age_start10, profile) |>
  dplyr::summarise(
    allocated_hosp_median = stats::median(allocated),
    allocated_hosp_lower = stats::quantile(allocated, 0.025, names = FALSE),
    allocated_hosp_upper = stats::quantile(allocated, 0.975, names = FALSE),
    hosp_rate_100k_median = stats::median(hosp_rate_100k_b, na.rm = TRUE),
    hosp_rate_100k_lower = stats::quantile(hosp_rate_100k_b, 0.025, na.rm = TRUE, names = FALSE),
    hosp_rate_100k_upper = stats::quantile(hosp_rate_100k_b, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )
alloc_death_ui <- draws_death |>
  dplyr::group_by(age_start10, profile) |>
  dplyr::summarise(
    allocated_death_median = stats::median(allocated),
    allocated_death_lower = stats::quantile(allocated, 0.025, names = FALSE),
    allocated_death_upper = stats::quantile(allocated, 0.975, names = FALSE),
    death_rate_100k_median = stats::median(death_rate_100k_b, na.rm = TRUE),
    death_rate_100k_lower = stats::quantile(death_rate_100k_b, 0.025, na.rm = TRUE, names = FALSE),
    death_rate_100k_upper = stats::quantile(death_rate_100k_b, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

alloc_hosp <- alloc_hosp |> dplyr::left_join(alloc_hosp_ui, by = c("age_start10", "profile"))
alloc_death <- alloc_death |> dplyr::left_join(alloc_death_ui, by = c("age_start10", "profile"))

save(alloc_hosp, alloc_death, draws_hosp, draws_death,
     file = "01_Data/brazil_profile_burden_allocation.RData")

message("[34][Part 6] Draw-based 95% UI computed (", B_final, " Monte Carlo replicates per outcome).")

## ============================================================
## Part 6b: the same 3 collapsed clinical categories, draw-level, for the
## section-10 comparison figure.
## ============================================================

profile_group_levels <- c("No core condition", "One core condition", "Multimorbidity (≥2 core conditions)")
profile_to_group <- c(
  "None" = "No core condition",
  "DM" = "One core condition", "HTN" = "One core condition", "CKD" = "One core condition",
  "DM+HTN" = "Multimorbidity (≥2 core conditions)", "DM+CKD" = "Multimorbidity (≥2 core conditions)",
  "HTN+CKD" = "Multimorbidity (≥2 core conditions)", "DM+HTN+CKD" = "Multimorbidity (≥2 core conditions)"
)

summarise_group_draws <- function(draws, rate_col_b, total_col) {
  draws |>
    dplyr::mutate(group = factor(profile_to_group[as.character(profile)], levels = profile_group_levels)) |>
    dplyr::group_by(draw_id, age_start10, group) |>
    dplyr::summarise(
      allocated_g = sum(allocated),
      symptomatic_g = sum(symptomatic_profile_b),
      .groups = "drop"
    ) |>
    dplyr::mutate(rate_100k_g = ifelse(symptomatic_g > 0, allocated_g / symptomatic_g * 1e5, NA_real_)) |>
    dplyr::group_by(age_start10, group) |>
    dplyr::summarise(
      rate_100k_median = stats::median(rate_100k_g, na.rm = TRUE),
      rate_100k_lower = stats::quantile(rate_100k_g, 0.025, na.rm = TRUE, names = FALSE),
      rate_100k_upper = stats::quantile(rate_100k_g, 0.975, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

group_hosp_ui <- summarise_group_draws(draws_hosp, hosp_rate_100k_b, "total_hosp") |> dplyr::mutate(outcome = "Hospitalisation")
group_death_ui <- summarise_group_draws(draws_death, death_rate_100k_b, "total_death") |> dplyr::mutate(outcome = "Death")
group_burden_ui <- dplyr::bind_rows(group_hosp_ui, group_death_ui)

save(group_burden_ui, file = "01_Data/brazil_profile_burden_group_ui.RData")

## ============================================================
## Part 7: table export (point estimate + draw-based median/95% UI; the
## old, invalid marginal-endpoint allocated_*_lo/_hi columns are gone)
## ============================================================

alloc_table <- alloc_hosp |>
  dplyr::select(
    age_start10, profile, pi_point, rr_hosp,
    allocated_hosp, allocated_hosp_median, allocated_hosp_lower, allocated_hosp_upper,
    hosp_rate_100k, hosp_rate_100k_median, hosp_rate_100k_lower, hosp_rate_100k_upper,
    symptomatic_profile, total_hosp
  ) |>
  dplyr::left_join(
    alloc_death |> dplyr::select(
      age_start10, profile, rr_death,
      allocated_death, allocated_death_median, allocated_death_lower, allocated_death_upper,
      death_rate_100k, death_rate_100k_median, death_rate_100k_lower, death_rate_100k_upper,
      total_death
    ),
    by = c("age_start10", "profile")
  ) |>
  dplyr::mutate(age_band = make_age_label(age_start10)) |>
  dplyr::arrange(age_start10, factor(profile, levels = core_profile_levels)) |>
  dplyr::relocate(age_band, .after = age_start10)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "profile_burden_allocation")
openxlsx::writeDataTable(wb, "profile_burden_allocation", alloc_table, withFilter = TRUE)
openxlsx::setColWidths(wb, "profile_burden_allocation", cols = 1:ncol(alloc_table), widths = "auto")
openxlsx::saveWorkbook(wb, file.path(table_dir, "brazil_profile_burden_allocation_PROTOTYPE.xlsx"), overwrite = TRUE)

## ============================================================
## Part 8: figures - legacy absolute/None-vs-comorbid bars, relabelled
## ("None" as a profile tag is fine in the 8-profile figure; the 2-category
## comparator's wording is fixed per instruction - "No underlying condition"
## could be read as implying no condition of ANY kind, when these patients
## may have conditions outside DM/HTN/CKD).
## ============================================================

profile_colours <- c(
  "None" = "#999999", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

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

plot_none_vs_comorbid <- plot_alloc |>
  dplyr::mutate(group2 = ifelse(profile == "None", "No core condition", "≥1 core condition")) |>
  dplyr::group_by(age_label, outcome, group2) |>
  dplyr::summarise(allocated = sum(allocated), .groups = "drop") |>
  dplyr::mutate(group2 = factor(group2, levels = c("No core condition", "≥1 core condition")))

group2_colours <- c("No core condition" = "#999999", "≥1 core condition" = "#C44E52")

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

## ============================================================
## Part 9: HEATMAP - age x profile x outcome-burden-per-100k (the main new
## comparison figure; the stacked-absolute-count figure above is dominated
## visually by "None" simply because it is the largest group, and does not
## directly show how burden differs WITHIN an age group across profiles).
## ============================================================

heatmap_data <- dplyr::bind_rows(
  alloc_hosp |> dplyr::transmute(age_start10, profile, rate_100k = hosp_rate_100k, outcome = "A    Hospitalisation"),
  alloc_death |> dplyr::transmute(age_start10, profile, rate_100k = death_rate_100k, outcome = "B    Death")
) |>
  dplyr::mutate(
    age_label = factor(make_age_label(age_start10), levels = make_age_label(sort(unique(age_start10)))),
    profile = factor(profile, levels = rev(core_profile_levels)),  # None at bottom -> top-to-bottom = None..DM+HTN+CKD
    outcome = factor(outcome, levels = c("A    Hospitalisation", "B    Death"))
  )

wb2 <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb2, "profile_burden_rates")
openxlsx::writeDataTable(wb2, "profile_burden_rates", heatmap_data |> dplyr::select(age_start10, age_label, profile, outcome, rate_100k), withFilter = TRUE)
openxlsx::saveWorkbook(wb2, file.path(table_dir, "brazil_profile_burden_rates_PROTOTYPE.xlsx"), overwrite = TRUE)

plot_heatmap_outcome <- function(data_outcome, legend_label) {
  ggplot2::ggplot(data_outcome, ggplot2::aes(x = age_label, y = profile, fill = rate_100k)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = scales::label_number(accuracy = 0.1)(rate_100k)), size = 2.5, colour = "#1A1A1A") +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, name = legend_label) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic(base_size = 9.5, base_family = font_family) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.key.height = grid::unit(10, "mm")
    ) +
    ggplot2::facet_wrap(~outcome, ncol = 1)
}

fig_heat_hosp <- plot_heatmap_outcome(
  heatmap_data |> dplyr::filter(outcome == "A    Hospitalisation"),
  "Hospitalisations per\n100,000 symptomatic cases"
)
fig_heat_death <- plot_heatmap_outcome(
  heatmap_data |> dplyr::filter(outcome == "B    Death"),
  "Deaths per\n100,000 symptomatic cases"
)

fig_heatmap <- (fig_heat_hosp / fig_heat_death) +
  patchwork::plot_annotation(
    title = "Age- and comorbidity-profile-specific severe chikungunya burden in Brazil",
    subtitle = "Modelled rates within mutually exclusive DM/HTN/CKD profiles",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12.5, family = font_family, colour = "#1A1A1A"),
      plot.subtitle = ggplot2::element_text(size = 9, family = font_family, colour = "#4D4D4D", margin = ggplot2::margin(b = 6))
    )
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_rate_heatmap_brazil_PROTOTYPE.png"),
                 fig_heatmap, width = 180, height = 210, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_rate_heatmap_brazil_PROTOTYPE.pdf"),
                 fig_heatmap, width = 180, height = 210, units = "mm", device = grDevices::cairo_pdf)

message("[34][Part 9] Saved: fig_profile_burden_rate_heatmap_brazil_PROTOTYPE.png/.pdf + brazil_profile_burden_rates_PROTOTYPE.xlsx")

## ============================================================
## Part 10: simpler 3-category clinical comparison, with 95% UI ribbons
## from the draw-based propagation (Part 6b).
## ============================================================

point_group <- dplyr::bind_rows(
  alloc_hosp |> dplyr::transmute(age_start10, profile, allocated = allocated_hosp, symptomatic_profile, outcome = "Hospitalisation"),
  alloc_death |> dplyr::transmute(age_start10, profile, allocated = allocated_death, symptomatic_profile, outcome = "Death")
) |>
  dplyr::mutate(group = factor(profile_to_group[as.character(profile)], levels = profile_group_levels)) |>
  dplyr::group_by(age_start10, outcome, group) |>
  dplyr::summarise(
    allocated_g = sum(allocated), symptomatic_g = sum(symptomatic_profile), .groups = "drop"
  ) |>
  dplyr::mutate(rate_100k = ifelse(symptomatic_g > 0, allocated_g / symptomatic_g * 1e5, NA_real_))

plot_group <- point_group |>
  dplyr::left_join(group_burden_ui, by = c("age_start10", "outcome", "group")) |>
  dplyr::mutate(
    age_mid = age_start10 + ifelse(age_start10 == 90, 2.5, 5),
    outcome = factor(outcome, levels = c("Hospitalisation", "Death"))
  )

group_colours <- c(
  "No core condition" = "#999999", "One core condition" = "#4C72B0",
  "Multimorbidity (≥2 core conditions)" = "#C44E52"
)

fig_group_comparison <- ggplot2::ggplot(plot_group, ggplot2::aes(x = age_mid, y = rate_100k, colour = group, fill = group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = rate_100k_lower, ymax = rate_100k_upper), colour = NA, alpha = 0.18) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.4) +
  ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  ggplot2::scale_colour_manual(values = group_colours) +
  ggplot2::scale_fill_manual(values = group_colours) +
  ggplot2::scale_x_continuous(breaks = c(burden_age_bands$age_start10), labels = make_age_label(burden_age_bands$age_start10)) +
  ggplot2::labs(
    x = "Age band (years)", y = "Burden rate per 100,000 symptomatic cases", colour = NULL, fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "top",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_burden_by_comorbidity_status_age_brazil_PROTOTYPE.png"),
                 fig_group_comparison, width = 160, height = 160, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_burden_by_comorbidity_status_age_brazil_PROTOTYPE.pdf"),
                 fig_group_comparison, width = 160, height = 160, units = "mm", device = grDevices::cairo_pdf)

message("[34][Part 10] Saved: fig_burden_by_comorbidity_status_age_brazil_PROTOTYPE.png/.pdf")

## ============================================================
## Part 11: burden-composition 100%-stacked area figures (calibrated
## scenario only - the current main estimate), the burden analogue of
## fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png.
## ============================================================

burden_share_data <- dplyr::bind_rows(
  alloc_hosp |> dplyr::group_by(age_start10) |> dplyr::mutate(share = allocated_hosp / sum(allocated_hosp)) |> dplyr::ungroup() |>
    dplyr::transmute(age_start10, profile, share, outcome = "A    Hospitalisation"),
  alloc_death |> dplyr::group_by(age_start10) |> dplyr::mutate(share = allocated_death / sum(allocated_death)) |> dplyr::ungroup() |>
    dplyr::transmute(age_start10, profile, share, outcome = "B    Death")
) |>
  dplyr::mutate(
    age_mid = age_start10 + ifelse(age_start10 == 90, 2.5, 5),
    profile = factor(profile, levels = rev(core_profile_levels)),
    outcome = factor(outcome, levels = c("A    Hospitalisation", "B    Death"))
  )

fig_burden_share <- ggplot2::ggplot(burden_share_data, ggplot2::aes(x = age_mid, y = share, fill = profile)) +
  ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.12) +
  ggplot2::facet_wrap(~outcome, ncol = 2) +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_x_continuous(breaks = burden_age_bands$age_start10 + 5, labels = make_age_label(burden_age_bands$age_start10), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  ggplot2::labs(
    title = "Age-specific distribution of severe chikungunya burden across comorbidity profiles",
    x = "Age band (years)", y = "Share of age-specific outcome burden", fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 10, base_family = font_family) +
  ggplot2::theme(
    legend.position = "bottom",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    plot.title = ggplot2::element_text(face = "bold", size = 11, family = font_family)
  )

ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_share_calibrated_brazil_PROTOTYPE.png"),
                 fig_burden_share, width = 200, height = 110, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_burden_share_calibrated_brazil_PROTOTYPE.pdf"),
                 fig_burden_share, width = 200, height = 110, units = "mm", device = grDevices::cairo_pdf)

message("[34][Part 11] Saved: fig_profile_burden_share_calibrated_brazil_PROTOTYPE.png/.pdf")

## ============================================================
## Part 12: FINAL VALIDATION REPORT
## ============================================================

cat("\n\n========================================================\n")
cat("FINAL VALIDATION REPORT\n")
cat("========================================================\n")

cat("\n-- Profile-prevalence row sums by age (10y bands) --\n")
print(as.data.frame(chk_sum10), row.names = FALSE, digits = 6)

cat("\n-- Reconstructed DM/HTN/CKD marginal vs source marginal (max abs diff, 10y-band level) --\n")
cat("DM:", max(abs(marginal_check_10y$DM_diff)),
    " HTN:", max(abs(marginal_check_10y$HTN_diff)),
    " CKD:", max(abs(marginal_check_10y$CKD_diff)), "\n")

cat("\n-- Hospitalisation allocations vs known totals --\n")
print(as.data.frame(chk_hosp), row.names = FALSE)

cat("\n-- Death allocations vs known totals --\n")
print(as.data.frame(chk_death), row.names = FALSE)

cat("\n-- Raw lambda bootstrap draws outside [0,1] (from 32_'s lambda_draws) --\n")
n_lambda_out <- sum(lambda_draws$lambda_raw < 0 | lambda_draws$lambda_raw > 1)
cat(sprintf("%d / %d (%.3f%%)\n", n_lambda_out, nrow(lambda_draws), 100 * n_lambda_out / nrow(lambda_draws)))

cat("\n-- Age bands present in the final burden-allocation data --\n")
print(sort(unique(alloc_hosp$age_start10)))
cat("90+ included in burden ALLOCATION:", 90 %in% alloc_hosp$age_start10,
    "(90+ profile prevalence was still constructed in Part 2, but excluded from the allocation itself)\n")

cat("\n-- 90+ overall burden: directly observed or extrapolated? --\n")
cat(sprintf("Case %s. ", burden_case))
if (burden_case == "C") cat("Neither - no 90+ row exists anywhere in country_age_burden_overall.rds, so 90+ is simply absent from the burden analysis (not fabricated).\n")
if (burden_case == "B") cat("Extrapolated: 90+ hospitalisation/death = 90+ symptomatic cases x the 80-89 outcome RATE (rate carried forward, not the absolute count).\n")
if (burden_case == "A") cat("Directly observed in the upstream file.\n")

cat("\n-- Uncertainty in total age-specific burden: propagated or held fixed? --\n")
cat("Held FIXED at its point estimate (country_age_burden_overall.rds provides only _med/_lo/_hi, not real draws).\n")
cat("95% UI reflects uncertainty in clustering and profile-specific relative risks, conditional on the estimated total age-specific burden.\n")

cat("\n========================================================\n")
cat("SCRIPTS MODIFIED / OUTPUTS CREATED THIS REVIEW\n")
cat("========================================================\n")
cat("Modified:\n")
cat("  02_Script/12_core3_condition_specific_rr.R - standardise_profile_rr() now optionally saves\n")
cat("    the draw-level RR matrix (draws_out=); core_profile_rr_hosp/death summaries UNCHANGED\n")
cat("    (same seed/model). New cache files: core_profile_rr_hosp_draws.RData, core_profile_rr_death_draws.RData.\n")
cat("  02_Script/29_background_prevalence_core3_brazil.R - figure wording 'Independence (minimum\n")
cat("    clustering)' -> 'Independence'.\n")
cat("  02_Script/32_clustering_ratio_r_brazil.R - renamed r -> clustering weight lambda throughout;\n")
cat("    added lambda_draws (raw + [0,1]-clamped) and out-of-range reporting; added profile-sum and\n")
cat("    marginal-reconstruction validation; updated figure title/axis wording.\n")
cat("  02_Script/34_profile_burden_allocation_brazil.R - fixed the 90-94/95+ -> 80-89 age-mapping bug;\n")
cat("    Case A/B/C 90+ burden handling (Case C applies here); removed the invalid\n")
cat("    allocated_*_lo/_hi; added draw-based Monte Carlo 95% UI; added profile-specific symptomatic\n")
cat("    case counts and rates per 100,000; added the heatmap, 3-category, and burden-share figures;\n")
cat("    relabelled the None-vs-comorbid comparator.\n")
cat("\nNew data outputs:\n")
cat("  01_Data/core_profile_rr_hosp_draws.RData, core_profile_rr_death_draws.RData\n")
cat("  01_Data/brazil_clustering_lambda_draws.RData\n")
cat("  01_Data/brazil_profile_burden_allocation.RData (now also includes draws_hosp/draws_death)\n")
cat("  01_Data/brazil_profile_burden_group_ui.RData\n")
cat("\nNew figures:\n")
cat("  03_Output/figures/fig_profile_burden_rate_heatmap_brazil_PROTOTYPE.png/.pdf\n")
cat("  03_Output/figures/fig_burden_by_comorbidity_status_age_brazil_PROTOTYPE.png/.pdf\n")
cat("  03_Output/figures/fig_profile_burden_share_calibrated_brazil_PROTOTYPE.png/.pdf\n")
cat("\nNew tables:\n")
cat("  03_Output/tables/brazil_profile_burden_rates_PROTOTYPE.xlsx\n")
cat("  03_Output/tables/brazil_profile_burden_allocation_PROTOTYPE.xlsx (regenerated, new columns)\n")
cat("[34] DONE.\n")
