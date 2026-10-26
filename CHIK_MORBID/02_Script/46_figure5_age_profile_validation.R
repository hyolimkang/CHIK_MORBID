# ---------------------------------------------------------------------------
# 46_figure5_age_profile_validation.R
#
# Figure 5 validation: age-specific associations between the eight joint
# core-3 profiles (DM/HTN/CKD) and chikungunya-attributed death.
#
# The original Figure 5 is the death row of
# 03_Output/figures/fig3_profile_age_specific_relative_risk.pdf/.png, built
# by 12_core3_condition_specific_rr.R section 12 from the factor-smooth GAM
#   death_only ~ core_profile + sex + year + s(uf_residence, bs = "re") +
#                s(age_years, core_profile, bs = "fs", k = 10)
# (bam, fREML, discrete) and cached as 01_Data/core_profile_rr_age_curve.RData.
#
# This script does NOT modify any original analysis, cache or figure. It:
#   1. rebuilds the exact death cohort used by script 12 (same add_core3()
#      eligibility rules, same died_from_chik outcome)
#   2. tabulates age-group x profile empirical support (Task 1)
#   3. refits the original fs model (verified against the cached curve), a
#      no-interaction model, and simpler/alternative interaction models, and
#      records fit, convergence and interaction diagnostics (Task 2)
#   4. re-estimates within-age-group standardised risks, RRs and RDs by
#      g-computation with the same coefficient-draw method (Task 3)
#   5. repeats 4 under alternative age groupings / model specifications
#      (Task 5)
# Figures are drawn separately by 47_figure5_age_profile_figures.R from the
# CSVs written here.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds,
#        01_Data/core_profile_rr_age_curve.RData (for the reproduction check)
# Output: 03_Output/validation/figure5_age_validation/
# Runtime: roughly 30-60 min (nine bam fits on ~1 million records, plus
#          coefficient-draw standardisation).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(MASS)
  library(mgcv)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
})

out_dir <- "03_Output/validation/figure5_age_validation"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

n_threads <- max(1, min(8, parallel::detectCores() - 2))
B_primary <- 2000   # same number of draws as the pooled profile RRs in script 12
B_sens <- 1000      # same number of draws as the original continuous curve

core_profile_levels <- c(
  "None",
  "DM", "HTN", "CKD",
  "DM+HTN", "DM+CKD", "HTN+CKD",
  "DM+HTN+CKD"
)

## Age schemes. "primary" is the grouping requested for the Figure 5 audit;
## "script12_bands" is the grouping script 12 section 9 already used; the
## others are progressively merged versions used in the robustness checks.
age_schemes <- list(
  primary        = list(breaks = c(0, 15, 40, 60, 75, 101),
                        labels = c("0-14", "15-39", "40-59", "60-74", "75+")),
  script12_bands = list(breaks = c(0, 20, 40, 60, 80, 101),
                        labels = c("0-19", "20-39", "40-59", "60-79", "80+")),
  merged_4       = list(breaks = c(0, 40, 60, 75, 101),
                        labels = c("0-39", "40-59", "60-74", "75+")),
  merged_3       = list(breaks = c(0, 60, 75, 101),
                        labels = c("0-59", "60-74", "75+")),
  merged_2       = list(breaks = c(0, 60, 101),
                        labels = c("0-59", "60+"))
)

cut_age <- function(age, scheme) {
  s <- age_schemes[[scheme]]
  cut(age, breaks = s$breaks, labels = s$labels, right = FALSE,
      include.lowest = TRUE)
}

## Diagnostic heuristic only - see FIGURE5_VALIDATION.md.
death_threshold <- 10

support_category <- function(deaths) {
  dplyr::case_when(
    deaths >= death_threshold ~ "Adequate",
    deaths >= 1               ~ "Sparse",
    TRUE                      ~ "No deaths"
  )
}

log_msg <- function(...) message(sprintf("[46 %s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...))

## ============================================================
## 1. Rebuild the exact script-12 death cohort
## ============================================================
## Identical eligibility and coding to add_core3() + the death-cohort filter
## in 12_core3_condition_specific_rr.R section 1 (copied rather than sourced,
## because sourcing script 12 would rerun every model in it).

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

death_cohort <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years),
    age_years >= 0,
    age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(
      dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
      ~ .x %in% c("no", "yes")
    )
  ) |>
  dplyr::mutate(
    year         = factor(lubridate::year(event_date)),
    sex          = factor(sex, levels = c("female", "male")),
    uf_residence = factor(uf_residence),
    dm   = as.integer(diabetes == "yes"),
    htn  = as.integer(hypertension == "yes"),
    ckd  = as.integer(renal_disease == "yes"),
    core_profile = dplyr::case_when(
      dm == 0 & htn == 0 & ckd == 0 ~ "None",
      dm == 1 & htn == 0 & ckd == 0 ~ "DM",
      dm == 0 & htn == 1 & ckd == 0 ~ "HTN",
      dm == 0 & htn == 0 & ckd == 1 ~ "CKD",
      dm == 1 & htn == 1 & ckd == 0 ~ "DM+HTN",
      dm == 1 & htn == 0 & ckd == 1 ~ "DM+CKD",
      dm == 0 & htn == 1 & ckd == 1 ~ "HTN+CKD",
      dm == 1 & htn == 1 & ckd == 1 ~ "DM+HTN+CKD"
    ),
    core_profile = factor(core_profile, levels = core_profile_levels)
  )

n_eligible <- nrow(death_cohort)

death_cohort <- death_cohort |>
  dplyr::filter(!is.na(died_from_chik)) |>
  dplyr::mutate(death_only = died_from_chik) |>
  dplyr::select(age_years, sex, year, uf_residence, core_profile, death_only)

rm(ind)
invisible(gc())

## Variables used only by the alternative specifications below; all are
## deterministic functions of age_years/core_profile, so they can be rebuilt
## on any prediction grid with the same helper.
add_model_vars <- function(df) {
  df |>
    dplyr::mutate(
      age10 = (age_years - 50) / 10,
      ## Profile-specific linear age slopes (log-odds per decade) relative
      ## to None; None's own age shape is carried by s(age_years).
      lin_DM         = age10 * (core_profile == "DM"),
      lin_HTN        = age10 * (core_profile == "HTN"),
      lin_CKD        = age10 * (core_profile == "CKD"),
      lin_DM_HTN     = age10 * (core_profile == "DM+HTN"),
      lin_DM_CKD     = age10 * (core_profile == "DM+CKD"),
      lin_HTN_CKD    = age10 * (core_profile == "HTN+CKD"),
      lin_DM_HTN_CKD = age10 * (core_profile == "DM+HTN+CKD"),
      age_group = cut_age(age_years, "primary"),
      profile_age = factor(
        paste(core_profile, age_group, sep = " | "),
        levels = as.vector(outer(core_profile_levels,
                                 age_schemes$primary$labels,
                                 paste, sep = " | "))
      )
    )
}

death_cohort <- add_model_vars(death_cohort)

cohort_check <- tibble::tibble(
  metric = c("Eligible core-3 population (before outcome filter)",
             "Mortality cohort N (outcome known)",
             "Chikungunya-attributed deaths",
             "Crude mortality risk (%)",
             "Age range (years)",
             "Notification years"),
  value = c(format(n_eligible), format(nrow(death_cohort)),
            format(sum(death_cohort$death_only)),
            sprintf("%.4f", 100 * mean(death_cohort$death_only)),
            paste(range(death_cohort$age_years), collapse = "-"),
            paste(range(as.integer(as.character(death_cohort$year))), collapse = "-"))
)
print(cohort_check)
readr::write_csv(cohort_check, file.path(out_dir, "mortality_cohort_check.csv"))

## Gate 3.1 corrected the outcome so EVOLUCAO 4/9/blank are unknown (NA),
## not "survived"; script 12's death cohort is therefore 983,947 records
## (eligible population 1,074,796). Stop if the cohort has drifted.
stopifnot(n_eligible == 1074796, nrow(death_cohort) == 983947,
          sum(death_cohort$death_only) == 1050)

## ============================================================
## 2. Task 1 - empirical support by age group x profile
## ============================================================

exact_ci <- function(x, n) {
  ci <- binom::binom.confint(x, n, methods = "exact")
  tibble::tibble(crude_lower_pct = 100 * ci$lower,
                 crude_upper_pct = 100 * ci$upper)
}

make_support <- function(scheme) {
  cells <- death_cohort |>
    dplyr::mutate(age_group = cut_age(age_years, scheme)) |>
    dplyr::count(age_group, core_profile, death_only) |>
    tidyr::complete(age_group, core_profile, death_only,
                    fill = list(n = 0L)) |>
    dplyr::group_by(age_group, core_profile) |>
    dplyr::summarise(n_cases = sum(n), n_deaths = sum(n[death_only]),
                     .groups = "drop") |>
    dplyr::mutate(
      age_scheme = scheme,
      crude_risk_pct = ifelse(n_cases > 0, 100 * n_deaths / n_cases, NA_real_),
      pct_of_cohort = 100 * n_cases / nrow(death_cohort),
      pct_of_deaths = 100 * n_deaths / sum(death_cohort$death_only)
    )
  ## Clopper-Pearson exact intervals: appropriate for the many cells with
  ## 0-9 events, where Wald intervals fail.
  cis <- purrr::map2_dfr(cells$n_deaths, cells$n_cases, function(x, n) {
    if (n == 0) tibble::tibble(crude_lower_pct = NA_real_, crude_upper_pct = NA_real_)
    else exact_ci(x, n)
  })
  dplyr::bind_cols(cells, cis) |>
    dplyr::group_by(age_group) |>
    dplyr::mutate(none_deaths = n_deaths[core_profile == "None"]) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      support_category = support_category(n_deaths),
      supported_ge10_deaths = n_deaths >= death_threshold,
      age_group = as.character(age_group),
      core_profile = as.character(core_profile)
    ) |>
    dplyr::select(age_scheme, age_group, core_profile, n_cases, n_deaths,
                  crude_risk_pct, crude_lower_pct, crude_upper_pct,
                  pct_of_cohort, pct_of_deaths, none_deaths,
                  support_category, supported_ge10_deaths)
}

support_all <- purrr::map_dfr(names(age_schemes), make_support)

## Per-profile age distribution of cases and of deaths.
q <- function(x, p) if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE)) else NA_real_

profile_age_distribution <- death_cohort |>
  dplyr::group_by(core_profile) |>
  dplyr::summarise(
    n_cases = dplyr::n(),
    n_deaths = sum(death_only),
    case_age_min = min(age_years), case_age_p05 = q(age_years, 0.05),
    case_age_q1 = q(age_years, 0.25), case_age_median = q(age_years, 0.5),
    case_age_q3 = q(age_years, 0.75), case_age_p95 = q(age_years, 0.95),
    case_age_max = max(age_years),
    death_age_min = if (any(death_only)) min(age_years[death_only]) else NA_real_,
    death_age_q1 = q(age_years[death_only], 0.25),
    death_age_median = q(age_years[death_only], 0.5),
    death_age_q3 = q(age_years[death_only], 0.75),
    death_age_max = if (any(death_only)) max(age_years[death_only]) else NA_real_,
    deaths_age_lt15 = sum(death_only & age_years < 15),
    deaths_age_lt40 = sum(death_only & age_years < 40),
    deaths_age_ge75 = sum(death_only & age_years >= 75),
    .groups = "drop"
  )
readr::write_csv(profile_age_distribution,
                 file.path(out_dir, "profile_age_and_death_age_distribution.csv"))

## 5-year bins: where within each profile the deaths actually sit, and how
## each candidate boundary splits them.
support_5yr <- death_cohort |>
  dplyr::mutate(age_bin = cut(age_years, breaks = c(seq(0, 95, 5), 101),
                              right = FALSE, include.lowest = TRUE,
                              labels = c(paste0(seq(0, 90, 5), "-", seq(4, 94, 5)), "95+"))) |>
  dplyr::group_by(core_profile, age_bin, .drop = FALSE) |>
  dplyr::summarise(n_cases = dplyr::n(), n_deaths = sum(death_only), .groups = "drop")
readr::write_csv(support_5yr, file.path(out_dir, "age_profile_support_5yr_bins.csv"))

## Number of adequately supported (>=10 deaths) non-None cells per scheme.
scheme_support_summary <- support_all |>
  dplyr::filter(core_profile != "None") |>
  dplyr::group_by(age_scheme) |>
  dplyr::summarise(
    n_cells = dplyr::n(),
    cells_adequate = sum(support_category == "Adequate"),
    cells_sparse = sum(support_category == "Sparse"),
    cells_no_deaths = sum(support_category == "No deaths"),
    profiles_with_any_adequate_cell = dplyr::n_distinct(core_profile[support_category == "Adequate"]),
    .groups = "drop"
  )
print(scheme_support_summary)
readr::write_csv(scheme_support_summary, file.path(out_dir, "age_scheme_support_summary.csv"))

## Data-quality probe: multimorbid profiles recorded in children.
child_multimorbid <- death_cohort |>
  dplyr::filter(age_years < 15, core_profile %in% c("DM+CKD", "HTN+CKD", "DM+HTN+CKD", "DM+HTN")) |>
  dplyr::count(core_profile, year, uf_residence, name = "n") |>
  dplyr::group_by(core_profile) |>
  dplyr::mutate(total = sum(n), share = n / total) |>
  dplyr::arrange(core_profile, dplyr::desc(n)) |>
  dplyr::slice_head(n = 3) |>
  dplyr::ungroup()
readr::write_csv(child_multimorbid, file.path(out_dir, "child_multimorbid_top_state_years.csv"))

log_msg("support tables written")

## ============================================================
## 3. Task 2 - model fits
## ============================================================

fit_bam <- function(formula, data, sp = NULL) {
  warns <- character()
  t0 <- Sys.time()
  fit <- withCallingHandlers(
    mgcv::bam(formula, data = data, family = binomial(link = "logit"),
              method = "fREML", discrete = TRUE, nthreads = n_threads, sp = sp),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  fit$audit_warnings <- warns
  fit$audit_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  fit
}

base_terms <- "sex + year + s(uf_residence, bs = 're')"

model_specs <- list(
  no_interaction = list(
    label = "No interaction: profile + s(age)",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, k = 10, bs = 'ts')")),
  ## k.check flags the k = 10 shared age smooth as possibly too stiff, so a
  ## richer basis separates "better age shape" from "age x profile" in the
  ## AIC comparison.
  no_interaction_k20 = list(
    label = "No interaction: profile + s(age, k = 20)",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, k = 20, bs = 'ts')")),
  fs_original = list(
    label = "Original Figure 5: s(age, profile, bs='fs', k=10)",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, core_profile, bs = 'fs', k = 10)")),
  ## The original formula has no global age smooth, so the fs penalty shrinks
  ## each profile's curve toward a flat (constant log-odds) curve rather than
  ## toward the common age shape. Pedersen et al. (2019) "model GS": shared
  ## s(age) plus fs deviations with first-order penalty (m = 1), so sparse
  ## profiles shrink toward the common age curve, i.e. toward a constant OR.
  fs_global = list(
    label = "Global s(age) + fs deviations (m = 1), Pedersen model GS",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, k = 10, bs = 'ts')",
                    "+ s(age_years, core_profile, bs = 'fs', k = 10, m = 1)")),
  fs_k5 = list(
    label = "fs, k = 5",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, core_profile, bs = 'fs', k = 5)")),
  fs_k15 = list(
    label = "fs, k = 15",
    formula = paste("death_only ~ core_profile +", base_terms,
                    "+ s(age_years, core_profile, bs = 'fs', k = 15)")),
  linear_interaction = list(
    label = "Linear (log-odds) age x profile + s(age)",
    formula = paste("death_only ~ core_profile + lin_DM + lin_HTN + lin_CKD + lin_DM_HTN +",
                    "lin_DM_CKD + lin_HTN_CKD + lin_DM_HTN_CKD +", base_terms,
                    "+ s(age_years, k = 10, bs = 'ts')")),
  band_re = list(
    label = "Age-group x profile random effect + s(age)",
    formula = paste("death_only ~ core_profile + age_group +", base_terms,
                    "+ s(age_years, k = 10, bs = 'ts') + s(profile_age, bs = 're')"))
)

fits <- list()
for (nm in names(model_specs)) {
  log_msg("fitting %s", nm)
  fits[[nm]] <- fit_bam(stats::as.formula(model_specs[[nm]]$formula), death_cohort)
  log_msg("  done in %.0f s", fits[[nm]]$audit_seconds)
}

## Fixed-smoothing-parameter variants of the original model: the fs term's
## estimated smoothing parameters multiplied by 1/4 (wigglier) and 4
## (smoother); the state random-effect variance is left at its estimate.
sp_hat <- fits$fs_original$sp
fs_sp_index <- grepl("core_profile", names(sp_hat), fixed = TRUE)
for (mult in c(0.25, 4)) {
  nm <- if (mult < 1) "fs_sp_x0.25" else "fs_sp_x4"
  sp_new <- sp_hat
  sp_new[fs_sp_index] <- sp_new[fs_sp_index] * mult
  model_specs[[nm]] <- list(
    label = sprintf("fs, k = 10, fs smoothing parameters x %s (fixed)", mult),
    formula = model_specs$fs_original$formula)
  log_msg("fitting %s", nm)
  fits[[nm]] <- fit_bam(stats::as.formula(model_specs$fs_original$formula),
                        death_cohort, sp = sp_new)
}

## Original model refitted without ages 0-14 (no non-None deaths there).
model_specs$fs_age15plus <- list(
  label = "Original fs model refitted on ages 15-100 only",
  formula = model_specs$fs_original$formula)
log_msg("fitting fs_age15plus")
cohort_15plus <- dplyr::filter(death_cohort, age_years >= 15) |>
  dplyr::mutate(year = droplevels(year), uf_residence = droplevels(uf_residence))
fits$fs_age15plus <- fit_bam(stats::as.formula(model_specs$fs_original$formula), cohort_15plus)

log_msg("all fits complete")

## ------------------------------------------------------------
## 3a. Reproduction check: refitted fs model vs cached Figure 5 curve
## ------------------------------------------------------------
load("01_Data/core_profile_rr_age_curve.RData")
cached_curve <- core_profile_rr_age_curve |>
  dplyr::filter(outcome == "Death from chikungunya")

strata_all <- death_cohort |>
  dplyr::count(sex, year, uf_residence, name = "n") |>
  dplyr::mutate(weight = n / sum(n))

curve_point <- function(fit, ages = 1:95) {
  purrr::map_dfr(core_profile_levels, function(p) {
    g <- tidyr::expand_grid(age_years = ages, strata_all) |>
      dplyr::mutate(core_profile = factor(p, levels = core_profile_levels)) |>
      add_model_vars()
    pr <- stats::plogis(as.numeric(stats::predict(fit, newdata = g, type = "lpmatrix") %*% coef(fit)))
    tibble::tibble(core_profile = p, age_years = ages,
                   standardised_risk = colSums(matrix(pr, nrow = nrow(strata_all)) * strata_all$weight))
  }) |>
    dplyr::group_by(age_years) |>
    dplyr::mutate(rr = standardised_risk / standardised_risk[core_profile == "None"]) |>
    dplyr::ungroup()
}

refit_curve <- curve_point(fits$fs_original)
repro <- refit_curve |>
  dplyr::inner_join(
    cached_curve |> dplyr::transmute(core_profile = as.character(core_profile), age_years,
                                     cached_risk = standardised_risk, cached_rr = rr),
    by = c("core_profile", "age_years")) |>
  dplyr::summarise(
    n_points = dplyr::n(),
    max_abs_rel_diff_risk = max(abs(standardised_risk / cached_risk - 1)),
    max_abs_rel_diff_rr = max(abs(rr / cached_rr - 1))
  )
print(repro)
readr::write_csv(repro, file.path(out_dir, "original_model_reproduction_check.csv"))

## Point curves from the global-smooth (GS) alternative, for comparison with
## the published fs curves in the supplementary overlay.
readr::write_csv(curve_point(fits$fs_global), file.path(out_dir, "fs_global_curve_points.csv"))
if (repro$max_abs_rel_diff_rr > 0.01) {
  warning("Refitted fs model does not reproduce the cached Figure 5 curve within 1%")
}

## ------------------------------------------------------------
## 3b. Original curve vs local empirical support (extrapolation audit)
## ------------------------------------------------------------
## For every age on the published curve, count the profile's cases and
## deaths within +/- 5 years. A curve value with zero local deaths is driven
## by the shared smoothing parameter and the None/neighbouring-age data, not
## by events in that profile at that age.
by_single_age <- death_cohort |>
  dplyr::count(core_profile, age_years, wt = NULL, name = "cases") |>
  dplyr::left_join(
    death_cohort |> dplyr::filter(death_only) |>
      dplyr::count(core_profile, age_years, name = "deaths"),
    by = c("core_profile", "age_years")) |>
  dplyr::mutate(deaths = dplyr::coalesce(deaths, 0L),
                core_profile = as.character(core_profile))

local_support <- tidyr::expand_grid(core_profile = core_profile_levels, age_years = 1:95) |>
  dplyr::left_join(by_single_age, by = "core_profile", relationship = "many-to-many",
                   suffix = c("", "_obs")) |>
  dplyr::filter(abs(age_years_obs - age_years) <= 5) |>
  dplyr::group_by(core_profile, age_years) |>
  dplyr::summarise(local_cases = sum(cases), local_deaths = sum(deaths), .groups = "drop") |>
  tidyr::complete(core_profile = core_profile_levels, age_years = 1:95,
                  fill = list(local_cases = 0L, local_deaths = 0L))

curve_audit <- cached_curve |>
  dplyr::mutate(core_profile = as.character(core_profile)) |>
  dplyr::select(core_profile, age_years, standardised_risk, rr, rr_lower, rr_upper) |>
  dplyr::left_join(local_support, by = c("core_profile", "age_years")) |>
  dplyr::left_join(profile_age_distribution |>
                     dplyr::transmute(core_profile = as.character(core_profile),
                                      death_age_min, death_age_max),
                   by = "core_profile") |>
  dplyr::mutate(
    ci_width_ratio = rr_upper / rr_lower,
    local_support = dplyr::case_when(
      is.na(death_age_min) | age_years < death_age_min | age_years > death_age_max ~
        "Outside observed death-age range",
      local_deaths == 0 ~ "No deaths within +/-5 y",
      local_deaths < death_threshold ~ "1-9 deaths within +/-5 y",
      TRUE ~ ">=10 deaths within +/-5 y"
    )
  )
readr::write_csv(curve_audit, file.path(out_dir, "original_curve_local_support_audit.csv"))

curve_audit_summary <- curve_audit |>
  dplyr::filter(core_profile != "None") |>
  dplyr::group_by(core_profile, local_support) |>
  dplyr::summarise(n_ages = dplyr::n(), age_min = min(age_years), age_max = max(age_years),
                   rr_min = min(rr), rr_max = max(rr),
                   median_ci_width_ratio = stats::median(ci_width_ratio), .groups = "drop")
readr::write_csv(curve_audit_summary, file.path(out_dir, "original_curve_local_support_summary.csv"))

## ------------------------------------------------------------
## 3c. Fit / convergence / interaction diagnostics
## ------------------------------------------------------------
smooth_edf <- function(fit, pattern) {
  st <- summary(fit)$s.table
  i <- grepl(pattern, rownames(st), fixed = TRUE)
  if (!any(i)) return(NA_real_)
  sum(st[i, "edf"])
}

## Penalisation-aware Wald test that each profile's log-OR vs None is
## constant across age, using the Bayesian posterior covariance Vp (the
## covariance mgcv uses for its own smooth-term tests). Contrasts are
## [lp(p, a) - lp(None, a)] - [lp(p, 60) - lp(None, 60)] for a in
## 20, 30, ..., 90 (ages where at least the larger profiles have deaths).
## sex/year/state cancel because none of them interacts with profile.
age_shape_wald <- function(fit, ages = seq(20, 90, 10), ref_age = 60,
                           profiles = core_profile_levels[-1]) {
  grid <- tidyr::expand_grid(core_profile = factor(core_profile_levels, levels = core_profile_levels),
                             age_years = sort(unique(c(ages, ref_age)))) |>
    dplyr::mutate(sex = factor("female", levels = levels(death_cohort$sex)),
                  year = factor(levels(death_cohort$year)[1], levels = levels(death_cohort$year)),
                  uf_residence = factor(levels(death_cohort$uf_residence)[1],
                                        levels = levels(death_cohort$uf_residence))) |>
    add_model_vars()
  X <- stats::predict(fit, newdata = grid, type = "lpmatrix")
  row_of <- function(p, a) which(grid$core_profile == p & grid$age_years == a)
  test_ages <- setdiff(ages, ref_age)

  build_L <- function(ps) {
    do.call(rbind, lapply(ps, function(p) {
      t(sapply(test_ages, function(a) {
        (X[row_of(p, a), ] - X[row_of("None", a), ]) -
          (X[row_of(p, ref_age), ] - X[row_of("None", ref_age), ])
      }))
    }))
  }
  wald <- function(L) {
    d <- as.numeric(L %*% coef(fit))
    V <- L %*% fit$Vp %*% t(L)
    e <- eigen((V + t(V)) / 2, symmetric = TRUE)
    keep <- e$values > max(e$values) * 1e-7
    if (!any(keep)) return(c(stat = 0, df = 0, p = NA))
    z <- crossprod(e$vectors[, keep, drop = FALSE], d)
    stat <- sum(z^2 / e$values[keep])
    c(stat = stat, df = sum(keep), p = stats::pchisq(stat, sum(keep), lower.tail = FALSE))
  }
  res <- purrr::map_dfr(c(profiles, "All 7 profiles jointly"), function(p) {
    ps <- if (p == "All 7 profiles jointly") profiles else p
    w <- wald(build_L(ps))
    tibble::tibble(core_profile = p, wald_chisq = w[["stat"]], wald_df = w[["df"]],
                   wald_p = w[["p"]])
  })
  res
}

diagnose_fit <- function(nm, fit) {
  s <- summary(fit)
  set.seed(46)  # k.check subsamples and permutes residuals at random
  kc <- tryCatch(mgcv::k.check(fit, subsample = 5000, n.rep = 200), error = function(e) NULL)
  age_rows <- if (!is.null(kc)) grepl("age_years", rownames(kc)) else logical()
  vp_eig <- eigen(fit$Vp, symmetric = TRUE, only.values = TRUE)$values
  tibble::tibble(
    model = nm,
    model_label = model_specs[[nm]]$label,
    formula = model_specs[[nm]]$formula,
    n = nrow(fit$model),
    deaths = sum(fit$y),
    smoothing_parameters = if (nm %in% c("fs_sp_x0.25", "fs_sp_x4")) "fixed" else "estimated (fREML)",
    aic = stats::AIC(fit),
    total_edf = sum(fit$edf),
    edf_age_terms = smooth_edf(fit, "age_years"),
    edf_profile_age_re = smooth_edf(fit, "profile_age"),
    edf_state_re = smooth_edf(fit, "uf_residence"),
    deviance_explained_pct = 100 * s$dev.expl,
    fREML_score = as.numeric(fit$gcv.ubre),
    converged = if (is.null(fit$converged)) NA else fit$converged,
    pirls_iterations = if (!is.null(fit$iter)) fit$iter else NA_integer_,
    outer_convergence = if (!is.null(fit$outer.info$conv)) as.character(fit$outer.info$conv) else NA_character_,
    n_warnings = length(fit$audit_warnings),
    warnings = paste(unique(fit$audit_warnings), collapse = " | "),
    vp_min_eigen = min(vp_eig),
    vp_condition_number = max(vp_eig) / max(min(vp_eig), .Machine$double.eps),
    max_coef_se = max(sqrt(diag(fit$Vp))),
    k_index_min_age = if (any(age_rows)) min(kc[age_rows, "k-index"]) else NA_real_,
    k_check_p_min_age = if (any(age_rows)) min(kc[age_rows, "p-value"]) else NA_real_,
    fit_seconds = fit$audit_seconds
  )
}

diagnostics <- purrr::imap_dfr(fits, ~ diagnose_fit(.y, .x))

## Comparisons against the no-interaction model (same data only).
aic_ref <- diagnostics$aic[diagnostics$model == "no_interaction"]
diagnostics <- diagnostics |>
  dplyr::mutate(
    delta_aic_vs_no_interaction = ifelse(model == "fs_age15plus", NA_real_, aic - aic_ref),
    delta_edf_vs_no_interaction = ifelse(model == "fs_age15plus", NA_real_,
                                         total_edf - total_edf[model == "no_interaction"])
  )

## Approximate generalised LRT (mgcv anova.gam) - edf-based, approximate
## for penalised fits; reported alongside, not instead of, the Wald tests.
anova_vs_noint <- purrr::map_dfr(
  c("no_interaction_k20", "fs_original", "fs_global", "fs_k5", "fs_k15", "linear_interaction", "band_re"),
  function(nm) {
    a <- tryCatch(mgcv::anova.gam(fits$no_interaction, fits[[nm]], test = "Chisq"),
                  error = function(e) NULL)
    if (is.null(a)) return(tibble::tibble(model = nm, lrt_deviance = NA, lrt_df = NA, lrt_p = NA))
    tibble::tibble(model = nm, lrt_deviance = a$Deviance[2], lrt_df = a$Df[2],
                   lrt_p = a$`Pr(>Chi)`[2])
  })
diagnostics <- dplyr::left_join(diagnostics, anova_vs_noint, by = "model")

## Penalisation-aware Wald tests of age-varying log-OR.
wald_tests <- purrr::map_dfr(
  c("fs_original", "fs_global", "fs_k5", "fs_k15", "fs_sp_x0.25", "fs_sp_x4", "fs_age15plus"),
  function(nm) age_shape_wald(fits[[nm]]) |>
    dplyr::mutate(model = nm, age_range = "20-90", .before = 1)
)
## Same test over the whole curve (ages 1-95), i.e. including childhood ages
## where no non-None profile has more than one recorded death.
wald_tests_full <- purrr::map_dfr(
  c("fs_original", "fs_global", "fs_k5", "fs_k15", "fs_sp_x0.25", "fs_sp_x4"),
  function(nm) age_shape_wald(fits[[nm]], ages = c(1, 5, 10, 15, seq(20, 90, 10), 95)) |>
    dplyr::mutate(model = nm, age_range = "1-95", .before = 1)
)

## Linear interaction: ordinary Wald test on the 7 profile:age10 coefficients.
lin_idx <- grep("^lin_", names(coef(fits$linear_interaction)))
b_lin <- coef(fits$linear_interaction)[lin_idx]
V_lin <- fits$linear_interaction$Vp[lin_idx, lin_idx]
lin_stat <- as.numeric(t(b_lin) %*% solve(V_lin) %*% b_lin)
lin_coefs <- tibble::tibble(
  term = names(b_lin),
  log_or_change_per_decade = b_lin,
  se = sqrt(diag(V_lin)),
  or_ratio_per_decade = exp(b_lin),
  lower = exp(b_lin - 1.96 * sqrt(diag(V_lin))),
  upper = exp(b_lin + 1.96 * sqrt(diag(V_lin)))
)
wald_tests <- dplyr::bind_rows(
  wald_tests,
  wald_tests_full,
  tibble::tibble(model = "linear_interaction", age_range = "linear slope",
                 core_profile = "All 7 profiles jointly",
                 wald_chisq = lin_stat, wald_df = length(lin_idx),
                 wald_p = stats::pchisq(lin_stat, length(lin_idx), lower.tail = FALSE))
)

## Band random effect: estimated SD of profile x age-group deviations.
vc <- tryCatch(mgcv::gam.vcomp(fits$band_re), error = function(e) NULL)
band_re_sd <- if (!is.null(vc)) {
  vcm <- if (is.list(vc)) vc$vc else vc
  r <- grep("profile_age", rownames(vcm))
  tibble::tibble(term = "s(profile_age) SD (log-odds)",
                 estimate = vcm[r, 1],
                 lower = if (ncol(vcm) >= 3) vcm[r, 2] else NA_real_,
                 upper = if (ncol(vcm) >= 3) vcm[r, 3] else NA_real_)
} else tibble::tibble(term = "s(profile_age) SD (log-odds)", estimate = NA, lower = NA, upper = NA)

diag_long <- diagnostics |>
  dplyr::left_join(
    wald_tests |> dplyr::filter(core_profile == "All 7 profiles jointly", age_range != "1-95") |>
      dplyr::select(model, joint_age_shape_wald_chisq = wald_chisq,
                    joint_age_shape_wald_df = wald_df, joint_age_shape_wald_p = wald_p),
    by = "model") |>
  dplyr::mutate(
    profile_age_re_sd = ifelse(model == "band_re", band_re_sd$estimate, NA_real_),
    profile_age_re_sd_lower = ifelse(model == "band_re", band_re_sd$lower, NA_real_),
    profile_age_re_sd_upper = ifelse(model == "band_re", band_re_sd$upper, NA_real_)
  )

diag_long <- diag_long |>
  dplyr::left_join(
    wald_tests_full |> dplyr::filter(core_profile == "All 7 profiles jointly") |>
      dplyr::select(model, joint_age_shape_wald_chisq_1_95 = wald_chisq,
                    joint_age_shape_wald_df_1_95 = wald_df, joint_age_shape_wald_p_1_95 = wald_p),
    by = "model")

readr::write_csv(diag_long, file.path(out_dir, "age_interaction_diagnostics.csv"))
readr::write_csv(wald_tests, file.path(out_dir, "age_interaction_wald_tests_by_profile.csv"))
readr::write_csv(lin_coefs, file.path(out_dir, "linear_age_interaction_coefficients.csv"))
print(as.data.frame(diag_long[, c("model", "aic", "total_edf", "delta_aic_vs_no_interaction",
                                  "lrt_p", "joint_age_shape_wald_p", "converged", "n_warnings")]))
log_msg("diagnostics written")

## Observed vs expected deaths per age-group x profile cell (in-sample
## calibration of the fitted probabilities): shows whether the interaction
## model is merely tracking sparse counts, and where the no-interaction
## model misfits.
oe <- death_cohort |>
  dplyr::mutate(
    expected_fs = fitted(fits$fs_original),
    expected_noint = fitted(fits$no_interaction)
  ) |>
  dplyr::group_by(age_group, core_profile) |>
  dplyr::summarise(observed_deaths = sum(death_only),
                   expected_deaths_fs = sum(expected_fs),
                   expected_deaths_no_interaction = sum(expected_noint),
                   .groups = "drop") |>
  dplyr::mutate(age_group = as.character(age_group), core_profile = as.character(core_profile))

support_primary <- support_all |>
  dplyr::filter(age_scheme == "primary") |>
  dplyr::left_join(oe, by = c("age_group", "core_profile")) |>
  dplyr::select(-observed_deaths)

readr::write_csv(
  dplyr::bind_rows(support_primary,
                   support_all |> dplyr::filter(age_scheme != "primary")),
  file.path(out_dir, "age_profile_support.csv"), na = "")

## ============================================================
## 4. Task 3 - within-age-group standardised risk, RR and RD
## ============================================================
## Target population for age group g: the observed joint distribution of
## (exact age, sex, year, state) among ALL cohort members in g. The same
## target is applied to every profile, so profile contrasts within g differ
## only in profile. Every risk, RR and RD is computed within the same
## coefficient draw (MASS::mvrnorm on vcov(unconditional = TRUE), as in
## script 12), preserving covariance between profiles and age groups.

weighted_risk_draws <- function(X, beta_draws, w, block = 250) {
  out <- numeric(nrow(beta_draws))
  for (i in seq.int(1, nrow(beta_draws), by = block)) {
    idx <- i:min(i + block - 1, nrow(beta_draws))
    P <- stats::plogis(X %*% t(beta_draws[idx, , drop = FALSE]))
    out[idx] <- colSums(P * w) / sum(w)
  }
  out
}

standardise_by_age_group <- function(fit, data, scheme, B, seed, groups = NULL) {
  set.seed(seed)
  beta_hat <- coef(fit)
  beta_draws <- MASS::mvrnorm(B, mu = beta_hat, Sigma = stats::vcov(fit, unconditional = TRUE))
  d <- dplyr::mutate(data, grp = cut_age(age_years, scheme))
  grp_levels <- if (is.null(groups)) age_schemes[[scheme]]$labels else groups

  draws <- list()
  point <- list()
  for (g in grp_levels) {
    combo <- d |>
      dplyr::filter(grp == g) |>
      dplyr::count(age_years, sex, year, uf_residence, name = "n")
    rp <- setNames(numeric(length(core_profile_levels)), core_profile_levels)
    rd <- matrix(NA_real_, B, length(core_profile_levels),
                 dimnames = list(NULL, core_profile_levels))
    for (p in core_profile_levels) {
      nd <- combo |>
        dplyr::mutate(core_profile = factor(p, levels = core_profile_levels)) |>
        add_model_vars()
      X <- stats::predict(fit, newdata = nd, type = "lpmatrix")
      rp[p] <- sum(stats::plogis(as.numeric(X %*% beta_hat)) * combo$n) / sum(combo$n)
      rd[, p] <- weighted_risk_draws(X, beta_draws, combo$n)
    }
    draws[[g]] <- rd
    point[[g]] <- rp
  }
  list(draws = draws, point = point, groups = grp_levels, scheme = scheme)
}

summarise_std <- function(std) {
  qs <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE, na.rm = TRUE))
  purrr::map_dfr(std$groups, function(g) {
    rd <- std$draws[[g]]
    rp <- std$point[[g]]
    purrr::map_dfr(core_profile_levels, function(p) {
      rr_d <- rd[, p] / rd[, "None"]
      dd_d <- rd[, p] - rd[, "None"]
      tibble::tibble(
        age_group = g, core_profile = p,
        std_risk_pct = 100 * rp[[p]],
        std_risk_lower_pct = 100 * qs(rd[, p], 0.025),
        std_risk_upper_pct = 100 * qs(rd[, p], 0.975),
        rr = rp[[p]] / rp[["None"]],
        rr_lower = qs(rr_d, 0.025), rr_upper = qs(rr_d, 0.975),
        rd_pp = 100 * (rp[[p]] - rp[["None"]]),
        rd_lower_pp = 100 * qs(dd_d, 0.025), rd_upper_pp = 100 * qs(dd_d, 0.975)
      )
    })
  })
}

## Ratio of RRs between age groups (effect-modification contrast), within draw.
rr_ratio_contrasts <- function(std, ref_group) {
  qs <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE))
  rr_draw <- function(g, p) std$draws[[g]][, p] / std$draws[[g]][, "None"]
  rr_pt <- function(g, p) std$point[[g]][[p]] / std$point[[g]][["None"]]
  tidyr::expand_grid(age_group = setdiff(std$groups, ref_group),
                     core_profile = core_profile_levels[-1]) |>
    purrr::pmap_dfr(function(age_group, core_profile) {
      r <- rr_draw(age_group, core_profile) / rr_draw(ref_group, core_profile)
      tibble::tibble(age_group = age_group, reference_age_group = ref_group,
                     core_profile = core_profile,
                     rr_ratio = rr_pt(age_group, core_profile) / rr_pt(ref_group, core_profile),
                     rr_ratio_lower = qs(r, 0.025), rr_ratio_upper = qs(r, 0.975))
    })
}

attach_support <- function(est, scheme) {
  sup <- support_all |>
    dplyr::filter(age_scheme == scheme) |>
    dplyr::select(age_group, core_profile, n_cases, n_deaths, crude_risk_pct,
                  none_deaths, support_category)
  est |>
    dplyr::left_join(sup, by = c("age_group", "core_profile")) |>
    dplyr::mutate(
      estimate_status = dplyr::case_when(
        core_profile == "None" & support_category == "Adequate" ~ "Reference",
        core_profile == "None" ~ "Reference (sparse)",
        support_category == "Adequate" & none_deaths >= death_threshold ~ "Reported",
        support_category == "Adequate" ~ "Flagged: sparse reference",
        support_category == "Sparse" ~ "Flagged: sparse (1-9 deaths)",
        TRUE ~ "Suppressed: no observed deaths"
      )
    )
}

log_msg("primary standardisation (B = %d)", B_primary)
std_primary <- standardise_by_age_group(fits$fs_original, death_cohort, "primary",
                                        B = B_primary, seed = 202)

primary_est <- summarise_std(std_primary) |>
  attach_support("primary") |>
  dplyr::mutate(model = "fs_original", age_scheme = "primary", .before = 1)

## Same point estimates are also produced for the no-interaction model so
## the CSV carries the direct comparison asked for in Task 5.
readr::write_csv(primary_est, file.path(out_dir, "age_profile_standardised_estimates.csv"), na = "")

contrasts_primary <- rr_ratio_contrasts(std_primary, ref_group = "60-74") |>
  dplyr::mutate(age_scheme = "primary", .before = 1) |>
  dplyr::mutate(model = "fs_original", .before = 1)

## ============================================================
## 5. Task 5 - robustness
## ============================================================
sens_runs <- list(
  list(model = "no_interaction",     scheme = "primary",  groups = NULL),
  list(model = "no_interaction_k20", scheme = "primary",  groups = NULL),
  list(model = "fs_global",          scheme = "primary",  groups = NULL),
  list(model = "fs_k5",              scheme = "primary",  groups = NULL),
  list(model = "fs_k15",             scheme = "primary",  groups = NULL),
  list(model = "fs_sp_x0.25",        scheme = "primary",  groups = NULL),
  list(model = "fs_sp_x4",           scheme = "primary",  groups = NULL),
  list(model = "linear_interaction", scheme = "primary",  groups = NULL),
  list(model = "band_re",            scheme = "primary",  groups = NULL),
  list(model = "fs_age15plus",       scheme = "primary",  groups = c("15-39", "40-59", "60-74", "75+")),
  list(model = "fs_original",        scheme = "script12_bands", groups = NULL),
  list(model = "fs_original",        scheme = "merged_4", groups = NULL),
  list(model = "fs_original",        scheme = "merged_3", groups = NULL),
  list(model = "fs_original",        scheme = "merged_2", groups = NULL),
  list(model = "no_interaction",     scheme = "merged_3", groups = NULL)
)

sens_est <- list()
sens_contrasts <- list()
for (i in seq_along(sens_runs)) {
  r <- sens_runs[[i]]
  log_msg("sensitivity %s / %s", r$model, r$scheme)
  dat <- if (r$model == "fs_age15plus") cohort_15plus else death_cohort
  std <- standardise_by_age_group(fits[[r$model]], dat, r$scheme, B = B_sens,
                                  seed = 202 + i, groups = r$groups)
  sens_est[[i]] <- summarise_std(std) |>
    attach_support(r$scheme) |>
    dplyr::mutate(model = r$model, age_scheme = r$scheme, .before = 1)
  if ("60-74" %in% std$groups) {
    sens_contrasts[[i]] <- rr_ratio_contrasts(std, "60-74") |>
      dplyr::mutate(age_scheme = r$scheme, .before = 1) |>
      dplyr::mutate(model = r$model, .before = 1)
  }
}

robustness_est <- dplyr::bind_rows(primary_est, sens_est)
readr::write_csv(robustness_est, file.path(out_dir, "age_profile_robustness_estimates.csv"), na = "")
readr::write_csv(dplyr::bind_rows(contrasts_primary, sens_contrasts),
                 file.path(out_dir, "age_profile_rr_ratio_contrasts.csv"), na = "")

## Compact comparison: each alternative vs the primary fs estimate for the
## same (primary-scheme) cell, Reported cells only.
rob_compare <- robustness_est |>
  dplyr::filter(age_scheme == "primary", model != "fs_original") |>
  dplyr::select(model, age_group, core_profile, rr_alt = rr, rr_alt_lower = rr_lower,
                rr_alt_upper = rr_upper) |>
  dplyr::inner_join(primary_est |>
                      dplyr::select(age_group, core_profile, rr, rr_lower, rr_upper,
                                    estimate_status, n_deaths),
                    by = c("age_group", "core_profile")) |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(
    rr_ratio_alt_vs_primary = rr_alt / rr,
    primary_excludes_1 = rr_lower > 1,
    alt_excludes_1 = rr_alt_lower > 1,
    same_significance = primary_excludes_1 == alt_excludes_1
  )
readr::write_csv(rob_compare, file.path(out_dir, "robustness_vs_primary_cellwise.csv"), na = "")

rob_summary <- rob_compare |>
  dplyr::group_by(model, reported_cell = estimate_status == "Reported") |>
  dplyr::summarise(
    n_cells = dplyr::n(),
    median_abs_log_rr_diff = stats::median(abs(log(rr_ratio_alt_vs_primary))),
    max_abs_log_rr_diff = max(abs(log(rr_ratio_alt_vs_primary))),
    min_rr_ratio = min(rr_ratio_alt_vs_primary),
    max_rr_ratio = max(rr_ratio_alt_vs_primary),
    n_significance_changes = sum(!same_significance),
    .groups = "drop"
  )
print(as.data.frame(rob_summary))
readr::write_csv(rob_summary, file.path(out_dir, "robustness_summary.csv"))

## Record of every exclusion / suppression applied anywhere in this audit.
suppression_log <- primary_est |>
  dplyr::filter(!estimate_status %in% c("Reported", "Reference")) |>
  dplyr::select(age_group, core_profile, n_cases, n_deaths, none_deaths, estimate_status) |>
  dplyr::mutate(
    figure_5A = dplyr::case_when(
      grepl("^Suppressed", estimate_status) ~ "Not plotted; labelled '0 deaths'",
      grepl("^Flagged", estimate_status) ~ "Plotted as open grey marker",
      TRUE ~ "Reference"),
    figure_5B = ifelse(grepl("^Reference", estimate_status), "Plotted", "Not plotted")
  )
readr::write_csv(suppression_log, file.path(out_dir, "suppression_and_exclusion_log.csv"))

saveRDS(list(primary_draws = std_primary, sessionInfo = utils::sessionInfo()),
        file.path(out_dir, "primary_standardisation_draws.rds"))

log_msg("done")
