# ---------------------------------------------------------------------------
# 52_hospital_age_validation.R
#
# Age-specific hospitalisation analysis for the final combined Figure 3:
# empirical-support audit, age-model validation, within-age-group
# standardised risks/RRs/RDs, and an age-specific missing-outcome
# sensitivity analysis. The hospitalisation counterpart of the mortality
# audit in 46_figure5_age_profile_validation.R, with the same age groups,
# the same g-computation and the same coefficient-draw uncertainty.
#
# Models (hospitalisation cohort, N = 776,072; 25,363 hospitalisations):
#   H0  no age interaction: profile + s(age, k=10, ts)
#   H1  original age-interaction model (script 12 section 12, the model
#       behind the hospitalisation row of fig3_profile_age_specific_relative_risk):
#       profile + s(age, profile, bs="fs", k=10)   [no global age smooth]
#   H2  shared global age smooth + penalised profile deviations
#       (Pedersen et al. 2019 "GS"): s(age, k=10, ts) + s(age, profile, bs="fs", m=1)
#   H3  partially pooled age-group x profile interaction:
#       profile + age_group + s(age) + s(profile:age_group, bs="re")
# plus sensitivity variants (basis size, fixed smoothing, unpenalised
# age-group interaction, linear slopes). All share sex + year +
# s(state, bs="re"), fitted with mgcv::bam(fREML, discrete) as in script 12.
#
# Missing-outcome sensitivity: Gate 3.2B scenarios C1/C2 (state-years with
# >=70% / >=80% hospitalisation recording, >=100 eligible cases), refitted
# and compared on a common target population.
#
# Does not modify any earlier output. Output:
#   03_Output/figure_revision/figure3_final/  (see FIGURE3_FINAL_VALIDATION.md)
# Runtime: ~20 min.
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

options(warn = 1)

out_dir <- "03_Output/figure_revision/figure3_final"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

n_threads <- max(1, min(8, parallel::detectCores() - 2))
B_primary <- 2000
B_sens <- 1000
event_threshold <- 10   # screening heuristic, consistent with the mortality audit

core_profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD",
                         "HTN+CKD", "DM+HTN+CKD")
age_breaks <- c(0, 15, 40, 60, 75, 101)
age_levels <- c("0-14", "15-39", "40-59", "60-74", "75+")
cut_age <- function(a) cut(a, age_breaks, labels = age_levels, right = FALSE,
                           include.lowest = TRUE)

log_msg <- function(...) message(sprintf("[52 %s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...))

## ============================================================
## 1. Cohorts (identical rules to script 12) and count reconciliation
## ============================================================
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

eligible <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                  ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::mutate(
    year = factor(lubridate::year(event_date)),
    sex = factor(sex, levels = c("female", "male")),
    uf_residence = factor(uf_residence),
    dm = as.integer(diabetes == "yes"),
    htn = as.integer(hypertension == "yes"),
    ckd = as.integer(renal_disease == "yes"),
    core_profile = factor(dplyr::case_when(
      dm == 0 & htn == 0 & ckd == 0 ~ "None",
      dm == 1 & htn == 0 & ckd == 0 ~ "DM",
      dm == 0 & htn == 1 & ckd == 0 ~ "HTN",
      dm == 0 & htn == 0 & ckd == 1 ~ "CKD",
      dm == 1 & htn == 1 & ckd == 0 ~ "DM+HTN",
      dm == 1 & htn == 0 & ckd == 1 ~ "DM+CKD",
      dm == 0 & htn == 1 & ckd == 1 ~ "HTN+CKD",
      TRUE ~ "DM+HTN+CKD"
    ), levels = core_profile_levels),
    hosp_known = hospitalised %in% c("no", "yes")
  ) |>
  dplyr::select(age_years, sex, year, uf_residence, core_profile, hosp_known,
                hospitalised, died_from_chik)
rm(ind)
invisible(gc())

add_model_vars <- function(df) {
  df |>
    dplyr::mutate(
      age_group = cut_age(age_years),
      age10 = (age_years - 50) / 10,
      profile_age = factor(paste(core_profile, age_group, sep = " | "),
                           levels = as.vector(outer(core_profile_levels, age_levels,
                                                    paste, sep = " | "))),
      lin_DM = age10 * (core_profile == "DM"),
      lin_HTN = age10 * (core_profile == "HTN"),
      lin_CKD = age10 * (core_profile == "CKD"),
      lin_DM_HTN = age10 * (core_profile == "DM+HTN"),
      lin_DM_CKD = age10 * (core_profile == "DM+CKD"),
      lin_HTN_CKD = age10 * (core_profile == "HTN+CKD"),
      lin_DM_HTN_CKD = age10 * (core_profile == "DM+HTN+CKD")
    )
}

hosp <- eligible |>
  dplyr::filter(hosp_known) |>
  dplyr::mutate(y = as.integer(hospitalised == "yes")) |>
  dplyr::select(y, age_years, sex, year, uf_residence, core_profile) |>
  add_model_vars()

reconciliation <- tibble::tibble(
  metric = c("Core-3 eligible population", "Hospitalisation outcome-known cohort",
             "Hospitalisations", "Hospitalisation outcome missing (% of eligible)",
             "Mortality outcome-known cohort", "Chikungunya-attributed deaths"),
  expected = c(1074796, 776072, 25363, NA, 983947, 1050),
  observed = c(nrow(eligible), nrow(hosp), sum(hosp$y),
               round(100 * mean(!eligible$hosp_known), 2),
               sum(!is.na(eligible$died_from_chik)),
               sum(eligible$died_from_chik, na.rm = TRUE))
) |>
  dplyr::mutate(match = ifelse(is.na(expected), NA, expected == observed))
print(reconciliation)
readr::write_csv(reconciliation, file.path(out_dir, "cohort_reconciliation.csv"))
stopifnot(all(reconciliation$match, na.rm = TRUE))

## ============================================================
## 2. Empirical support by age group x profile
## ============================================================
q <- function(x, p) if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE)) else NA_real_

support <- hosp |>
  dplyr::group_by(age_group, core_profile, .drop = FALSE) |>
  dplyr::summarise(
    n_cases = dplyr::n(), n_hosp = sum(y), n_nonhosp = n_cases - n_hosp,
    age_min = if (n_cases > 0) min(age_years) else NA_real_,
    age_q1 = q(age_years, 0.25), age_median = q(age_years, 0.5), age_q3 = q(age_years, 0.75),
    age_max = if (n_cases > 0) max(age_years) else NA_real_,
    n_distinct_state_year = dplyr::n_distinct(paste(uf_residence, year)),
    .groups = "drop"
  )
ci <- purrr::map2_dfr(support$n_hosp, support$n_cases, function(x, n) {
  if (n == 0) return(tibble::tibble(crude_lower_pct = NA_real_, crude_upper_pct = NA_real_))
  b <- binom::binom.confint(x, n, methods = "exact")
  tibble::tibble(crude_lower_pct = 100 * b$lower, crude_upper_pct = 100 * b$upper)
})
support <- dplyr::bind_cols(support, ci) |>
  dplyr::mutate(
    crude_risk_pct = 100 * n_hosp / n_cases,
    pct_of_cohort = 100 * n_cases / nrow(hosp),
    support_category = dplyr::case_when(
      n_hosp >= event_threshold & n_nonhosp >= event_threshold ~ "Adequate",
      n_hosp >= 1 ~ "Sparse",
      TRUE ~ "No events"
    ),
    ge5_events = n_hosp >= 5, ge10_events = n_hosp >= 10, ge20_events = n_hosp >= 20,
    ge50_events = n_hosp >= 50,
    ## Relative half-width of the exact CI on the crude risk: a direct,
    ## threshold-free measure of how precisely the cell pins down its risk.
    crude_ci_ratio = crude_upper_pct / pmax(crude_lower_pct, 1e-6),
    age_group = as.character(age_group), core_profile = as.character(core_profile)
  )

## Profile-level age distribution of cases and of hospitalisations.
profile_age_dist <- hosp |>
  dplyr::group_by(core_profile) |>
  dplyr::summarise(
    n_cases = dplyr::n(), n_hosp = sum(y),
    case_age_median = q(age_years, 0.5), case_age_q1 = q(age_years, 0.25),
    case_age_q3 = q(age_years, 0.75),
    hosp_age_median = q(age_years[y == 1], 0.5), hosp_age_q1 = q(age_years[y == 1], 0.25),
    hosp_age_q3 = q(age_years[y == 1], 0.75),
    hosp_age_min = min(age_years[y == 1]), hosp_age_max = max(age_years[y == 1]),
    hosp_age_lt15 = sum(y == 1 & age_years < 15),
    .groups = "drop"
  )
readr::write_csv(profile_age_dist, file.path(out_dir, "hospital_profile_age_distribution.csv"))

## Childhood detail: 0-14 split into 0-4 / 5-9 / 10-14 (infant hospitalisation
## is known to be high in chikungunya).
child_detail <- hosp |>
  dplyr::filter(age_years < 15) |>
  dplyr::mutate(child_band = cut(age_years, c(0, 1, 5, 10, 15), right = FALSE,
                                 labels = c("<1", "1-4", "5-9", "10-14"))) |>
  dplyr::group_by(child_band, core_profile, .drop = FALSE) |>
  dplyr::summarise(n_cases = dplyr::n(), n_hosp = sum(y), .groups = "drop") |>
  dplyr::mutate(crude_risk_pct = 100 * n_hosp / n_cases)
readr::write_csv(child_detail, file.path(out_dir, "hospital_childhood_detail.csv"))

log_msg("support tables built")

## ============================================================
## 3. Models
## ============================================================
fit_bam <- function(formula, data, sp = NULL) {
  warns <- character()
  t0 <- Sys.time()
  fit <- withCallingHandlers(
    mgcv::bam(formula, data = data, family = binomial(link = "logit"),
              method = "fREML", discrete = TRUE, nthreads = n_threads, sp = sp),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  fit$audit_warnings <- warns
  fit$audit_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  fit
}

base <- "sex + year + s(uf_residence, bs = 're')"
specs <- list(
  H0 = list(label = "H0: no age interaction, profile + s(age, k=10)",
            f = paste("y ~ core_profile +", base, "+ s(age_years, k = 10, bs = 'ts')")),
  H0_k20 = list(label = "H0 with s(age, k=20)",
                f = paste("y ~ core_profile +", base, "+ s(age_years, k = 20, bs = 'ts')")),
  H1 = list(label = "H1: original factor-smooth, s(age, profile, bs='fs', k=10)",
            f = paste("y ~ core_profile +", base, "+ s(age_years, core_profile, bs = 'fs', k = 10)")),
  H2 = list(label = "H2: global s(age) + fs deviations (m=1)",
            f = paste("y ~ core_profile +", base, "+ s(age_years, k = 10, bs = 'ts')",
                      "+ s(age_years, core_profile, bs = 'fs', k = 10, m = 1)")),
  H3 = list(label = "H3: age-group x profile random effect + s(age)",
            f = paste("y ~ core_profile + age_group +", base,
                      "+ s(age_years, k = 10, bs = 'ts') + s(profile_age, bs = 're')")),
  H3_fixed = list(label = "H3 fixed: unpenalised profile x age-group + s(age)",
                  f = paste("y ~ core_profile * age_group +", base, "+ s(age_years, k = 10, bs = 'ts')")),
  H1_k5 = list(label = "H1, k = 5",
               f = paste("y ~ core_profile +", base, "+ s(age_years, core_profile, bs = 'fs', k = 5)")),
  H1_k15 = list(label = "H1, k = 15",
                f = paste("y ~ core_profile +", base, "+ s(age_years, core_profile, bs = 'fs', k = 15)")),
  H2_k5 = list(label = "H2, k = 5",
               f = paste("y ~ core_profile +", base, "+ s(age_years, k = 10, bs = 'ts')",
                         "+ s(age_years, core_profile, bs = 'fs', k = 5, m = 1)")),
  H2_k15 = list(label = "H2, k = 15",
                f = paste("y ~ core_profile +", base, "+ s(age_years, k = 15, bs = 'ts')",
                          "+ s(age_years, core_profile, bs = 'fs', k = 15, m = 1)")),
  linear = list(label = "Linear age x profile slopes + s(age)",
                f = paste("y ~ core_profile + lin_DM + lin_HTN + lin_CKD + lin_DM_HTN +",
                          "lin_DM_CKD + lin_HTN_CKD + lin_DM_HTN_CKD +", base,
                          "+ s(age_years, k = 10, bs = 'ts')"))
)

fits <- list()
for (nm in names(specs)) {
  log_msg("fitting %s", nm)
  fits[[nm]] <- fit_bam(stats::as.formula(specs[[nm]]$f), hosp)
}

## Fixed-smoothing variants: profile-deviation smoothing parameters x1/4, x4.
for (base_nm in c("H1", "H2")) {
  sp_hat <- fits[[base_nm]]$sp
  idx <- grepl("core_profile", names(sp_hat), fixed = TRUE)
  for (mult in c(0.25, 4)) {
    nm <- sprintf("%s_sp_x%s", base_nm, mult)
    sp_new <- sp_hat; sp_new[idx] <- sp_new[idx] * mult
    specs[[nm]] <- list(label = sprintf("%s, profile smoothing parameters x %s (fixed)", base_nm, mult),
                        f = specs[[base_nm]]$f)
    log_msg("fitting %s", nm)
    fits[[nm]] <- fit_bam(stats::as.formula(specs[[nm]]$f), hosp, sp = sp_new)
  }
}
log_msg("all fits complete")

## ------------------------------------------------------------
## 3a. Reproduce the original hospitalisation age-curve (H1)
## ------------------------------------------------------------
load("01_Data/core_profile_rr_age_curve.RData")
cached <- dplyr::filter(core_profile_rr_age_curve, outcome == "Hospitalisation")
strata_all <- dplyr::count(hosp, sex, year, uf_residence, name = "n") |>
  dplyr::mutate(w = n / sum(n))

curve_point <- function(fit, ages = 1:95) {
  purrr::map_dfr(core_profile_levels, function(p) {
    g <- tidyr::expand_grid(age_years = ages, strata_all) |>
      dplyr::mutate(core_profile = factor(p, levels = core_profile_levels)) |>
      add_model_vars()
    pr <- stats::plogis(as.numeric(stats::predict(fit, newdata = g, type = "lpmatrix") %*% coef(fit)))
    tibble::tibble(core_profile = p, age_years = ages,
                   std_risk = colSums(matrix(pr, nrow = nrow(strata_all)) * strata_all$w))
  }) |>
    dplyr::group_by(age_years) |>
    dplyr::mutate(rr = std_risk / std_risk[core_profile == "None"]) |>
    dplyr::ungroup()
}

curves <- purrr::map_dfr(c("H0", "H1", "H2"), ~ dplyr::mutate(curve_point(fits[[.x]]), model = .x))
repro <- curves |>
  dplyr::filter(model == "H1") |>
  dplyr::inner_join(dplyr::transmute(cached, core_profile = as.character(core_profile), age_years,
                                     cached_rr = rr, cached_risk = standardised_risk),
                    by = c("core_profile", "age_years")) |>
  dplyr::summarise(n_points = dplyr::n(),
                   max_abs_rel_diff_risk = max(abs(std_risk / cached_risk - 1)),
                   max_abs_rel_diff_rr = max(abs(rr / cached_rr - 1)))
print(repro)
readr::write_csv(repro, file.path(out_dir, "hospital_H1_reproduction_check.csv"))
readr::write_csv(curves, file.path(out_dir, "hospital_continuous_curves_H0_H1_H2.csv"))

## Local support of the original curve: hospitalisations in the same
## profile within +/-5 years of each plotted age.
by_age <- hosp |> dplyr::count(core_profile, age_years, wt = y, name = "events") |>
  dplyr::mutate(core_profile = as.character(core_profile))
local <- tidyr::expand_grid(core_profile = core_profile_levels, age_years = 1:95) |>
  dplyr::left_join(by_age, by = "core_profile", relationship = "many-to-many",
                   suffix = c("", "_obs")) |>
  dplyr::filter(abs(age_years_obs - age_years) <= 5) |>
  dplyr::group_by(core_profile, age_years) |>
  dplyr::summarise(local_events = sum(events), .groups = "drop") |>
  tidyr::complete(core_profile = core_profile_levels, age_years = 1:95,
                  fill = list(local_events = 0))
curve_audit <- dplyr::filter(curves, model %in% c("H1", "H2")) |>
  dplyr::left_join(local, by = c("core_profile", "age_years")) |>
  dplyr::mutate(local_support = dplyr::case_when(
    local_events >= event_threshold ~ ">=10 events within +/-5 y",
    local_events >= 1 ~ "1-9 events within +/-5 y",
    TRUE ~ "No events within +/-5 y"))
readr::write_csv(curve_audit, file.path(out_dir, "hospital_curve_local_support_audit.csv"))

## ------------------------------------------------------------
## 3b. Diagnostics and interaction tests
## ------------------------------------------------------------
## Penalisation-aware Wald test (Bayesian covariance Vp) that each profile's
## log-OR vs None is equal at `ages` and `ref_age`. Sex/year/state cancel.
age_shape_wald <- function(fit, ages, ref_age, profiles = core_profile_levels[-1]) {
  grid <- tidyr::expand_grid(core_profile = factor(core_profile_levels, levels = core_profile_levels),
                             age_years = sort(unique(c(ages, ref_age)))) |>
    dplyr::mutate(sex = factor("female", levels = levels(hosp$sex)),
                  year = factor(levels(hosp$year)[1], levels = levels(hosp$year)),
                  uf_residence = factor(levels(hosp$uf_residence)[1], levels = levels(hosp$uf_residence))) |>
    add_model_vars()
  X <- stats::predict(fit, newdata = grid, type = "lpmatrix")
  row_of <- function(p, a) which(grid$core_profile == p & grid$age_years == a)
  test_ages <- setdiff(ages, ref_age)
  build_L <- function(ps) do.call(rbind, lapply(ps, function(p) t(sapply(test_ages, function(a)
    (X[row_of(p, a), ] - X[row_of("None", a), ]) - (X[row_of(p, ref_age), ] - X[row_of("None", ref_age), ])))))
  wald <- function(L) {
    d <- as.numeric(L %*% coef(fit)); V <- L %*% fit$Vp %*% t(L)
    e <- eigen((V + t(V)) / 2, symmetric = TRUE); keep <- e$values > max(e$values) * 1e-7
    z <- crossprod(e$vectors[, keep, drop = FALSE], d); stat <- sum(z^2 / e$values[keep])
    c(stat = stat, df = sum(keep), p = stats::pchisq(stat, sum(keep), lower.tail = FALSE))
  }
  purrr::map_dfr(c(profiles, "All 7 profiles jointly"), function(p) {
    ps <- if (p == "All 7 profiles jointly") profiles else p
    w <- wald(build_L(ps))
    tibble::tibble(core_profile = p, wald_chisq = w[["stat"]], wald_df = w[["df"]], wald_p = w[["p"]])
  })
}

wald_sets <- list(
  adult_20_90 = list(ages = seq(20, 90, 10), ref = 60),
  full_1_95 = list(ages = c(1, 5, 10, 15, seq(20, 90, 10), 95), ref = 60),
  child_vs_young_adult = list(ages = c(2, 7, 12), ref = 30)
)
smooth_models <- c("H1", "H2", "H1_k5", "H1_k15", "H2_k5", "H2_k15",
                   "H1_sp_x0.25", "H1_sp_x4", "H2_sp_x0.25", "H2_sp_x4")
wald_tests <- purrr::map_dfr(smooth_models, function(nm) purrr::imap_dfr(wald_sets, function(s, set_nm)
  age_shape_wald(fits[[nm]], s$ages, s$ref) |> dplyr::mutate(model = nm, test = set_nm, .before = 1)))

## Unpenalised / linear interactions: ordinary Wald on interaction coefficients.
coef_wald <- function(fit, pattern, label) {
  i <- grep(pattern, names(coef(fit)))
  b <- coef(fit)[i]; V <- fit$Vp[i, i]
  e <- eigen((V + t(V)) / 2, symmetric = TRUE); keep <- e$values > max(e$values) * 1e-9
  z <- crossprod(e$vectors[, keep, drop = FALSE], b); stat <- sum(z^2 / e$values[keep])
  tibble::tibble(model = label, test = "interaction coefficients = 0", core_profile = "All 7 profiles jointly",
                 wald_chisq = stat, wald_df = sum(keep),
                 wald_p = stats::pchisq(stat, sum(keep), lower.tail = FALSE))
}
wald_tests <- dplyr::bind_rows(
  wald_tests,
  coef_wald(fits$linear, "^lin_", "linear"),
  coef_wald(fits$H3_fixed, "^core_profile.*:age_group", "H3_fixed")
)

vc <- tryCatch(mgcv::gam.vcomp(fits$H3), error = function(e) NULL)
h3_sd <- if (!is.null(vc)) {
  m <- if (is.list(vc)) vc$vc else vc
  r <- grep("profile_age", rownames(m))
  c(est = m[r, 1], lo = m[r, 2], hi = m[r, 3])
} else c(est = NA, lo = NA, hi = NA)

smooth_edf <- function(fit, pattern) {
  st <- summary(fit)$s.table
  i <- grepl(pattern, rownames(st), fixed = TRUE)
  if (!any(i)) NA_real_ else sum(st[i, "edf"])
}

diagnostics <- purrr::imap_dfr(fits, function(fit, nm) {
  set.seed(52)
  kc <- tryCatch(mgcv::k.check(fit, subsample = 5000, n.rep = 200), error = function(e) NULL)
  ar <- if (!is.null(kc)) grepl("age_years", rownames(kc)) else logical()
  ev <- eigen(fit$Vp, symmetric = TRUE, only.values = TRUE)$values
  tibble::tibble(
    model = nm, label = specs[[nm]]$label, formula = specs[[nm]]$f,
    smoothing = if (grepl("_sp_x", nm)) "fixed" else "estimated (fREML)",
    n = nrow(fit$model), events = sum(fit$y),
    aic = stats::AIC(fit), total_edf = sum(fit$edf),
    edf_age_terms = smooth_edf(fit, "age_years"),
    edf_profile_age_re = smooth_edf(fit, "profile_age"),
    deviance_explained_pct = 100 * summary(fit)$dev.expl,
    converged = if (is.null(fit$converged)) NA else fit$converged,
    n_warnings = length(fit$audit_warnings),
    warnings = paste(unique(fit$audit_warnings), collapse = " | "),
    vp_min_eigen = min(ev), max_coef_se = max(sqrt(diag(fit$Vp))),
    k_index_min_age = if (any(ar)) min(kc[ar, "k-index"]) else NA_real_,
    k_check_p_min_age = if (any(ar)) min(kc[ar, "p-value"]) else NA_real_,
    fit_seconds = fit$audit_seconds
  )
}) |>
  dplyr::mutate(delta_aic_vs_H0 = aic - aic[model == "H0"],
                delta_edf_vs_H0 = total_edf - total_edf[model == "H0"])

lrt <- purrr::map_dfr(c("H0_k20", "H1", "H2", "H3", "H3_fixed", "linear", "H1_k15", "H2_k15"), function(nm) {
  a <- tryCatch(mgcv::anova.gam(fits$H0, fits[[nm]], test = "Chisq"), error = function(e) NULL)
  if (is.null(a)) return(tibble::tibble(model = nm, lrt_deviance = NA, lrt_df = NA, lrt_p = NA))
  tibble::tibble(model = nm, lrt_deviance = a$Deviance[2], lrt_df = a$Df[2], lrt_p = a$`Pr(>Chi)`[2])
})

model_comparison <- diagnostics |>
  dplyr::select(model, label, smoothing, aic, delta_aic_vs_H0, total_edf, delta_edf_vs_H0,
                edf_age_terms, deviance_explained_pct) |>
  dplyr::left_join(lrt, by = "model") |>
  dplyr::left_join(
    wald_tests |> dplyr::filter(core_profile == "All 7 profiles jointly") |>
      dplyr::select(model, test, wald_p) |>
      tidyr::pivot_wider(names_from = test, values_from = wald_p, names_prefix = "joint_wald_p_"),
    by = "model") |>
  dplyr::mutate(h3_profile_age_sd = ifelse(model == "H3", h3_sd[["est"]], NA),
                h3_profile_age_sd_lower = ifelse(model == "H3", h3_sd[["lo"]], NA),
                h3_profile_age_sd_upper = ifelse(model == "H3", h3_sd[["hi"]], NA))

readr::write_csv(model_comparison, file.path(out_dir, "hospital_age_model_comparison.csv"), na = "")
readr::write_csv(dplyr::left_join(diagnostics, dplyr::select(model_comparison, model, dplyr::starts_with("joint_wald")),
                                  by = "model"),
                 file.path(out_dir, "hospital_age_interaction_diagnostics.csv"), na = "")
readr::write_csv(wald_tests, file.path(out_dir, "hospital_age_wald_tests_by_profile.csv"))
print(as.data.frame(model_comparison[, c("model", "aic", "delta_aic_vs_H0", "total_edf", "lrt_p",
                                         grep("joint_wald", names(model_comparison), value = TRUE))]),
      digits = 4)

## Observed vs expected hospitalisations per cell under the main models.
oe <- hosp |>
  dplyr::mutate(e_H0 = fitted(fits$H0), e_H1 = fitted(fits$H1), e_H2 = fitted(fits$H2),
                e_H3 = fitted(fits$H3)) |>
  dplyr::group_by(age_group, core_profile) |>
  dplyr::summarise(expected_hosp_H0 = sum(e_H0), expected_hosp_H1 = sum(e_H1),
                   expected_hosp_H2 = sum(e_H2), expected_hosp_H3 = sum(e_H3), .groups = "drop") |>
  dplyr::mutate(age_group = as.character(age_group), core_profile = as.character(core_profile))

## Covariate overlap: share of each age group's standardisation target that
## lives in state-years where the profile has at least one case of that age
## group. Low values mean the standardised risk leans on extrapolation
## across states/years with no observed cases of that profile.
overlap <- purrr::map_dfr(age_levels, function(g) {
  tgt <- hosp |> dplyr::filter(age_group == g) |> dplyr::count(uf_residence, year, name = "n_target")
  purrr::map_dfr(core_profile_levels, function(p) {
    has <- hosp |> dplyr::filter(age_group == g, core_profile == p) |> dplyr::distinct(uf_residence, year)
    tibble::tibble(age_group = g, core_profile = p,
                   target_share_in_supported_state_years =
                     sum(dplyr::semi_join(tgt, has, by = c("uf_residence", "year"))$n_target) / sum(tgt$n_target))
  })
})

support_out <- support |>
  dplyr::left_join(oe, by = c("age_group", "core_profile")) |>
  dplyr::left_join(overlap, by = c("age_group", "core_profile")) |>
  dplyr::arrange(factor(age_group, levels = age_levels), factor(core_profile, levels = core_profile_levels))
readr::write_csv(support_out, file.path(out_dir, "hospital_age_profile_support.csv"), na = "")

log_msg("diagnostics written")

## ============================================================
## 4. Within-age-group standardised risks, RR and RD
## ============================================================
## Target for age group g: observed joint (exact age, sex, year, state)
## distribution of all hospitalisation-cohort members in g; identical for
## every profile. Same draw for every profile and age group.
weighted_risk_draws <- function(X, bd, w, block = 250) {
  out <- numeric(nrow(bd))
  for (i in seq.int(1, nrow(bd), by = block)) {
    idx <- i:min(i + block - 1, nrow(bd))
    out[idx] <- colSums(stats::plogis(X %*% t(bd[idx, , drop = FALSE])) * w) / sum(w)
  }
  out
}

standardise <- function(fit, target, B, seed) {
  set.seed(seed)
  beta <- coef(fit)
  bd <- MASS::mvrnorm(B, beta, stats::vcov(fit, unconditional = TRUE))
  res <- list()
  for (g in age_levels) {
    combo <- target |> dplyr::filter(age_group == g) |>
      dplyr::count(age_years, sex, year, uf_residence, name = "n")
    pt <- setNames(numeric(8), core_profile_levels)
    dr <- matrix(NA_real_, B, 8, dimnames = list(NULL, core_profile_levels))
    for (p in core_profile_levels) {
      nd <- dplyr::mutate(combo, core_profile = factor(p, levels = core_profile_levels)) |> add_model_vars()
      ## Factor levels must match the fitted model's (restricted refits).
      nd$uf_residence <- factor(as.character(nd$uf_residence), levels = levels(fit$model$uf_residence))
      nd$year <- factor(as.character(nd$year), levels = levels(fit$model$year))
      X <- stats::predict(fit, newdata = nd, type = "lpmatrix")
      pt[p] <- sum(stats::plogis(as.numeric(X %*% beta)) * combo$n) / sum(combo$n)
      dr[, p] <- weighted_risk_draws(X, bd, combo$n)
    }
    res[[g]] <- list(point = pt, draws = dr)
  }
  res
}

qq <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE))
summarise_std <- function(s, model) {
  purrr::map_dfr(age_levels, function(g) {
    pt <- s[[g]]$point; dr <- s[[g]]$draws
    purrr::map_dfr(core_profile_levels, function(p) {
      rr_d <- dr[, p] / dr[, "None"]; rd_d <- dr[, p] - dr[, "None"]
      tibble::tibble(model = model, age_group = g, core_profile = p,
                     std_risk_pct = 100 * pt[[p]],
                     std_risk_lower_pct = 100 * qq(dr[, p], 0.025),
                     std_risk_upper_pct = 100 * qq(dr[, p], 0.975),
                     rr = pt[[p]] / pt[["None"]], rr_lower = qq(rr_d, 0.025), rr_upper = qq(rr_d, 0.975),
                     rd_pp = 100 * (pt[[p]] - pt[["None"]]),
                     rd_lower_pp = 100 * qq(rd_d, 0.025), rd_upper_pp = 100 * qq(rd_d, 0.975))
    })
  })
}

## Ratio of RRs vs the 40-59 group (effect-modification contrast), within draw.
rr_ratio <- function(s, model, ref = "40-59") {
  purrr::map_dfr(setdiff(age_levels, ref), function(g) purrr::map_dfr(core_profile_levels[-1], function(p) {
    r <- (s[[g]]$draws[, p] / s[[g]]$draws[, "None"]) / (s[[ref]]$draws[, p] / s[[ref]]$draws[, "None"])
    tibble::tibble(model = model, age_group = g, reference_age_group = ref, core_profile = p,
                   rr_ratio = (s[[g]]$point[[p]] / s[[g]]$point[["None"]]) /
                     (s[[ref]]$point[[p]] / s[[ref]]$point[["None"]]),
                   rr_ratio_lower = qq(r, 0.025), rr_ratio_upper = qq(r, 0.975))
  }))
}

main_models <- c("H0", "H1", "H2", "H3", "H3_fixed")
sens_models <- c("H0_k20", "H1_k5", "H1_k15", "H2_k5", "H2_k15", "linear",
                 "H1_sp_x0.25", "H1_sp_x4", "H2_sp_x0.25", "H2_sp_x4")
std_all <- list(); est_all <- list(); ratio_all <- list()
for (i in seq_along(c(main_models, sens_models))) {
  nm <- c(main_models, sens_models)[i]
  B <- if (nm %in% main_models) B_primary else B_sens
  log_msg("standardising %s (B = %d)", nm, B)
  s <- standardise(fits[[nm]], hosp, B, seed = 520 + i)
  std_all[[nm]] <- s
  est_all[[nm]] <- summarise_std(s, nm)
  ratio_all[[nm]] <- rr_ratio(s, nm)
}

sup_flags <- dplyr::select(support_out, age_group, core_profile, n_cases, n_hosp, n_nonhosp,
                           crude_risk_pct, support_category, target_share_in_supported_state_years)
est <- dplyr::bind_rows(est_all) |>
  dplyr::left_join(sup_flags, by = c("age_group", "core_profile")) |>
  dplyr::mutate(estimate_status = dplyr::case_when(
    core_profile == "None" ~ "Reference",
    support_category == "Adequate" ~ "Reported",
    support_category == "Sparse" ~ "Flagged: sparse (1-9 events)",
    TRUE ~ "Suppressed: no observed events"))

readr::write_csv(est, file.path(out_dir, "hospital_age_profile_all_model_estimates.csv"))
readr::write_csv(dplyr::bind_rows(ratio_all), file.path(out_dir, "hospital_age_rr_ratio_contrasts.csv"))

## Robustness: every model vs H0 and vs H2 for each cell.
robust <- est |>
  dplyr::filter(core_profile != "None") |>
  dplyr::select(model, age_group, core_profile, estimate_status, std_risk_pct, rr, rr_lower, rr_upper) |>
  dplyr::group_by(age_group, core_profile) |>
  dplyr::mutate(risk_ratio_vs_H2 = std_risk_pct / std_risk_pct[model == "H2"],
                rr_ratio_vs_H2 = rr / rr[model == "H2"],
                rr_ratio_vs_H0 = rr / rr[model == "H0"],
                excludes_1 = rr_lower > 1 | rr_upper < 1) |>
  dplyr::ungroup()
robust_summary <- robust |>
  dplyr::group_by(model, reported = estimate_status == "Reported") |>
  dplyr::summarise(n_cells = dplyr::n(),
                   max_abs_log_risk_diff_vs_H2 = max(abs(log(risk_ratio_vs_H2))),
                   median_abs_log_rr_diff_vs_H2 = stats::median(abs(log(rr_ratio_vs_H2))),
                   max_abs_log_rr_diff_vs_H2 = max(abs(log(rr_ratio_vs_H2))),
                   .groups = "drop")
readr::write_csv(robust, file.path(out_dir, "hospital_age_model_robustness.csv"))
readr::write_csv(robust_summary, file.path(out_dir, "hospital_age_model_robustness_summary.csv"))
print(as.data.frame(robust_summary), digits = 3)

## ============================================================
## 5. Missing-outcome sensitivity (Gate 3.2B scenarios C1/C2)
## ============================================================
## Completeness per state x year on the ELIGIBLE cohort; strata need >=100
## eligible cases (prespecified in script 40). The restricted hospitalisation
## cohort is refitted; full and restricted models are both standardised to
## the restricted cohort's age-group targets (common target).
completeness <- eligible |>
  dplyr::group_by(uf_residence, year) |>
  dplyr::summarise(n_eligible = dplyr::n(), n_known = sum(hosp_known), .groups = "drop") |>
  dplyr::mutate(completeness = n_known / n_eligible)

completeness_by_age_profile <- eligible |>
  dplyr::mutate(age_group = cut_age(age_years)) |>
  dplyr::group_by(age_group, core_profile) |>
  dplyr::summarise(n_eligible = dplyr::n(), pct_hosp_recorded = 100 * mean(hosp_known), .groups = "drop")
readr::write_csv(completeness_by_age_profile, file.path(out_dir, "hospital_recording_completeness_by_age_profile.csv"))

missing_models <- c("H0", "H2", "H3")
miss_rows <- list()
for (sc in c(C1 = 0.70, C2 = 0.80) |> names()) {
  thr <- c(C1 = 0.70, C2 = 0.80)[[sc]]
  keep <- completeness |> dplyr::filter(n_eligible >= 100, completeness >= thr) |>
    dplyr::select(uf_residence, year)
  d_sc <- hosp |> dplyr::semi_join(keep, by = c("uf_residence", "year")) |>
    dplyr::mutate(uf_residence = droplevels(uf_residence), year = droplevels(year))
  log_msg("scenario %s: n = %d, events = %d", sc, nrow(d_sc), sum(d_sc$y))
  for (m in missing_models) {
    f_sc <- fit_bam(stats::as.formula(specs[[m]]$f), d_sc)
    s_restricted <- summarise_std(standardise(f_sc, d_sc, B_sens, seed = 600), m)
    s_full <- summarise_std(standardise(fits[[m]], d_sc, B_sens, seed = 600), m)
    miss_rows[[paste(sc, m)]] <- dplyr::bind_rows(
      dplyr::mutate(s_full, model_source = "full model"),
      dplyr::mutate(s_restricted, model_source = "restricted model")
    ) |> dplyr::mutate(scenario = sc, n_restricted = nrow(d_sc), events_restricted = sum(d_sc$y),
                       pct_of_hosp_cohort = 100 * nrow(d_sc) / nrow(hosp), .before = 1)
  }
}
missing_sens <- dplyr::bind_rows(miss_rows) |>
  dplyr::left_join(dplyr::select(support_out, age_group, core_profile, support_category),
                   by = c("age_group", "core_profile"))
readr::write_csv(missing_sens, file.path(out_dir, "hospital_age_missingness_sensitivity.csv"))

missing_summary <- missing_sens |>
  dplyr::select(scenario, model, model_source, age_group, core_profile, support_category,
                std_risk_pct, rr) |>
  tidyr::pivot_wider(names_from = model_source, values_from = c(std_risk_pct, rr)) |>
  dplyr::mutate(risk_ratio_restricted_vs_full = `std_risk_pct_restricted model` / `std_risk_pct_full model`,
                rr_ratio_restricted_vs_full = `rr_restricted model` / `rr_full model`) |>
  dplyr::group_by(scenario, model, adequate = support_category == "Adequate") |>
  dplyr::summarise(n_cells = dplyr::n(),
                   median_abs_log_risk_diff = stats::median(abs(log(risk_ratio_restricted_vs_full))),
                   max_abs_log_risk_diff = max(abs(log(risk_ratio_restricted_vs_full))),
                   median_abs_log_rr_diff = stats::median(abs(log(rr_ratio_restricted_vs_full)), na.rm = TRUE),
                   max_abs_log_rr_diff = max(abs(log(rr_ratio_restricted_vs_full)), na.rm = TRUE),
                   .groups = "drop")
readr::write_csv(missing_summary, file.path(out_dir, "hospital_age_missingness_sensitivity_summary.csv"))
print(as.data.frame(missing_summary), digits = 3)

saveRDS(list(draws = std_all[main_models], sessionInfo = utils::sessionInfo()),
        file.path(out_dir, "hospital_age_standardisation_draws.rds"))

log_msg("done")
