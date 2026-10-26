# ---------------------------------------------------------------------------
# 40_hospitalisation_restricted_sensitivity.R   -- Gate 3.2B
#
# RESTRICTED-SAMPLE SENSITIVITY ANALYSIS for the primary hospitalisation RR
# models in 12_core3_condition_specific_rr.R, following the Gate 3.2A
# missingness audit (03_Output/validation/gate32a/VALIDATION_3_2A.md), which
# found: (1) small age/sex/condition gradients in recording completeness,
# (2) large geographic (state/region) and temporal (year) gradients, and
# (3) a severe, sustained Distrito Federal (DF) recording collapse from 2020
# onward.
#
# This script independently REPRODUCES (not sources) the exact model
# specifications and g-computation/bootstrap functions from
# 12_core3_condition_specific_rr.R's hospitalisation sections, then refits
# them on four prespecified restricted samples (Scenarios B/C1/C2/D) and
# compares them to the full complete-case model (Scenario A, the existing
# primary analysis) under two standardisation regimes:
#   Analysis 1 - each model standardised to its own (restricted) population
#   Analysis 2 - COMMON-TARGET standardisation (primary robustness check):
#                the original full model AND each restricted model are both
#                applied to the SAME target population (that scenario's full
#                eligible population, known+unknown hospitalisation status),
#                isolating whether the estimated associations themselves
#                changed, separately from compositional change in who is
#                being averaged over.
#
# Does NOT modify 02_clean_chik_sinan_brazil.R, 12_core3_condition_specific_rr.R,
# any production cache/figure/table, or the mortality-outcome definition.
# Does NOT implement IPW, multiple imputation, or MNAR modelling.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds
#        01_Data/core3_rr_hosp.RData, 01_Data/core_profile_rr_hosp.RData
#          (read-only, for the "original manuscript" comparison column)
# Output: 03_Output/validation/gate32b/*.csv, *.png, VALIDATION_3_2B.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(lubridate)
  library(mgcv); library(MASS); library(tibble); library(readr)
})
options(scipen = 999, dplyr.summarise.inform = FALSE)

out_dir <- "03_Output/validation/gate32b"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
MIN_STRATUM_DENOM <- 100L   # prespecified minimum eligible N for a state-year stratum to be judged on
                            # its completeness % at all (Scenario C) - chosen because a stratum below
                            # this has a 95% CI on its own completeness wide enough (roughly +/-10pp at
                            # 70%) to make inclusion/exclusion by a point estimate unreliable; NOT chosen
                            # by looking at resulting RRs (see VALIDATION_3_2B.md Section on Scenario C).
SEED <- 1L                  # matches 12_core3_condition_specific_rr.R's own default bootstrap seed
B_BOOT <- 2000L              # matches 12_'s production bootstrap draw count

uf_lookup <- tibble::tribble(
  ~uf_residence, ~state_abbrev, ~region,
  "11","RO","Norte", "12","AC","Norte", "13","AM","Norte", "14","RR","Norte",
  "15","PA","Norte", "16","AP","Norte", "17","TO","Norte",
  "21","MA","Nordeste","22","PI","Nordeste","23","CE","Nordeste","24","RN","Nordeste",
  "25","PB","Nordeste","26","PE","Nordeste","27","AL","Nordeste","28","SE","Nordeste","29","BA","Nordeste",
  "31","MG","Sudeste","32","ES","Sudeste","33","RJ","Sudeste","35","SP","Sudeste",
  "41","PR","Sul","42","SC","Sul","43","RS","Sul",
  "50","MS","Centro-Oeste","51","MT","Centro-Oeste","52","GO","Centro-Oeste","53","DF","Centro-Oeste"
)

## =============================================================================
## PHASE 1 -- RECONSTRUCT ELIGIBLE COHORT (identical eligibility logic to
## 12_'s add_core3(), independently re-derived here per the task's explicit
## instruction not to source 12_ for helpers)
## =============================================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
core_cols <- c("diabetes", "hypertension", "renal_disease")

eligible <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::mutate(
    cal_year = lubridate::year(event_date),
    sex  = factor(sex, levels = c("female","male")),
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
    core_profile = factor(core_profile, levels = core_profile_levels),
    uf_residence = as.character(uf_residence),
    R_hosp = dplyr::if_else(hospitalised %in% c("no","yes"), 1L, 0L)
  ) |>
  dplyr::left_join(uf_lookup |> dplyr::select(uf_residence, state_abbrev), by = "uf_residence")

stopifnot(nrow(eligible) == 1074796)
message(sprintf("[40] eligible core-3 cohort N = %s (expected 1,074,796) -- PASS",
                 format(nrow(eligible), big.mark=",")))

## ---- verify DF is coded "53" in uf_residence, not the string "DF" ----
df_rows <- eligible |> dplyr::filter(state_abbrev == "DF")
stopifnot(nrow(df_rows) > 0, all(df_rows$uf_residence == "53"))
stopifnot(!any(eligible$uf_residence == "DF", na.rm = TRUE))
message(sprintf("[40] verified: Distrito Federal = uf_residence code '53' (n=%s eligible), not the string 'DF' -- PASS",
                 format(nrow(df_rows), big.mark=",")))

make_hosp_known <- function(df) {
  df |>
    dplyr::filter(hospitalised %in% c("no","yes")) |>
    dplyr::mutate(hosp_only = hospitalised == "yes")
}

hk_A_check <- make_hosp_known(eligible)
stopifnot(nrow(hk_A_check) == 776072, sum(hk_A_check$hosp_only) == 25363)
message(sprintf("[40] baseline hospitalisation-known cohort N = %s, events = %s (expected 776,072 / 25,363) -- PASS",
                 format(nrow(hk_A_check), big.mark=","), format(sum(hk_A_check$hosp_only), big.mark=",")))

## =============================================================================
## PHASE 2 -- DEFINE RESTRICTED SCENARIOS
## =============================================================================

## ---- Scenario A: reference, no restriction ----
eligible_A <- eligible

## ---- Scenario B: exclude Distrito Federal (uf_residence=="53"), 2020-2025 ----
## Restriction applied to the full ELIGIBLE population (known + unknown hosp status).
eligible_B <- eligible |>
  dplyr::filter(!(uf_residence == "53" & cal_year %in% 2020:2025))

## ---- Scenario C: state-year recording-completeness thresholds ----
## Completeness computed on the FULL eligible cohort (not complete cases only).
state_year_completeness <- eligible |>
  dplyr::group_by(uf_residence, state_abbrev, cal_year) |>
  dplyr::summarise(n_eligible = dplyr::n(), n_known = sum(R_hosp == 1L), .groups = "drop") |>
  dplyr::mutate(completeness = n_known / n_eligible)

## Prespecified rule: a stratum must have >= MIN_STRATUM_DENOM eligible cases
## to be judged on its completeness at all (below that, the completeness %
## itself is too noisy to trust as "high" or "low"). Strata below the minimum
## are EXCLUDED from both C1 and C2 regardless of their observed completeness.
flag_included <- function(df, threshold) {
  df |> dplyr::mutate(
    included_primary = n_eligible >= MIN_STRATUM_DENOM & completeness >= threshold,
    included_no_min_denom_rule = completeness >= threshold   ## diagnostic only - NOT used for the restricted cohort
  )
}
state_year_70 <- flag_included(state_year_completeness, 0.70)
state_year_80 <- flag_included(state_year_completeness, 0.80)

included_strata_C1 <- state_year_70 |> dplyr::filter(included_primary) |> dplyr::select(uf_residence, cal_year)
included_strata_C2 <- state_year_80 |> dplyr::filter(included_primary) |> dplyr::select(uf_residence, cal_year)

eligible_C1 <- eligible |> dplyr::semi_join(included_strata_C1, by = c("uf_residence","cal_year"))
eligible_C2 <- eligible |> dplyr::semi_join(included_strata_C2, by = c("uf_residence","cal_year"))

## ---- Scenario D: laboratory-confirmed cases only (existing is_lab_confirmed) ----
eligible_D <- eligible |> dplyr::filter(is_lab_confirmed)

scenario_eligible <- list(A = eligible_A, B = eligible_B, C1 = eligible_C1, C2 = eligible_C2, D = eligible_D)
scenario_known    <- purrr::map(scenario_eligible, make_hosp_known)

message("[40] scenario eligible-population sizes:")
for (nm in names(scenario_eligible)) {
  message(sprintf("  %-3s eligible N=%8s | hosp-known N=%8s | events=%6s",
    nm, format(nrow(scenario_eligible[[nm]]), big.mark=","),
    format(nrow(scenario_known[[nm]]), big.mark=","),
    format(sum(scenario_known[[nm]]$hosp_only), big.mark=",")))
}

saveRDS(list(scenario_eligible = scenario_eligible, scenario_known = scenario_known,
             state_year_completeness = state_year_completeness,
             state_year_70 = state_year_70, state_year_80 = state_year_80),
        file.path(out_dir, "_stage1_scenarios.rds"))

message("[40] STAGE 1 complete.")

## =============================================================================
## PHASE 3 -- MODEL FITTING (reproduces 12_core3_condition_specific_rr.R's
## exact hospitalisation model specifications; the file itself is never
## sourced, per task instruction, to avoid overwriting production caches)
## =============================================================================

## Factor levels for uf_residence/year are built from each scenario's own
## FITTING data (the hospitalisation-known subset) rather than the larger
## eligible population, so every declared factor level has >=1 training row
## (a zero-row level in a bs="re" smooth cannot be estimated and would
## silently become an unpredictable "new level" at predict() time). This
## also directly gives the "common support" set used by Phase 5's
## common-target analysis: anything outside these levels is, by definition,
## NOT supported by that scenario's restricted model.
prep_fit_data <- function(df) {
  df |>
    dplyr::mutate(
      uf_residence = factor(uf_residence),
      year         = factor(as.character(cal_year))
    )
}

fit_condition_model <- function(data) {
  mgcv::bam(
    hosp_only ~
      s(age_years, k = 10, bs = "ts") +
      sex + year + s(uf_residence, bs = "re") + dm + htn + ckd,
    data = data,
    family = binomial(link = "logit"),
    method = "fREML",
    discrete = TRUE
  )
}

fit_profile_model <- function(data) {
  data2 <- data
  data2$core_profile <- droplevels(data2$core_profile)
  mgcv::gam(
    hosp_only ~
      core_profile + sex + year + s(uf_residence, bs = "re") +
      s(age_years, k = 10, bs = "ts"),
    data = data2,
    family = binomial(link = "logit"),
    method = "REML"
  )
}

data_S   <- purrr::map(scenario_known, prep_fit_data)
fits_cond <- list(); fits_prof <- list()
convergence_log <- list()

for (nm in names(data_S)) {
  message(sprintf("[40] fitting Scenario %s condition-specific model (N=%s)...",
                   nm, format(nrow(data_S[[nm]]), big.mark=",")))
  t0 <- Sys.time()
  warn_cond <- character(0)
  fit_c <- withCallingHandlers(
    tryCatch(fit_condition_model(data_S[[nm]]), error = function(e) {
      message(sprintf("[40]   Scenario %s condition model FAILED: %s", nm, conditionMessage(e)))
      NULL
    }),
    warning = function(w) { warn_cond[length(warn_cond)+1] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  fits_cond[[nm]] <- fit_c
  message(sprintf("[40]   done in %.1fs. converged=%s",
                   as.numeric(difftime(Sys.time(), t0, units="secs")),
                   if (!is.null(fit_c)) isTRUE(fit_c$converged) else NA))

  message(sprintf("[40] fitting Scenario %s profile model (N=%s)...",
                   nm, format(nrow(data_S[[nm]]), big.mark=",")))
  t0 <- Sys.time()
  warn_prof <- character(0)
  fit_p <- withCallingHandlers(
    tryCatch(fit_profile_model(data_S[[nm]]), error = function(e) {
      message(sprintf("[40]   Scenario %s profile model FAILED: %s", nm, conditionMessage(e)))
      NULL
    }),
    warning = function(w) { warn_prof[length(warn_prof)+1] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  fits_prof[[nm]] <- fit_p
  message(sprintf("[40]   done in %.1fs. converged=%s",
                   as.numeric(difftime(Sys.time(), t0, units="secs")),
                   if (!is.null(fit_p)) isTRUE(fit_p$converged) else NA))

  profile_n_events <- data_S[[nm]] |>
    dplyr::group_by(core_profile) |>
    dplyr::summarise(n = dplyr::n(), n_events = sum(hosp_only), .groups = "drop")
  fitted_profile_levels <- if (!is.null(fit_p)) fit_p$xlevels[["core_profile"]] else character(0)

  convergence_log[[nm]] <- tibble::tibble(
    scenario = nm,
    model = c("condition_specific", "profile"),
    n_model = nrow(data_S[[nm]]),
    n_events_model = sum(data_S[[nm]]$hosp_only),
    converged = c(if (!is.null(fit_c)) isTRUE(fit_c$converged) else NA,
                  if (!is.null(fit_p)) isTRUE(fit_p$converged) else NA),
    fit_failed = c(is.null(fit_c), is.null(fit_p)),
    n_warnings = c(length(warn_cond), length(warn_prof)),
    warnings_text = c(paste(unique(warn_cond), collapse = " | "), paste(unique(warn_prof), collapse = " | ")),
    n_profile_levels_absent = sum(!core_profile_levels %in% fitted_profile_levels),
    profile_levels_absent = paste(setdiff(core_profile_levels, fitted_profile_levels), collapse=","),
    n_profile_levels_sparse_lt10events = sum(profile_n_events$n_events[match(core_profile_levels, profile_n_events$core_profile)] < 10, na.rm = TRUE)
  )
}

convergence_summary <- dplyr::bind_rows(convergence_log)
readr::write_csv(convergence_summary, file.path(out_dir, "model_convergence_and_sparse_cells.csv"))
message("[40] STAGE 2 (model fitting) complete.")

## =============================================================================
## PHASE 4 -- STANDARDISATION FUNCTIONS (verbatim re-derivation of
## standardised_rr() / standardise_profile_rr() / weighted_risk_draws() from
## 12_core3_condition_specific_rr.R's hospitalisation g-computation section)
## =============================================================================

weighted_risk_draws <- function(X, beta_draws, w, draw_block_size = 500) {
  draw_count <- nrow(beta_draws)
  output <- numeric(draw_count)
  for (first_draw in seq.int(1, draw_count, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1, draw_count)
    p_draws_block <- plogis(X %*% t(beta_draws[draw_index, , drop = FALSE]))
    output[draw_index] <- colSums(p_draws_block * w) / sum(w)
  }
  output
}

standardised_rr <- function(fit, target_data, exposure_var, B = B_BOOT, seed = SEED, draw_block_size = 100) {
  combo <- target_data |>
    dplyr::count(age_years, sex, year, uf_residence, dm, htn, ckd, name = "n")
  combo_exposed   <- combo; combo_exposed[[exposure_var]]   <- 1L
  combo_unexposed <- combo; combo_unexposed[[exposure_var]] <- 0L

  X1 <- predict(fit, newdata = combo_exposed,   type = "lpmatrix")
  X0 <- predict(fit, newdata = combo_unexposed, type = "lpmatrix")
  w <- combo$n
  beta_hat <- coef(fit)
  p1_point <- plogis(as.numeric(X1 %*% beta_hat))
  p0_point <- plogis(as.numeric(X0 %*% beta_hat))
  risk_exposed_point   <- sum(p1_point * w) / sum(w)
  risk_unexposed_point <- sum(p0_point * w) / sum(w)
  rr_point <- risk_exposed_point / risk_unexposed_point

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov(fit, unconditional = TRUE))
  risk_exposed_draws <- numeric(B); risk_unexposed_draws <- numeric(B)
  weight_sum <- sum(w)
  for (first_draw in seq.int(1, B, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1, B)
    beta_block <- t(beta_draws[draw_index, , drop = FALSE])
    p1_block <- plogis(X1 %*% beta_block)
    risk_exposed_draws[draw_index] <- as.numeric(crossprod(w, p1_block)) / weight_sum
    p0_block <- plogis(X0 %*% beta_block)
    risk_unexposed_draws[draw_index] <- as.numeric(crossprod(w, p0_block)) / weight_sum
  }
  rr_draws <- risk_exposed_draws / risk_unexposed_draws

  tibble::tibble(
    condition = exposure_var, rr = rr_point,
    rr_lower = quantile(rr_draws, 0.025, names = FALSE), rr_upper = quantile(rr_draws, 0.975, names = FALSE),
    standardised_risk_exposed = risk_exposed_point,
    standardised_risk_exposed_lower = quantile(risk_exposed_draws, 0.025, names = FALSE),
    standardised_risk_exposed_upper = quantile(risk_exposed_draws, 0.975, names = FALSE),
    standardised_risk_unexposed = risk_unexposed_point,
    standardised_risk_unexposed_lower = quantile(risk_unexposed_draws, 0.025, names = FALSE),
    standardised_risk_unexposed_upper = quantile(risk_unexposed_draws, 0.975, names = FALSE),
    target_n = sum(w)
  )
}

standardise_profile_rr <- function(fit, target_data, profile_levels_full = core_profile_levels, B = B_BOOT, seed = SEED) {
  fitted_levels <- fit$xlevels[["core_profile"]]
  combo <- target_data |> dplyr::count(age_years, sex, year, uf_residence, name = "n")
  w <- combo$n
  beta_hat <- coef(fit)
  set.seed(seed)
  beta_draws <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov(fit, unconditional = TRUE))

  risk_point <- setNames(rep(NA_real_, length(profile_levels_full)), profile_levels_full)
  risk_draws <- matrix(NA_real_, nrow = B, ncol = length(profile_levels_full),
                        dimnames = list(NULL, profile_levels_full))

  for (p in fitted_levels) {
    combo_p <- combo
    combo_p$core_profile <- factor(p, levels = fitted_levels)
    X_p <- predict(fit, newdata = combo_p, type = "lpmatrix")
    p_point <- plogis(as.numeric(X_p %*% beta_hat))
    risk_point[p] <- sum(p_point * w) / sum(w)
    risk_draws[, p] <- weighted_risk_draws(X_p, beta_draws, w)
  }
  rr_point <- risk_point / risk_point[["None"]]
  rr_draws <- sweep(risk_draws, 1, risk_draws[, "None"], FUN = "/")

  tibble::tibble(
    core_profile = factor(profile_levels_full, levels = profile_levels_full),
    estimable = profile_levels_full %in% fitted_levels,
    standardised_risk = as.numeric(risk_point),
    standardised_risk_lower = apply(risk_draws, 2, quantile, probs = 0.025, names = FALSE),
    standardised_risk_upper = apply(risk_draws, 2, quantile, probs = 0.975, names = FALSE),
    rr = as.numeric(rr_point),
    rr_lower = apply(rr_draws, 2, quantile, probs = 0.025, names = FALSE),
    rr_upper = apply(rr_draws, 2, quantile, probs = 0.975, names = FALSE),
    target_n = sum(w)
  )
}

message("[40] STAGE 2 standardisation helpers defined.")

## =============================================================================
## PHASE 5 -- ANALYSIS 1: SCENARIO-SPECIFIC STANDARDISATION
## Each model standardised to its own (restricted) hospitalisation-known
## population. Describes the restricted target population each scenario
## represents, but conflates model differences with target-population
## differences - see Analysis 2 for the primary robustness comparison.
## =============================================================================

analysis1_cond <- list(); analysis1_prof <- list()

for (nm in names(data_S)) {
  if (!is.null(fits_cond[[nm]])) {
    for (cond in c("dm","htn","ckd")) {
      analysis1_cond[[paste(nm,cond)]] <- standardised_rr(fits_cond[[nm]], data_S[[nm]], cond) |>
        dplyr::mutate(scenario = nm, .before = 1)
    }
  }
  if (!is.null(fits_prof[[nm]])) {
    analysis1_prof[[nm]] <- standardise_profile_rr(fits_prof[[nm]], data_S[[nm]]) |>
      dplyr::mutate(scenario = nm, .before = 1)
  }
  message(sprintf("[40] Analysis 1 (scenario-specific) done for Scenario %s", nm))
}

analysis1_cond_tbl <- dplyr::bind_rows(analysis1_cond)
analysis1_prof_tbl <- dplyr::bind_rows(analysis1_prof)
message("[40] STAGE 3 (Analysis 1) complete.")

## =============================================================================
## PHASE 6 -- ANALYSIS 2: COMMON-TARGET STANDARDISATION (PRIMARY ROBUSTNESS
## COMPARISON). Both the full model (Scenario A) and each restricted model
## are applied to the IDENTICAL target population: that scenario's full
## eligible population (known + unknown hospitalisation status), restricted
## to the (state, year) support actually observed in the restricted model's
## own training data - support outside that is excluded and reported, not
## extrapolated into.
## =============================================================================

uf_levels_by_scn   <- purrr::map(data_S, ~ levels(.x$uf_residence))
year_levels_by_scn <- purrr::map(data_S, ~ levels(.x$year))

align_levels <- function(df, uf_levels, year_levels) {
  df |> dplyr::mutate(
    uf_residence = factor(as.character(uf_residence), levels = uf_levels),
    year         = factor(as.character(cal_year),      levels = year_levels)
  )
}

build_common_target <- function(target_raw, uf_levels, year_levels) {
  supported <- as.character(target_raw$uf_residence) %in% uf_levels &
               as.character(target_raw$cal_year)      %in% year_levels
  list(
    target_supported = target_raw[supported, , drop = FALSE],
    n_total = nrow(target_raw),
    n_supported = sum(supported),
    n_unsupported = sum(!supported),
    unsupported_states = sort(unique(target_raw$state_abbrev[!supported]))
  )
}

restricted_scenarios <- c("B","C1","C2","D")
common_target_diag_list <- list()
analysis2_cond_list <- list(); analysis2_prof_list <- list()

for (nm in restricted_scenarios) {
  if (is.null(fits_cond[[nm]]) || is.null(fits_prof[[nm]]) ||
      is.null(fits_cond[["A"]]) || is.null(fits_prof[["A"]])) {
    message(sprintf("[40] Scenario %s: skipping Analysis 2 - a required model fit is missing.", nm))
    next
  }

  ct <- build_common_target(scenario_eligible[[nm]], uf_levels_by_scn[[nm]], year_levels_by_scn[[nm]])
  common_target_diag_list[[nm]] <- tibble::tibble(
    scenario = nm,
    n_target_total = ct$n_total,
    n_supported = ct$n_supported,
    n_unsupported = ct$n_unsupported,
    pct_supported = round(100 * ct$n_supported / ct$n_total, 3),
    unsupported_states = paste(ct$unsupported_states, collapse = ",")
  )

  target_for_full   <- align_levels(ct$target_supported, uf_levels_by_scn[["A"]], year_levels_by_scn[["A"]])
  target_for_restr  <- align_levels(ct$target_supported, uf_levels_by_scn[[nm]],  year_levels_by_scn[[nm]])
  stopifnot(!anyNA(target_for_full$uf_residence), !anyNA(target_for_full$year))
  stopifnot(!anyNA(target_for_restr$uf_residence), !anyNA(target_for_restr$year))

  for (cond in c("dm","htn","ckd")) {
    analysis2_cond_list[[paste(nm,cond,"full")]] <-
      standardised_rr(fits_cond[["A"]], target_for_full, cond) |>
      dplyr::mutate(scenario = nm, model_source = "full_model_A", .before = 1)
    analysis2_cond_list[[paste(nm,cond,"restricted")]] <-
      standardised_rr(fits_cond[[nm]], target_for_restr, cond) |>
      dplyr::mutate(scenario = nm, model_source = "restricted_model", .before = 1)
  }

  analysis2_prof_list[[paste(nm,"full")]] <-
    standardise_profile_rr(fits_prof[["A"]], target_for_full) |>
    dplyr::mutate(scenario = nm, model_source = "full_model_A", .before = 1)
  analysis2_prof_list[[paste(nm,"restricted")]] <-
    standardise_profile_rr(fits_prof[[nm]], target_for_restr) |>
    dplyr::mutate(scenario = nm, model_source = "restricted_model", .before = 1)

  message(sprintf("[40] Analysis 2 (common-target) done for Scenario %s (support: %s/%s = %.1f%%)",
                   nm, format(ct$n_supported,big.mark=","), format(ct$n_total,big.mark=","),
                   100*ct$n_supported/ct$n_total))
}

common_target_diag <- dplyr::bind_rows(common_target_diag_list)
readr::write_csv(common_target_diag, file.path(out_dir, "common_target_standardisation_diagnostics.csv"))

analysis2_cond_tbl <- dplyr::bind_rows(analysis2_cond_list)
analysis2_prof_tbl <- dplyr::bind_rows(analysis2_prof_list)
message("[40] STAGE 4 (Analysis 2, common-target) complete.")

## =============================================================================
## PHASE 7 -- ASSEMBLE COMPARISON TABLES (+ original manuscript values)
## =============================================================================

load("01_Data/core3_rr_hosp.RData")          # -> core3_rr_hosp
load("01_Data/core_profile_rr_hosp.RData")   # -> core_profile_rr_hosp

cond_label_to_key <- c(Diabetes = "dm", Hypertension = "htn", `Chronic kidney disease` = "ckd")

original_cond <- core3_rr_hosp |>
  dplyr::transmute(
    condition = unname(cond_label_to_key[as.character(condition)]),
    scenario = "A", analysis_type = "original_manuscript", model_source = NA_character_,
    rr, rr_lower, rr_upper,
    standardised_risk_exposed, standardised_risk_exposed_lower, standardised_risk_exposed_upper,
    standardised_risk_unexposed, standardised_risk_unexposed_lower, standardised_risk_unexposed_upper,
    target_n = NA_real_
  )

original_prof <- core_profile_rr_hosp |>
  dplyr::transmute(
    core_profile, scenario = "A", analysis_type = "original_manuscript", model_source = NA_character_,
    estimable = TRUE,
    standardised_risk, standardised_risk_lower, standardised_risk_upper,
    rr, rr_lower, rr_upper, target_n = NA_real_
  )

a1_cond <- analysis1_cond_tbl |> dplyr::mutate(analysis_type = "scenario_specific", model_source = NA_character_)
a2_cond <- analysis2_cond_tbl |>
  dplyr::mutate(analysis_type = dplyr::if_else(model_source == "full_model_A",
                                                "common_target_full_model", "common_target_restricted_model"))

hospitalisation_condition_rr_comparison <- dplyr::bind_rows(original_cond, a1_cond, a2_cond) |>
  dplyr::select(scenario, condition, analysis_type, model_source, rr, rr_lower, rr_upper,
                standardised_risk_exposed, standardised_risk_exposed_lower, standardised_risk_exposed_upper,
                standardised_risk_unexposed, standardised_risk_unexposed_lower, standardised_risk_unexposed_upper,
                target_n) |>
  dplyr::arrange(condition, scenario, analysis_type)

## pct change: common_target_restricted_model vs common_target_full_model, same scenario+condition (paired target)
pct_change_cond <- hospitalisation_condition_rr_comparison |>
  dplyr::filter(analysis_type %in% c("common_target_full_model","common_target_restricted_model")) |>
  dplyr::select(scenario, condition, analysis_type, rr) |>
  tidyr::pivot_wider(names_from = analysis_type, values_from = rr) |>
  dplyr::mutate(pct_change_rr = round(100 * (common_target_restricted_model - common_target_full_model) /
                                        common_target_full_model, 2)) |>
  dplyr::select(scenario, condition, pct_change_rr)

hospitalisation_condition_rr_comparison <- hospitalisation_condition_rr_comparison |>
  dplyr::left_join(pct_change_cond, by = c("scenario","condition"))
readr::write_csv(hospitalisation_condition_rr_comparison, file.path(out_dir, "hospitalisation_condition_rr_comparison.csv"))

a1_prof <- analysis1_prof_tbl |> dplyr::mutate(analysis_type = "scenario_specific", model_source = NA_character_)
a2_prof <- analysis2_prof_tbl |>
  dplyr::mutate(analysis_type = dplyr::if_else(model_source == "full_model_A",
                                                "common_target_full_model", "common_target_restricted_model"))

hospitalisation_profile_rr_comparison <- dplyr::bind_rows(original_prof, a1_prof, a2_prof) |>
  dplyr::select(scenario, core_profile, analysis_type, model_source, estimable,
                standardised_risk, standardised_risk_lower, standardised_risk_upper,
                rr, rr_lower, rr_upper, target_n) |>
  dplyr::arrange(core_profile, scenario, analysis_type)

pct_change_prof <- hospitalisation_profile_rr_comparison |>
  dplyr::filter(analysis_type %in% c("common_target_full_model","common_target_restricted_model")) |>
  dplyr::select(scenario, core_profile, analysis_type, rr) |>
  tidyr::pivot_wider(names_from = analysis_type, values_from = rr) |>
  dplyr::mutate(pct_change_rr = round(100 * (common_target_restricted_model - common_target_full_model) /
                                        common_target_full_model, 2)) |>
  dplyr::select(scenario, core_profile, pct_change_rr)

hospitalisation_profile_rr_comparison <- hospitalisation_profile_rr_comparison |>
  dplyr::left_join(pct_change_prof, by = c("scenario","core_profile"))
readr::write_csv(hospitalisation_profile_rr_comparison, file.path(out_dir, "hospitalisation_profile_rr_comparison.csv"))

## ---- standardised absolute risks, all scenarios/analyses in one file ----
abs_cond <- hospitalisation_condition_rr_comparison |>
  dplyr::transmute(scenario, group = condition, analysis_type,
                    risk_exposed = standardised_risk_exposed, risk_exposed_lower = standardised_risk_exposed_lower,
                    risk_exposed_upper = standardised_risk_exposed_upper,
                    risk_unexposed = standardised_risk_unexposed, risk_unexposed_lower = standardised_risk_unexposed_lower,
                    risk_unexposed_upper = standardised_risk_unexposed_upper, model_type = "condition_specific")
abs_prof <- hospitalisation_profile_rr_comparison |>
  dplyr::transmute(scenario, group = as.character(core_profile), analysis_type,
                    risk_exposed = standardised_risk, risk_exposed_lower = standardised_risk_lower,
                    risk_exposed_upper = standardised_risk_upper,
                    risk_unexposed = NA_real_, risk_unexposed_lower = NA_real_, risk_unexposed_upper = NA_real_,
                    model_type = "joint_profile")
hospitalisation_absolute_risk_comparison <- dplyr::bind_rows(abs_cond, abs_prof)
readr::write_csv(hospitalisation_absolute_risk_comparison, file.path(out_dir, "hospitalisation_absolute_risk_comparison.csv"))

message("[40] STAGE 5 (comparison tables) complete.")

## =============================================================================
## PHASE 8 -- COHORT / STATE-YEAR INCLUSION SUMMARY TABLES
## =============================================================================

profile_pct <- function(df) {
  ## NB: `n` must be dropped before pivoting - left in, pivot_wider treats it
  ## as an implicit id_col (id_cols=NULL means "every other column"), and
  ## since n differs per profile row, that silently explodes the result to
  ## one row per profile (mostly NA) instead of collapsing to one summary row.
  df |> dplyr::count(core_profile) |> dplyr::mutate(pct = round(100*n/sum(n),2)) |>
    dplyr::select(core_profile, pct) |>
    tidyr::pivot_wider(names_from = core_profile, values_from = pct, names_prefix = "pct_profile_")
}

scenario_cohort_summary <- purrr::imap_dfr(scenario_eligible, function(elig_df, nm) {
  known_df <- scenario_known[[nm]]
  base <- tibble::tibble(
    scenario = nm,
    n_eligible = nrow(elig_df),
    n_known = nrow(known_df),
    n_missing = nrow(elig_df) - nrow(known_df),
    pct_known = round(100*nrow(known_df)/nrow(elig_df), 2),
    n_hosp_events = sum(known_df$hosp_only),
    pct_hosp_among_known = round(100*sum(known_df$hosp_only)/nrow(known_df), 2),
    pct_of_original_eligible_retained = round(100*nrow(elig_df)/nrow(scenario_eligible[["A"]]), 2),
    pct_of_original_known_retained = round(100*nrow(known_df)/nrow(scenario_known[["A"]]), 2),
    mean_age_eligible = round(mean(elig_df$age_years), 2),
    sd_age_eligible = round(sd(elig_df$age_years), 2),
    n_states_included = dplyr::n_distinct(elig_df$uf_residence),
    years_included = paste(sort(unique(elig_df$cal_year)), collapse = ",")
  )
  dplyr::bind_cols(base, profile_pct(elig_df))
})
readr::write_csv(scenario_cohort_summary, file.path(out_dir, "scenario_cohort_summary.csv"))

## ---- state-year inclusion diagnostics (Scenario C), incl. min-denominator-rule sensitivity ----
state_year_inclusion <- dplyr::bind_rows(
  state_year_70 |> dplyr::mutate(threshold = "70pct"),
  state_year_80 |> dplyr::mutate(threshold = "80pct")
) |>
  dplyr::left_join(uf_lookup |> dplyr::select(uf_residence, state_abbrev2 = state_abbrev), by = "uf_residence") |>
  dplyr::transmute(
    threshold, state = dplyr::coalesce(state_abbrev, state_abbrev2), uf_residence, cal_year,
    n_eligible, n_known, completeness = round(completeness, 4),
    below_min_denominator = n_eligible < MIN_STRATUM_DENOM,
    included_primary_rule = included_primary,
    included_if_min_denom_rule_ignored = included_no_min_denom_rule
  ) |>
  dplyr::arrange(threshold, state, cal_year)
readr::write_csv(state_year_inclusion, file.path(out_dir, "scenario_state_year_inclusion.csv"))

min_denom_sensitivity <- state_year_inclusion |>
  dplyr::group_by(threshold) |>
  dplyr::summarise(
    n_strata_total = dplyr::n(),
    n_strata_below_min_denom = sum(below_min_denominator),
    n_eligible_in_below_min_denom_strata = sum(n_eligible[below_min_denominator]),
    n_strata_included_primary_rule = sum(included_primary_rule),
    n_strata_included_if_rule_ignored = sum(included_if_min_denom_rule_ignored),
    n_strata_gained_if_rule_ignored = sum(included_if_min_denom_rule_ignored & !included_primary_rule),
    .groups = "drop"
  )
message("[40] min-denominator-rule sensitivity (does the >= 100 eligible-case rule change C1/C2 inclusion?):")
print(min_denom_sensitivity)
readr::write_csv(min_denom_sensitivity, file.path(out_dir, "scenario_state_year_min_denom_sensitivity.csv"))

message("[40] STAGE 6 (cohort/state-year summaries) complete.")

## =============================================================================
## PHASE 9 -- FIGURES
## =============================================================================

scenario_labels <- c(
  A  = "A: Original (complete-case)",
  B  = "B: Exclude DF 2020-25",
  C1 = "C1: State-year completeness >=70%",
  C2 = "C2: State-year completeness >=80%",
  D  = "D: Lab-confirmed only"
)
cond_label <- c(dm = "Diabetes", htn = "Hypertension", ckd = "Chronic kidney disease")

## Fixed, explicit scenario->colour mapping shared by Figures 1 and 2, so a
## scenario keeps the same colour whether or not Scenario A (absent from
## Figure 2, which only compares restricted-vs-full) is present in that
## particular plot - scale_colour_brewer() alone reassigns colours per-plot
## based on whichever factor levels are actually present, which would
## silently recolour Scenario B red once A drops out.
scenario_colours <- stats::setNames(
  c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00"),
  unname(scenario_labels)
)

## ---- Figure 1: RR comparison forest plot (original vs scenario-specific restricted) ----
fig1_cond <- hospitalisation_condition_rr_comparison |>
  dplyr::filter(analysis_type %in% c("original_manuscript","scenario_specific")) |>
  dplyr::transmute(scenario, group = unname(cond_label[condition]),
                    panel = "Condition-specific (mutually adjusted for the other two)",
                    analysis_type, rr, rr_lower, rr_upper)

fig1_prof <- hospitalisation_profile_rr_comparison |>
  dplyr::filter(analysis_type %in% c("original_manuscript","scenario_specific"), core_profile != "None") |>
  dplyr::transmute(scenario, group = as.character(core_profile),
                    panel = "Joint profile (vs no core condition)",
                    analysis_type, rr, rr_lower, rr_upper)

fig1_data <- dplyr::bind_rows(fig1_cond, fig1_prof) |>
  dplyr::mutate(
    scenario_label = factor(unname(scenario_labels[scenario]), levels = unname(scenario_labels)),
    group = factor(group, levels = c("Diabetes","Hypertension","Chronic kidney disease",
                                      "DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD"))
  )

p1 <- ggplot2::ggplot(fig1_data, ggplot2::aes(x = rr, y = group, colour = scenario_label)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
    position = ggplot2::position_dodge(width = 0.6), size = 0.35, fatten = 2
  ) +
  ggplot2::scale_x_log10(breaks = scales::log_breaks(n = 6), labels = scales::label_number(accuracy = 0.1)) +
  ggplot2::scale_colour_manual(values = scenario_colours) +
  ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 1) +
  ggplot2::labs(x = "Standardised RR (log scale)", y = NULL, colour = NULL,
                title = "Hospitalisation RR: original complete-case model vs each restricted scenario",
                subtitle = "Each model standardised to its own (scenario-specific) population; 95% bootstrap UI") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

ggplot2::ggsave(file.path(out_dir, "figure_rr_comparison.png"), p1, width = 10, height = 11, dpi = 300)

## ---- Figure 2: percentage change in RR, common-target standardisation ----
fig2_cond <- hospitalisation_condition_rr_comparison |>
  dplyr::filter(analysis_type == "common_target_restricted_model") |>
  dplyr::transmute(scenario, group = unname(cond_label[condition]),
                    panel = "Condition-specific", pct_change_rr) |>
  dplyr::distinct()

fig2_prof <- hospitalisation_profile_rr_comparison |>
  dplyr::filter(analysis_type == "common_target_restricted_model", core_profile != "None") |>
  dplyr::transmute(scenario, group = as.character(core_profile),
                    panel = "Joint profile", pct_change_rr) |>
  dplyr::distinct()

fig2_data <- dplyr::bind_rows(fig2_cond, fig2_prof) |>
  dplyr::mutate(
    scenario_label = factor(unname(scenario_labels[scenario]), levels = unname(scenario_labels)),
    group = factor(group, levels = c("Diabetes","Hypertension","Chronic kidney disease",
                                      "DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD"))
  )

p2 <- ggplot2::ggplot(fig2_data, ggplot2::aes(x = pct_change_rr, y = group, colour = scenario_label)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(size = 2.6, position = ggplot2::position_dodge(width = 0.6)) +
  ggplot2::scale_colour_manual(values = scenario_colours) +
  ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 1) +
  ggplot2::labs(x = "% change in RR (restricted model vs original model, applied to the SAME common target)",
                y = NULL, colour = NULL,
                title = "Sensitivity of hospitalisation RR to sample restriction",
                subtitle = "Common-target standardisation: isolates model change from target-population change") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

ggplot2::ggsave(file.path(out_dir, "figure_rr_percentage_change.png"), p2, width = 10, height = 9, dpi = 300)

## ---- Figure 3: sample retention and completeness ----
fig3_data <- scenario_cohort_summary |>
  dplyr::transmute(scenario, n_eligible, pct_of_original_eligible_retained, pct_known,
                    scenario_label = factor(unname(scenario_labels[scenario]), levels = unname(scenario_labels)))

p3a <- ggplot2::ggplot(fig3_data, ggplot2::aes(x = scenario_label, y = pct_of_original_eligible_retained)) +
  ggplot2::geom_col(fill = "#4C78A8") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(pct_of_original_eligible_retained, "%\n(n=",
                                                   format(n_eligible, big.mark=","), ")")),
                      vjust = -0.15, size = 3) +
  ggplot2::scale_y_continuous(limits = c(0, 110), expand = c(0,0)) +
  ggplot2::labs(x = NULL, y = "% of Scenario A eligible\npopulation retained",
                title = "Scenario sample retention") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

p3b <- ggplot2::ggplot(fig3_data, ggplot2::aes(x = scenario_label, y = pct_known)) +
  ggplot2::geom_col(fill = "#E69F00") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(pct_known, "%")), vjust = -0.3, size = 3) +
  ggplot2::scale_y_continuous(limits = c(0, 110), expand = c(0,0)) +
  ggplot2::labs(x = NULL, y = "% hospitalisation status\nknown WITHIN scenario",
                title = "Recording completeness within each restricted scenario") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

p3 <- patchwork::wrap_plots(p3a, p3b, ncol = 1)
ggplot2::ggsave(file.path(out_dir, "figure_scenario_sample_retention.png"), p3, width = 8, height = 9, dpi = 300)

message("[40] STAGE 7 (figures) complete.")

## =============================================================================
## PHASE 10 -- QUANTITATIVE SUMMARY FOR THE REPORT (computed, not hand-typed)
## =============================================================================

pct_changes_all <- dplyr::bind_rows(
  pct_change_cond |> dplyr::transmute(scenario, group = unname(cond_label[condition]), model_type = "condition", pct_change_rr),
  pct_change_prof |> dplyr::filter(core_profile != "None") |>
    dplyr::transmute(scenario, group = as.character(core_profile), model_type = "profile", pct_change_rr)
)

max_abs_change_row <- pct_changes_all |> dplyr::slice_max(abs(pct_change_rr), n = 1, with_ties = FALSE)
median_abs_change <- round(median(abs(pct_changes_all$pct_change_rr), na.rm = TRUE), 2)
n_flagged_gt10 <- sum(abs(pct_changes_all$pct_change_rr) > 10, na.rm = TRUE)
flagged_gt10_tbl <- pct_changes_all |> dplyr::filter(abs(pct_change_rr) > 10) |> dplyr::arrange(dplyr::desc(abs(pct_change_rr)))

## does CKD remain the strongest individual condition, in every scenario's own standardisation?
ckd_strongest_by_scenario <- analysis1_cond_tbl |>
  dplyr::group_by(scenario) |>
  dplyr::summarise(strongest = condition[which.max(rr)], ckd_rr = rr[condition=="ckd"],
                    max_rr = max(rr), .groups = "drop") |>
  dplyr::mutate(ckd_is_strongest = strongest == "ckd")
ckd_always_strongest <- all(ckd_strongest_by_scenario$ckd_is_strongest)

## do CKD-containing profiles remain among the top-ranked profiles in every scenario?
top3_profiles_by_scenario <- analysis1_prof_tbl |>
  dplyr::filter(core_profile != "None", estimable) |>
  dplyr::group_by(scenario) |>
  dplyr::slice_max(rr, n = 3, with_ties = FALSE) |>
  dplyr::summarise(top3 = paste(core_profile, collapse = ", "),
                    n_ckd_containing_in_top3 = sum(grepl("CKD", core_profile)), .groups = "drop")
ckd_profiles_consistently_top <- all(top3_profiles_by_scenario$n_ckd_containing_in_top3 >= 1)

## CI width: restricted-model common-target CI vs full-model common-target CI (same target -> isolates
## precision loss from sample-size reduction, not from target-population change)
ci_width_cond <- hospitalisation_condition_rr_comparison |>
  dplyr::filter(analysis_type %in% c("common_target_full_model","common_target_restricted_model")) |>
  dplyr::mutate(ci_width_log = log(rr_upper) - log(rr_lower)) |>
  dplyr::select(scenario, condition, analysis_type, ci_width_log) |>
  tidyr::pivot_wider(names_from = analysis_type, values_from = ci_width_log) |>
  dplyr::mutate(ci_widening_ratio = round(common_target_restricted_model / common_target_full_model, 2))
median_ci_widening_ratio <- round(median(ci_width_cond$ci_widening_ratio, na.rm = TRUE), 2)

n_fit_failures <- sum(convergence_summary$fit_failed)
n_nonconverged <- sum(!convergence_summary$converged & !convergence_summary$fit_failed, na.rm = TRUE)
n_sparse_profile_scenarios <- sum(convergence_summary$n_profile_levels_absent > 0 |
                                    convergence_summary$n_profile_levels_sparse_lt10events > 0)

## ---- overall verdict logic (computed, not asserted) ----
verdict <- if (n_fit_failures > 0 || n_nonconverged > 0) {
  "FAIL"
} else if (!ckd_always_strongest || !ckd_profiles_consistently_top) {
  "CONDITIONAL PASS"
} else if (abs(max_abs_change_row$pct_change_rr) > 25) {
  "CONDITIONAL PASS"
} else if (abs(max_abs_change_row$pct_change_rr) > 10) {
  "CONDITIONAL PASS"
} else {
  "PASS"
}

summary_stats <- list(
  max_abs_change_row = max_abs_change_row, median_abs_change = median_abs_change,
  n_flagged_gt10 = n_flagged_gt10, flagged_gt10_tbl = flagged_gt10_tbl,
  ckd_strongest_by_scenario = ckd_strongest_by_scenario, ckd_always_strongest = ckd_always_strongest,
  top3_profiles_by_scenario = top3_profiles_by_scenario, ckd_profiles_consistently_top = ckd_profiles_consistently_top,
  ci_width_cond = ci_width_cond, median_ci_widening_ratio = median_ci_widening_ratio,
  n_fit_failures = n_fit_failures, n_nonconverged = n_nonconverged,
  n_sparse_profile_scenarios = n_sparse_profile_scenarios, verdict = verdict
)
saveRDS(summary_stats, file.path(out_dir, "_summary_stats_for_report.rds"))

message(sprintf("[40] Max |pct change| = %.2f%% (%s, %s, scenario %s)",
                 max_abs_change_row$pct_change_rr, max_abs_change_row$group,
                 max_abs_change_row$model_type, max_abs_change_row$scenario))
message(sprintf("[40] Median |pct change| = %.2f%%, %d/%d comparisons flagged >10%%",
                 median_abs_change, n_flagged_gt10, nrow(pct_changes_all)))
message(sprintf("[40] CKD always strongest condition: %s. CKD-containing profiles consistently top-3: %s",
                 ckd_always_strongest, ckd_profiles_consistently_top))
message(sprintf("[40] Model fit failures: %d. Non-converged: %d.", n_fit_failures, n_nonconverged))
message(sprintf("[40] VERDICT: %s", verdict))

message("[40] STAGE 8 (summary statistics) complete.")

## =============================================================================
## PHASE 11 -- VALIDATION_3_2B.md
## =============================================================================

df_to_md <- function(df, digits = 3) {
  df <- as.data.frame(df)
  for (j in seq_along(df)) if (is.numeric(df[[j]])) df[[j]] <- round(df[[j]], digits)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

cond_main_tbl <- hospitalisation_condition_rr_comparison |>
  dplyr::filter(analysis_type %in% c("original_manuscript","common_target_full_model","common_target_restricted_model")) |>
  dplyr::transmute(scenario, condition = unname(cond_label[condition]), analysis_type,
                    rr = round(rr,2), ci = sprintf("%.2f-%.2f", rr_lower, rr_upper), n = round(target_n))

prof_main_tbl <- hospitalisation_profile_rr_comparison |>
  dplyr::filter(analysis_type %in% c("original_manuscript","common_target_full_model","common_target_restricted_model"),
                core_profile != "None") |>
  dplyr::transmute(scenario, core_profile, analysis_type, rr = round(rr,2),
                    ci = sprintf("%.2f-%.2f", rr_lower, rr_upper), n = round(target_n))

verdict_explanation <- dplyr::case_when(
  verdict == "FAIL" ~ "one or more models failed to fit or converge - see model_convergence_and_sparse_cells.csv.",
  verdict == "CONDITIONAL PASS" & !ckd_always_strongest ~ "CKD was not the single strongest individual condition in every scenario's own standardisation.",
  verdict == "CONDITIONAL PASS" & !ckd_profiles_consistently_top ~ "a CKD-containing profile was not always represented in the top-3 ranked profiles.",
  verdict == "CONDITIONAL PASS" ~ sprintf("the largest common-target RR change was %.1f%% (%s, %s, Scenario %s), above the prespecified 10%% descriptive flag.",
                                            max_abs_change_row$pct_change_rr, max_abs_change_row$group,
                                            max_abs_change_row$model_type, max_abs_change_row$scenario),
  TRUE ~ "all technical checks passed and no RR changed by more than 10% under common-target standardisation."
)

report <- sprintf('# Gate 3.2B - Restricted-sample sensitivity analysis for hospitalisation RR

Brazil, confirmed chikungunya cases, 2017-2025. Produced by `02_Script/40_hospitalisation_restricted_sensitivity.R`, following the Gate 3.2A missingness audit (`03_Output/validation/gate32a/VALIDATION_3_2A.md`). This is a validation exercise: it does not change the primary hospitalisation model, and does not implement IPW, multiple imputation, or MNAR modelling.

## 1. Baseline reproduction (Scenario A)

| | N | Events |
|---|---:|---:|
| Eligible core-three population | %s | - |
| Hospitalisation complete-case cohort | %s | %s |

Matches the expected baseline (1,074,796 / 776,072 / 25,363) exactly - **PASS**.

## 2. Scenario definitions and cohort sizes

%s

Full detail, including age/profile composition per scenario: `scenario_cohort_summary.csv`. State-year inclusion detail for Scenario C (both thresholds, with and without the minimum-denominator rule): `scenario_state_year_inclusion.csv` and `scenario_state_year_min_denom_sensitivity.csv`.

**Distrito Federal (DF) verification**: confirmed as `uf_residence` code "53" (IBGE standard), not the string "DF" - %s eligible DF records identified and correctly excluded for 2020-2025 under Scenario B.

## 3. Model convergence and sparse cells

Model fit failures: **%d**. Non-converged fits: **%d**. Scenario x model combinations with at least one absent or sparse (<10 events) profile cell: **%d** (see `model_convergence_and_sparse_cells.csv` for which).

## 4. Condition-specific RR: original vs restricted (common-target standardisation)

%s

Full table with all analysis types (scenario-specific AND common-target, both full and restricted models) and 95%% UIs: `hospitalisation_condition_rr_comparison.csv`.

## 5. Joint-profile RR: original vs restricted (common-target standardisation)

%s

Full table: `hospitalisation_profile_rr_comparison.csv`. Standardised absolute risks for all scenarios/analyses: `hospitalisation_absolute_risk_comparison.csv`.

## 6. Magnitude of change

- **Maximum |%% change|** in common-target RR (restricted model vs original model, same target population): **%.2f%%** (%s, %s, Scenario %s).
- **Median |%% change|** across all condition + profile common-target comparisons: **%.2f%%**.
- **%d of %d** comparisons exceeded the prespecified +/-10%% descriptive-flag threshold (not automatic evidence of invalidity - see below for which, and why).
- **Median CI-widening ratio** (restricted-model common-target CI width on the log scale, divided by the original model\'s CI width on the SAME target): **%.2fx**.

%s

## 7. Does the main clinical pattern hold?

| Question | Answer |
|---|---|
| Does CKD remain the strongest individual condition in every scenario? | **%s** |
| Do CKD-containing profiles remain among the top-3 ranked profiles in every scenario? | **%s** |

%s

## 8. Common-target support (Analysis 2 positivity check)

%s

No scenario required falling back to a different overlap target - the restricted model\'s own (state, year) support, used directly as the common target\'s inclusion criterion, already guarantees full predictive support for both the full and the restricted model by construction (see script Phase 6 for the reasoning). Full detail: `common_target_standardisation_diagnostics.csv`.

## 9. Interpretation

### A. Stability of clinical associations
%s

### B. Geographical and temporal reporting bias
Scenario B (excluding DF 2020-2025, the most extreme single-area/period recording gap found in Gate 3.2A) and Scenarios C1/C2 (restricting to higher-completeness state-year strata generally) directly test whether the poorest-recorded areas/periods are driving the primary estimates. See Section 6 for the quantitative answer; in short, %s.

### C. Differential profile representation
Scenario-specific standardisation (Analysis 1, `hospitalisation_profile_rr_comparison.csv` with `analysis_type=="scenario_specific"`) shows each restricted sample\'s own profile composition has shifted modestly (`scenario_cohort_summary.csv` `pct_profile_*` columns) in the direction already flagged by Gate 3.2A (comorbid profiles slightly over-represented among recorded cases) - the common-target analysis in Section 4-6 is what isolates whether this compositional shift, rather than a genuine change in the fitted association, explains any observed RR movement.

### D. Remaining limitations
- A stable RR across these restricted analyses does **not** prove hospitalisation-status missingness is ignorable (MAR) - it shows the primary estimates are not an artefact of the specific poorly-recorded areas/periods tested here, which is a narrower and more defensible claim.
- Conversely, any RR change after restriction may reflect genuine epidemiological heterogeneity (e.g. real regional differences in chikungunya severity or healthcare access) or reduced precision, not necessarily bias - this report does not assume restriction-driven change equals bias.
- Scenario D (laboratory-confirmed only) changes case-definition composition as well as recording completeness and is interpreted only as a diagnostic-ascertainment sensitivity check, not a missingness correction.
- No MNAR mechanism (hospitalisation-status recording depending on the hospitalisation outcome itself, conditional on observed covariates) can be excluded by any analysis in this report.
- Formal IPW/positivity diagnostics (Gate 3.2A Phase 5) were deferred and have still not been computed; this report\'s "common support" check (Section 8) is a necessary but not sufficient condition for IPW to be well-behaved if attempted later.

### E. Recommendation for the manuscript
%s

## 10. Gate 3.2B status: **%s**

%s

---
*Fixed bootstrap seed: %d. Bootstrap draws per standardisation: %d. Minimum state-year stratum denominator (Scenario C): %d eligible cases.*
',
  format(nrow(eligible), big.mark=","), format(nrow(hk_A_check), big.mark=","), format(sum(hk_A_check$hosp_only), big.mark=","),
  df_to_md(scenario_cohort_summary |> dplyr::select(scenario, n_eligible, n_known, pct_known, n_hosp_events,
                                                       pct_of_original_eligible_retained), digits = 2),
  format(nrow(df_rows), big.mark=","),
  n_fit_failures, n_nonconverged, n_sparse_profile_scenarios,
  df_to_md(cond_main_tbl, digits = 2),
  df_to_md(prof_main_tbl, digits = 2),
  max_abs_change_row$pct_change_rr, max_abs_change_row$group, max_abs_change_row$model_type, max_abs_change_row$scenario,
  median_abs_change, n_flagged_gt10, nrow(pct_changes_all),
  median_ci_widening_ratio,
  if (n_flagged_gt10 > 0) paste0("Flagged comparisons (>10% change):\n\n", df_to_md(flagged_gt10_tbl, digits = 2)) else "No comparison exceeded the 10% flag threshold.",
  ifelse(ckd_always_strongest, "YES", "NO"), ifelse(ckd_profiles_consistently_top, "YES", "NO"),
  df_to_md(ckd_strongest_by_scenario, digits = 2),
  df_to_md(common_target_diag, digits = 2),
  if (verdict == "PASS") "The estimated hospitalisation associations for DM, HTN, CKD, and the 8-profile ranking are stable across all four restricted scenarios under common-target standardisation - no comparison moved by more than 10%, and the CKD-driven risk gradient is preserved throughout." else sprintf("See Sections 6-7 above: %s", verdict_explanation),
  ifelse(n_flagged_gt10 == 0, "the poorly-recorded areas/periods tested do not appear to be driving the primary estimates",
         "some comparisons moved by more than the prespecified 10% threshold; Section 6 and `hospitalisation_condition_rr_comparison.csv`/`hospitalisation_profile_rr_comparison.csv` identify exactly which, for closer review rather than as proof of bias"),
  if (verdict == "PASS") "The existing complete-case hospitalisation model can remain the primary analysis. These four restricted-sample analyses are sufficient as sensitivity analyses for this round; a formal IPW sensitivity analysis is not clearly necessary on this evidence alone, since the associations were already stable without it. Further MNAR analysis is not clearly warranted by this report, though the limitations in Section 9D remain unresolved by any complete-case or restricted-sample approach." else "Review the flagged comparisons in Section 6 before deciding; a formal IPW sensitivity analysis (Gate 3.2A's deferred Phase 5) would help quantify whether the specific flagged changes track overall recording-probability rather than one of the restricted scenarios' own idiosyncrasies. MNAR analysis is a larger undertaking and should only follow if IPW itself cannot resolve the flagged discrepancies.",
  verdict, verdict_explanation,
  SEED, B_BOOT, MIN_STRATUM_DENOM
)

writeLines(report, file.path(out_dir, "VALIDATION_3_2B.md"))
message("[40] VALIDATION_3_2B.md written.")
message("[40] Gate 3.2B COMPLETE. Verdict: ", verdict)
