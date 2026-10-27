# ---------------------------------------------------------------------------
# primary_models_functions.R
#
# Shared helpers for the harmonised primary-model pipeline (scripts 62-65):
#   hospitalisation_H2  y ~ profile + sex + year + s(state, re) + s(age, k=10, ts)
#                           + s(age, profile, bs="fs", k=10, m=1)
#   mortality_H0        y ~ profile + sex + year + s(state, re) + s(age, k=10, ts)
# Cohort rules are those of script 12 (as copied in scripts 46, 52, 56, 60).
# Sourced, not run on its own.
# ---------------------------------------------------------------------------

pm_dir <- "03_Output/primary_models_H2_H0_final"

core_profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD",
                         "HTN+CKD", "DM+HTN+CKD")
age_breaks <- c(0, 15, 40, 60, 75, 101)
age_levels <- c("0-14", "15-39", "40-59", "60-74", "75+")
cut_age <- function(a) cut(a, age_breaks, labels = age_levels, right = FALSE,
                           include.lowest = TRUE)

expected_counts <- list(eligible = 1074796, hosp_n = 776072, hosp_events = 25363,
                        death_n = 983947, death_events = 1050)

primary_models <- list(
  hospitalisation_H2 = list(
    outcome = "hosp", outcome_label = "Hospitalisation",
    formula = paste("y ~ core_profile + sex + year + s(uf_residence, bs = 're') +",
                    "s(age_years, k = 10, bs = 'ts') +",
                    "s(age_years, core_profile, bs = 'fs', k = 10, m = 1)"),
    seed = 6201),
  mortality_H0 = list(
    outcome = "death", outcome_label = "Chikungunya-attributed death",
    formula = paste("y ~ core_profile + sex + year + s(uf_residence, bs = 're') +",
                    "s(age_years, k = 10, bs = 'ts')"),
    seed = 6202)
)

contrast_defs <- data.frame(
  group = rep(c("One condition", "Two conditions"), each = 3),
  profile_a = c("DM", "CKD", "CKD", "DM+CKD", "HTN+CKD", "HTN+CKD"),
  profile_b = c("HTN", "HTN", "DM", "DM+HTN", "DM+HTN", "DM+CKD"),
  stringsAsFactors = FALSE)

pm_log <- function(tag, ...) message(sprintf("[%s %s] ", tag, format(Sys.time(), "%H:%M:%S")), sprintf(...))

## ---- Cohorts -----------------------------------------------------------------
pm_build_cohorts <- function(path = "01_Data/chik_sinan_individual_2015_2025.rds") {
  ind <- readRDS(path)
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
      age_group = cut_age(age_years)
    ) |>
    dplyr::select(age_years, age_group, sex, year, uf_residence, dm, htn, ckd, core_profile,
                  hospitalised, died_from_chik, evolution)
  rm(ind); invisible(gc())
  keep <- c("y", "age_years", "age_group", "sex", "year", "uf_residence", "core_profile")
  hosp <- eligible |> dplyr::filter(hospitalised %in% c("no", "yes")) |>
    dplyr::mutate(y = as.integer(hospitalised == "yes")) |> dplyr::select(dplyr::all_of(keep))
  death <- eligible |> dplyr::filter(!is.na(died_from_chik)) |>
    dplyr::mutate(y = as.integer(died_from_chik)) |> dplyr::select(dplyr::all_of(keep))
  list(eligible = eligible, hosp = hosp, death = death)
}

## ---- Model fitting -----------------------------------------------------------------
pm_fit <- function(formula, data, n_threads, sp = NULL) {
  warns <- character()
  t0 <- Sys.time()
  fit <- withCallingHandlers(
    mgcv::bam(stats::as.formula(formula), data = data, family = binomial(link = "logit"),
              method = "fREML", discrete = TRUE, nthreads = n_threads, sp = sp),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  fit$audit_warnings <- warns
  fit$audit_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  fit
}

pm_draw_coefs <- function(fit, B, seed) {
  set.seed(seed)
  V <- stats::vcov(fit, unconditional = TRUE)
  MASS::mvrnorm(B, coef(fit), V)
}

## ---- Standardisation -------------------------------------------------------------
## Target: unique (exact age, sex, year, state) combinations of `target`,
## frequency-weighted; the same target for every profile. For each profile the
## full linear predictor (all smooths, incl. profile-specific age deviations) is
## evaluated with predict(type = "lpmatrix"). Overall and age-group risks come
## from the same per-combination probabilities and the same coefficient draws.
pm_standardise <- function(fit, target, beta_draws, profiles = core_profile_levels,
                           groups = age_levels, block = 250) {
  combo <- dplyr::count(target, age_years, sex, year, uf_residence, name = "n")
  combo$age_group <- factor(as.character(cut_age(combo$age_years)), levels = groups)
  stopifnot(!anyNA(combo$age_group))
  w <- combo$n; N <- sum(w); Na <- as.numeric(tapply(w, combo$age_group, sum))
  beta <- coef(fit); B <- nrow(beta_draws)
  np <- length(profiles); ng <- length(groups)
  pt_all <- setNames(numeric(np), profiles)
  pt_age <- matrix(NA_real_, ng, np, dimnames = list(groups, profiles))
  dr_all <- matrix(NA_real_, B, np, dimnames = list(NULL, profiles))
  dr_age <- array(NA_real_, c(B, ng, np), dimnames = list(NULL, groups, profiles))
  for (p in profiles) {
    nd <- combo
    nd$core_profile <- factor(p, levels = core_profile_levels)
    nd$uf_residence <- factor(as.character(nd$uf_residence), levels = levels(fit$model$uf_residence))
    nd$year <- factor(as.character(nd$year), levels = levels(fit$model$year))
    X <- stats::predict(fit, newdata = nd, type = "lpmatrix")
    pr <- stats::plogis(as.numeric(X %*% beta))
    pt_all[p] <- sum(pr * w) / N
    pt_age[, p] <- as.numeric(rowsum(pr * w, combo$age_group, reorder = TRUE)) / Na
    for (i in seq.int(1, B, by = block)) {
      idx <- i:min(i + block - 1, B)
      W <- stats::plogis(X %*% t(beta_draws[idx, , drop = FALSE])) * w
      dr_all[idx, p] <- colSums(W) / N
      dr_age[idx, , p] <- t(rowsum(W, combo$age_group, reorder = TRUE) / Na)
    }
  }
  list(point = pt_all, draws = dr_all, point_age = pt_age, draws_age = dr_age,
       n_target = N, n_age = setNames(Na, groups), n_combos = nrow(combo))
}

## Individual-level (unweighted) point standardisation, chunked, as a check
## that the frequency-weighted implementation is numerically equivalent.
pm_individual_point <- function(fit, target, profiles = core_profile_levels, chunk = 200000) {
  sapply(profiles, function(p) {
    tot <- 0
    for (i in seq.int(1, nrow(target), by = chunk)) {
      nd <- target[i:min(i + chunk - 1, nrow(target)), c("age_years", "sex", "year", "uf_residence")]
      nd$core_profile <- factor(p, levels = core_profile_levels)
      tot <- tot + sum(stats::predict(fit, newdata = nd, type = "response"))
    }
    tot / nrow(target)
  })
}

qq <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE))

## ---- Summaries -------------------------------------------------------------------------
pm_overall_table <- function(s, model_id, outcome, support) {
  pr <- names(s$point)
  dplyr::tibble(
    outcome = outcome, model_id = model_id, core_profile = pr,
    risk = unname(s$point), risk_lower = apply(s$draws, 2, qq, 0.025),
    risk_upper = apply(s$draws, 2, qq, 0.975),
    rr = unname(s$point / s$point[["None"]]),
    rr_lower = apply(s$draws / s$draws[, "None"], 2, qq, 0.025),
    rr_upper = apply(s$draws / s$draws[, "None"], 2, qq, 0.975),
    rd_vs_none_pp = 100 * unname(s$point - s$point[["None"]]),
    rd_vs_none_lower_pp = 100 * apply(s$draws - s$draws[, "None"], 2, qq, 0.025),
    rd_vs_none_upper_pp = 100 * apply(s$draws - s$draws[, "None"], 2, qq, 0.975)
  ) |>
    dplyr::left_join(support, by = "core_profile")
}

pm_age_table <- function(s, model_id, outcome) {
  purrr::map_dfr(dimnames(s$draws_age)[[2]], function(g) {
    pt <- s$point_age[g, ]; dr <- s$draws_age[, g, ]
    purrr::map_dfr(names(pt), function(p) dplyr::tibble(
      outcome = outcome, model_id = model_id, age_group = g, core_profile = p,
      n_target_age = s$n_age[[g]],
      std_risk_pct = 100 * pt[[p]],
      std_risk_lower_pct = 100 * qq(dr[, p], 0.025), std_risk_upper_pct = 100 * qq(dr[, p], 0.975),
      rr = pt[[p]] / pt[["None"]],
      rr_lower = qq(dr[, p] / dr[, "None"], 0.025), rr_upper = qq(dr[, p] / dr[, "None"], 0.975),
      rd_pp = 100 * (pt[[p]] - pt[["None"]]),
      rd_lower_pp = 100 * qq(dr[, p] - dr[, "None"], 0.025),
      rd_upper_pp = 100 * qq(dr[, p] - dr[, "None"], 0.975)))
  })
}

## Six within-count contrasts; pointwise 95% and Bonferroni-simultaneous
## (6 contrasts per outcome, 1 - 0.05/6) intervals from shared draws.
pm_contrasts <- function(s, model_id, outcome, support) {
  a_s <- 0.05 / nrow(contrast_defs)
  purrr::pmap_dfr(contrast_defs, function(group, profile_a, profile_b) {
    rd <- 100 * (s$draws[, profile_a] - s$draws[, profile_b])
    rr <- s$draws[, profile_a] / s$draws[, profile_b]
    sa <- support[support$core_profile == profile_a, ]; sb <- support[support$core_profile == profile_b, ]
    dplyr::tibble(
      outcome = outcome, model_id = model_id, group = group, profile_a = profile_a, profile_b = profile_b,
      contrast = paste(profile_a, "vs", profile_b),
      risk_a_pct = 100 * s$point[[profile_a]], risk_b_pct = 100 * s$point[[profile_b]],
      rd_pp = 100 * (s$point[[profile_a]] - s$point[[profile_b]]),
      rd_lower_pp = qq(rd, 0.025), rd_upper_pp = qq(rd, 0.975),
      rd_lower_simult_pp = qq(rd, a_s / 2), rd_upper_simult_pp = qq(rd, 1 - a_s / 2),
      rr = s$point[[profile_a]] / s$point[[profile_b]], rr_lower = qq(rr, 0.025), rr_upper = qq(rr, 0.975),
      prop_draws_rd_gt0 = mean(rd > 0),
      n_cases_a = sa$n_cases, n_events_a = sa$n_events, n_cases_b = sb$n_cases, n_events_b = sb$n_events,
      sparse = min(sa$n_events, sb$n_events) < 10)
  })
}

pm_support <- function(cohort) {
  cohort |>
    dplyr::group_by(core_profile, .drop = FALSE) |>
    dplyr::summarise(n_cases = dplyr::n(), n_events = sum(y), .groups = "drop") |>
    dplyr::mutate(core_profile = as.character(core_profile), n_nonevents = n_cases - n_events)
}

pm_age_support <- function(cohort) {
  cohort |>
    dplyr::group_by(age_group, core_profile, .drop = FALSE) |>
    dplyr::summarise(n_cases = dplyr::n(), n_events = sum(y), .groups = "drop") |>
    dplyr::mutate(age_group = as.character(age_group), core_profile = as.character(core_profile),
                  n_nonevents = n_cases - n_events,
                  support_category = dplyr::case_when(
                    n_events >= 10 & n_nonevents >= 10 ~ "Adequate (>=10 events and >=10 non-events)",
                    n_events >= 1 ~ "Sparse (1-9 events)",
                    TRUE ~ "No observed events"))
}
