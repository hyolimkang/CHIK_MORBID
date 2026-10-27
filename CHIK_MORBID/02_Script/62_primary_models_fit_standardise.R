# ---------------------------------------------------------------------------
# 62_primary_models_fit_standardise.R
#
# Harmonised primary models: ONE fitted GAM per outcome underlies every main
# estimate (Figures 1-3, Table 2).
#   hospitalisation_H2  (cohort N = 776,072; 25,363 hospitalisations)
#   mortality_H0        (cohort N = 983,947; 1,050 deaths)
# Steps: cohort reconstruction and checks -> fit -> model verification ->
# 4,000 shared coefficient draws per outcome -> overall and age-specific
# standardised risks (same draws, same per-combination predictions) -> RRs,
# within-count RDs -> overall-vs-age identity checks (point and every draw)
# -> Monte Carlo check -> comparison with historical estimates.
#
# Reads only validated inputs; writes only to 03_Output/primary_models_H2_H0_final/.
# Figures/tables: script 64. Sensitivity analyses: script 63. Checks: script 65.
# Runtime: ~15-25 min.
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
source("02_Script/primary_models_functions.R")

B <- 4000
B_mc <- 1000          # Monte Carlo check: first 1,000 of the same draws
n_threads <- max(1, min(8, parallel::detectCores() - 2))
tol_identity <- 1e-12
for (d in c("models", "estimates", "validation", "supplementary"))
  dir.create(file.path(pm_dir, d), recursive = TRUE, showWarnings = FALSE)
log62 <- function(...) pm_log("62", ...)

## ============================================================
## 1. Cohorts and checks (stop on any failure)
## ============================================================
data_path <- "01_Data/chik_sinan_individual_2015_2025.rds"
co <- pm_build_cohorts(data_path)
cohorts <- list(hosp = co$hosp, death = co$death)

## EVOLUCAO mapping (Gate 3.1): death = code 2 only; 1/3 = non-death; others NA.
evo <- co$eligible |> dplyr::count(evolution, died_from_chik)
evo_ok <- all(evo$died_from_chik[evo$evolution %in% "death_chik"] %in% TRUE) &&
  all(evo$died_from_chik[evo$evolution %in% c("cured", "death_other")] %in% FALSE) &&
  all(is.na(evo$died_from_chik[!evo$evolution %in% c("death_chik", "cured", "death_other")]))

table1 <- readr::read_csv("03_Output/tables/table1_updated/Table1_Updated_Values.csv", show_col_types = FALSE)
t1_lab <- c(None = "None, n (%)", DM = "DM only, n (%)", HTN = "HTN only, n (%)", CKD = "CKD only, n (%)",
            `DM+HTN` = "DM + HTN, n (%)", `DM+CKD` = "DM + CKD, n (%)", `HTN+CKD` = "HTN + CKD, n (%)",
            `DM+HTN+CKD` = "DM + HTN + CKD, n (%)")
t1_n <- function(col) as.numeric(gsub(",", "", sub(" .*", "", table1[[col]][match(t1_lab, table1$characteristic)])))

## Validated profile event counts (script 28 caches, used by Figure 1 / Table 2).
load("01_Data/core_profile_rr_hosp.RData"); load("01_Data/core_profile_rr_death.RData")
hist_overall <- list(hosp = as.data.frame(core_profile_rr_hosp), death = as.data.frame(core_profile_rr_death))

support <- lapply(cohorts, pm_support)
age_support <- lapply(cohorts, pm_age_support)
chk <- function(check, outcome, expected, observed)
  tibble::tibble(check = check, outcome = outcome, expected = as.character(expected),
                 observed = as.character(observed), pass = identical(as.character(expected), as.character(observed)))
recon <- dplyr::bind_rows(
  chk("Eligible core-3 population", "both", expected_counts$eligible, nrow(co$eligible)),
  chk("Cohort N", "hosp", expected_counts$hosp_n, nrow(cohorts$hosp)),
  chk("Events", "hosp", expected_counts$hosp_events, sum(cohorts$hosp$y)),
  chk("Cohort N", "death", expected_counts$death_n, nrow(cohorts$death)),
  chk("Events", "death", expected_counts$death_events, sum(cohorts$death$y)),
  chk("EVOLUCAO mapping (2 = death; 1,3 = no; other = NA)", "death", TRUE, evo_ok),
  purrr::map_dfr(names(cohorts), function(o) dplyr::bind_rows(
    chk("Eight mutually exclusive profiles, none missing", o, 8,
        dplyr::n_distinct(cohorts[[o]]$core_profile[!is.na(cohorts[[o]]$core_profile)])),
    chk("Profile counts sum to cohort N", o, nrow(cohorts[[o]]), sum(support[[o]]$n_cases)),
    chk("Profile event counts sum to events", o, sum(cohorts[[o]]$y), sum(support[[o]]$n_events)),
    chk("Profile N match Table 1", o, paste(t1_n(ifelse(o == "hosp", "hospitalisation", "mortality")), collapse = ","),
        paste(support[[o]]$n_cases, collapse = ",")),
    chk("Profile events match validated profile caches", o,
        paste(hist_overall[[o]]$n_events_profile, collapse = ","), paste(support[[o]]$n_events, collapse = ",")),
    chk("Age groups partition cohort", o, nrow(cohorts[[o]]), sum(!is.na(cohorts[[o]]$age_group)))
  )),
  chk("Mutual exclusivity: dm+htn+ckd recode reproduces profile", "both", TRUE,
      all(with(co$eligible, (dm + htn + ckd) == c(0, 1, 1, 1, 2, 2, 2, 3)[as.integer(core_profile)])))
)
readr::write_csv(recon, file.path(pm_dir, "validation", "cohort_reconciliation.csv"))
print(recon, n = Inf)
if (!all(recon$pass)) stop("Cohort validation failed - see cohort_reconciliation.csv")
readr::write_csv(evo, file.path(pm_dir, "validation", "evolucao_mapping_check.csv"))
rm(co); invisible(gc())

## ============================================================
## 2. Fit the two primary models
## ============================================================
fits <- list()
for (m in names(primary_models)) {
  o <- primary_models[[m]]$outcome
  log62("fitting %s", m)
  fits[[m]] <- pm_fit(primary_models[[m]]$formula, cohorts[[o]], n_threads)
  saveRDS(fits[[m]], file.path(pm_dir, "models",
                               ifelse(o == "hosp", "hospitalisation_H2_primary.rds", "mortality_H0_primary.rds")))
}

## Model verification.
git_rev <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA_character_)
data_md5 <- unname(tools::md5sum(data_path))
verif <- purrr::imap_dfr(fits, function(fit, m) {
  V <- stats::vcov(fit, unconditional = TRUE)
  ev <- eigen((V + t(V)) / 2, symmetric = TRUE, only.values = TRUE)$values
  set.seed(62)
  kc <- tryCatch(mgcv::k.check(fit, subsample = 5000, n.rep = 200), error = function(e) NULL)
  st <- summary(fit)$s.table
  tibble::tibble(
    model_id = m, n = nrow(fit$model), events = sum(fit$y),
    converged = if (is.null(fit$converged)) NA else fit$converged,
    outer_convergence = if (!is.null(fit$outer.info$conv)) fit$outer.info$conv else NA_character_,
    n_warnings = length(fit$audit_warnings), warnings = paste(unique(fit$audit_warnings), collapse = " | "),
    aic = stats::AIC(fit), total_edf = sum(fit$edf), deviance_explained_pct = 100 * summary(fit)$dev.expl,
    n_coef = length(coef(fit)),
    vcov_min_eigen = min(ev), vcov_max_eigen = max(ev), vcov_condition = max(ev) / min(ev),
    vcov_positive_definite = min(ev) > 0,
    smooth_terms = paste(rownames(st), collapse = "; "),
    smooth_edf = paste(sprintf("%.2f", st[, "edf"]), collapse = "; "),
    smoothing_parameters = paste(sprintf("%s=%.3g", names(fit$sp), fit$sp), collapse = "; "),
    k_check_terms = if (!is.null(kc)) paste(rownames(kc), collapse = "; ") else NA_character_,
    k_index = if (!is.null(kc)) paste(sprintf("%.3f", kc[, "k-index"]), collapse = "; ") else NA_character_,
    k_check_p = if (!is.null(kc)) paste(sprintf("%.3f", kc[, "p-value"]), collapse = "; ") else NA_character_,
    k_prime_vs_edf = if (!is.null(kc)) paste(sprintf("%g/%.2f", kc[, "k'"], kc[, "edf"]), collapse = "; ") else NA_character_,
    fit_seconds = fit$audit_seconds)
})
readr::write_csv(verif, file.path(pm_dir, "validation", "primary_model_verification.csv"))
print(as.data.frame(verif[, c("model_id", "n", "events", "converged", "n_warnings", "aic", "total_edf",
                              "deviance_explained_pct", "vcov_positive_definite", "vcov_condition")]))
stopifnot(all(verif$vcov_positive_definite))

provenance <- purrr::imap_dfr(primary_models, function(spec, m) tibble::tibble(
  model_id = m, outcome = spec$outcome_label, dataset = data_path, dataset_md5 = data_md5,
  cohort_rule = paste("Confirmed CHIK (CLASSI_FIN 13); event year >= 2017; age 0-100; sex male/female;",
                      "DM/HTN/renal explicitly yes/no;",
                      ifelse(spec$outcome == "hosp", "hospitalisation status yes/no",
                             "died_from_chik non-missing (EVOLUCAO 2 vs 1/3)")),
  formula = spec$formula, fitting = "mgcv::bam(family = binomial(logit), method = 'fREML', discrete = TRUE)",
  n = nrow(fits[[m]]$model), events = sum(fits[[m]]$y),
  covariates = "core_profile (8 levels, ref None), sex, year (factor), uf_residence (random effect), age_years (smooth)",
  coefficient_draws = B, draw_seed = spec$seed, covariance = "vcov(fit, unconditional = TRUE)",
  code_version = paste0("git ", git_rev, " + uncommitted scripts 62-65"),
  r_version = R.version.string, mgcv_version = as.character(utils::packageVersion("mgcv")),
  run_date = format(Sys.time(), "%Y-%m-%d %H:%M")))
readr::write_csv(provenance, file.path(pm_dir, "models", "model_provenance.csv"))

## ============================================================
## 3. Shared draws and standardisation
## ============================================================
draws <- list(); std <- list()
for (m in names(primary_models)) {
  o <- primary_models[[m]]$outcome
  draws[[m]] <- pm_draw_coefs(fits[[m]], B, primary_models[[m]]$seed)
  log62("standardising %s (B = %d)", m, B)
  std[[m]] <- pm_standardise(fits[[m]], cohorts[[o]], draws[[m]])
}
saveRDS(list(beta_draws = draws$hospitalisation_H2, seed = primary_models$hospitalisation_H2$seed,
             coef_names = names(coef(fits$hospitalisation_H2))),
        file.path(pm_dir, "estimates", "coefficient_draws_hospitalisation.rds"))
saveRDS(list(beta_draws = draws$mortality_H0, seed = primary_models$mortality_H0$seed,
             coef_names = names(coef(fits$mortality_H0))),
        file.path(pm_dir, "estimates", "coefficient_draws_mortality.rds"))
saveRDS(std, file.path(pm_dir, "estimates", "primary_standardised_risk_draws.rds"))

## ============================================================
## 4. Estimates
## ============================================================
overall <- dplyr::bind_rows(purrr::imap(std, function(s, m) {
  o <- primary_models[[m]]$outcome
  pm_overall_table(s, m, primary_models[[m]]$outcome_label, support[[o]])
}))
age_est <- dplyr::bind_rows(purrr::imap(std, function(s, m) {
  o <- primary_models[[m]]$outcome
  pm_age_table(s, m, primary_models[[m]]$outcome_label) |>
    dplyr::left_join(age_support[[o]], by = c("age_group", "core_profile"))
}))
contrasts <- dplyr::bind_rows(purrr::imap(std, function(s, m) {
  o <- primary_models[[m]]$outcome
  pm_contrasts(s, m, primary_models[[m]]$outcome_label, support[[o]])
}))

readr::write_csv(dplyr::select(overall, outcome, model_id, core_profile, n_cases, n_events, n_nonevents,
                               risk, risk_lower, risk_upper),
                 file.path(pm_dir, "estimates", "overall_standardised_risks.csv"))
readr::write_csv(dplyr::select(overall, outcome, model_id, core_profile, n_cases, n_events,
                               rr, rr_lower, rr_upper, rd_vs_none_pp, rd_vs_none_lower_pp, rd_vs_none_upper_pp),
                 file.path(pm_dir, "estimates", "overall_standardised_RRs.csv"))
readr::write_csv(dplyr::select(age_est, outcome, model_id, age_group, core_profile, n_cases, n_events, n_nonevents,
                               support_category, n_target_age, std_risk_pct, std_risk_lower_pct, std_risk_upper_pct),
                 file.path(pm_dir, "estimates", "age_specific_standardised_risks.csv"))
readr::write_csv(dplyr::select(age_est, outcome, model_id, age_group, core_profile, n_cases, n_events, n_nonevents,
                               support_category, rr, rr_lower, rr_upper, rd_pp, rd_lower_pp, rd_upper_pp),
                 file.path(pm_dir, "estimates", "age_specific_standardised_RRs.csv"))
readr::write_csv(contrasts, file.path(pm_dir, "estimates", "within_count_standardised_RDs.csv"))
readr::write_csv(dplyr::bind_rows(purrr::imap(support, ~ dplyr::mutate(.x, outcome = .y, .before = 1))),
                 file.path(pm_dir, "estimates", "profile_event_support.csv"))
readr::write_csv(age_est, file.path(pm_dir, "supplementary", "age_specific_profile_estimates.csv"))
readr::write_csv(dplyr::bind_rows(purrr::imap(age_support, ~ dplyr::mutate(.x, outcome = .y, .before = 1))),
                 file.path(pm_dir, "supplementary", "age_profile_support.csv"))

## ============================================================
## 5. Standardisation identities (point and every draw)
## ============================================================
draw_checks <- purrr::imap_dfr(std, function(s, m) {
  wa <- s$n_age / s$n_target
  pt_diff <- s$point - colSums(s$point_age * wa)
  dr_diff <- s$draws - apply(s$draws_age, c(1, 3), function(v) sum(v * wa))
  tibble::tibble(
    model_id = m, core_profile = names(s$point),
    target_n = s$n_target, age_weights = paste(sprintf("%.6f", wa), collapse = ";"),
    point_overall_minus_weighted_age = unname(pt_diff),
    max_abs_draw_overall_minus_weighted_age = apply(abs(dr_diff), 2, max),
    none_rr_all_draws_equal_1 = all(s$draws[, "None"] / s$draws[, "None"] == 1),
    all_probabilities_in_0_1 = all(s$draws > 0 & s$draws < 1) && all(s$draws_age > 0 & s$draws_age < 1),
    any_nonfinite = any(!is.finite(s$draws)) || any(!is.finite(s$draws_age)),
    risk_ci_contains_point = qq(s$draws[, names(s$point)[1]], 0) >= 0)
}) |>
  dplyr::group_by(model_id) |>
  dplyr::mutate(risk_ci_contains_point = {
    s <- std[[model_id[1]]]
    lo <- apply(s$draws, 2, qq, 0.025); hi <- apply(s$draws, 2, qq, 0.975)
    s$point >= lo & s$point <= hi
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(identity_pass = abs(point_overall_minus_weighted_age) < tol_identity &
                  max_abs_draw_overall_minus_weighted_age < tol_identity)

## Frequency-weighted vs individual-level averaging (point estimates).
indiv <- purrr::imap_dfr(fits, function(fit, m) {
  o <- primary_models[[m]]$outcome
  log62("individual-level point check %s", m)
  ip <- pm_individual_point(fit, cohorts[[o]])
  tibble::tibble(model_id = m, core_profile = names(ip), individual_level_risk = unname(ip),
                 weighted_risk = unname(std[[m]]$point[names(ip)]))
}) |> dplyr::mutate(rel_diff = weighted_risk / individual_level_risk - 1)
draw_checks <- dplyr::left_join(draw_checks, indiv, by = c("model_id", "core_profile"))
readr::write_csv(draw_checks, file.path(pm_dir, "validation", "draw_level_standardisation_checks.csv"))
print(as.data.frame(draw_checks[, c("model_id", "core_profile", "point_overall_minus_weighted_age",
                                    "max_abs_draw_overall_minus_weighted_age", "rel_diff", "identity_pass")]))
if (!all(draw_checks$identity_pass)) stop("Overall-vs-age standardisation identity failed")
if (max(abs(draw_checks$rel_diff)) > 1e-8) stop("Weighted and individual-level standardisation disagree")
if (!all(draw_checks$all_probabilities_in_0_1) || any(draw_checks$any_nonfinite)) stop("Invalid draw probabilities")

## Monte Carlo check: 4,000 vs first 1,000 draws (same seed stream).
mc <- purrr::imap_dfr(std, function(s, m) {
  sub <- s; sub$draws <- s$draws[seq_len(B_mc), ]
  a <- pm_overall_table(s, m, m, support[[primary_models[[m]]$outcome]])
  b <- pm_overall_table(sub, m, m, support[[primary_models[[m]]$outcome]])
  tibble::tibble(model_id = m, core_profile = a$core_profile,
                 rel_diff_risk_lower = b$risk_lower / a$risk_lower - 1,
                 rel_diff_risk_upper = b$risk_upper / a$risk_upper - 1,
                 rel_diff_rr_lower = b$rr_lower / a$rr_lower - 1,
                 rel_diff_rr_upper = b$rr_upper / a$rr_upper - 1)
})
readr::write_csv(mc, file.path(pm_dir, "validation", "monte_carlo_check_4000_vs_1000_draws.csv"))

## ============================================================
## 6. Comparison with historical estimates
## ============================================================
## (a) Overall: historical Figure 1 / Table 2 caches (H0, gam REML, 2,000 draws).
old_overall <- purrr::imap_dfr(hist_overall, function(d, o) tibble::tibble(
  outcome = c(hosp = "Hospitalisation", death = "Chikungunya-attributed death")[[o]],
  core_profile = as.character(d$core_profile), old_model = "H0 (gam REML; script 28)",
  old_risk = d$standardised_risk, old_risk_lower = d$standardised_risk_lower, old_risk_upper = d$standardised_risk_upper,
  old_rr = d$rr, old_rr_lower = d$rr_lower, old_rr_upper = d$rr_upper))
cmp_overall <- overall |>
  dplyr::select(outcome, model_id, core_profile, n_events, risk, risk_lower, risk_upper, rr, rr_lower, rr_upper) |>
  dplyr::inner_join(old_overall, by = c("outcome", "core_profile")) |>
  dplyr::mutate(quantity = "overall", risk_diff_pp = 100 * (risk - old_risk), risk_pct_change = 100 * (risk / old_risk - 1),
                rr_diff = rr - old_rr, rr_pct_change = 100 * (rr / old_rr - 1),
                old_ci_width_rr = old_rr_upper - old_rr_lower, new_ci_width_rr = rr_upper - rr_lower,
                old_rank_rr = rank(-old_rr), new_rank_rr = rank(-rr))
cmp_overall <- cmp_overall |> dplyr::group_by(outcome) |>
  dplyr::mutate(old_rank_rr = rank(-old_rr), new_rank_rr = rank(-rr)) |> dplyr::ungroup()

## (b) Age-specific: hospitalisation H2 (script 52, 2,000 draws), mortality fs_original (script 46).
old_age_h <- readr::read_csv("03_Output/figure_revision/figure3_final/hospital_age_profile_all_model_estimates.csv",
                             show_col_types = FALSE) |>
  dplyr::filter(model == "H2") |> dplyr::mutate(outcome = "Hospitalisation", old_model = "H2 (script 52)")
old_age_m <- readr::read_csv("03_Output/validation/figure5_age_validation/age_profile_standardised_estimates.csv",
                             show_col_types = FALSE) |>
  dplyr::mutate(outcome = "Chikungunya-attributed death", old_model = "fs_original (script 46)")
old_age <- dplyr::bind_rows(old_age_h, old_age_m) |>
  dplyr::transmute(outcome, age_group, core_profile, old_model, old_risk_pct = std_risk_pct,
                   old_risk_lower_pct = std_risk_lower_pct, old_risk_upper_pct = std_risk_upper_pct,
                   old_rr = rr, old_rr_lower = rr_lower, old_rr_upper = rr_upper)
cmp_age <- age_est |>
  dplyr::select(outcome, model_id, age_group, core_profile, n_events, support_category, std_risk_pct,
                std_risk_lower_pct, std_risk_upper_pct, rr, rr_lower, rr_upper) |>
  dplyr::inner_join(old_age, by = c("outcome", "age_group", "core_profile")) |>
  dplyr::mutate(quantity = "age-specific", risk_diff_pp = std_risk_pct - old_risk_pct,
                risk_pct_change = 100 * (std_risk_pct / old_risk_pct - 1),
                rr_pct_change = 100 * (rr / old_rr - 1))

## (c) Within-count RDs: historical Figure 3 (script 56: H0 bam, 4,000 draws).
old_rd <- readr::read_csv("03_Output/figure_revision/within_count/within_count_direct_contrasts.csv",
                          show_col_types = FALSE) |>
  dplyr::transmute(outcome = ifelse(outcome == "hosp", "Hospitalisation", "Chikungunya-attributed death"),
                   profile_a, profile_b, old_model = "H0 (bam; script 56)", old_rd_pp = rd_pp,
                   old_rd_lower_pp = rd_lower_pp, old_rd_upper_pp = rd_upper_pp)
cmp_rd <- contrasts |>
  dplyr::select(outcome, model_id, group, profile_a, profile_b, rd_pp, rd_lower_pp, rd_upper_pp, sparse) |>
  dplyr::inner_join(old_rd, by = c("outcome", "profile_a", "profile_b")) |>
  dplyr::mutate(quantity = "within-count RD", rd_diff_pp = rd_pp - old_rd_pp,
                rd_pct_change = 100 * (rd_pp / old_rd_pp - 1),
                old_excludes_0 = old_rd_lower_pp > 0 | old_rd_upper_pp < 0,
                new_excludes_0 = rd_lower_pp > 0 | rd_upper_pp < 0)

saveRDS(list(overall = cmp_overall, age = cmp_age, rd = cmp_rd),
        file.path(pm_dir, "validation", "old_vs_new_primary_estimates.rds"))
old_vs_new <- dplyr::bind_rows(
  cmp_overall |> dplyr::transmute(quantity, outcome, new_model = model_id, old_model, age_group = "All ages",
                                  item = core_profile, n_events,
                                  old_estimate = 100 * old_risk, old_lower = 100 * old_risk_lower, old_upper = 100 * old_risk_upper,
                                  new_estimate = 100 * risk, new_lower = 100 * risk_lower, new_upper = 100 * risk_upper,
                                  metric = "Standardised risk (%)"),
  cmp_overall |> dplyr::transmute(quantity, outcome, new_model = model_id, old_model, age_group = "All ages",
                                  item = core_profile, n_events,
                                  old_estimate = old_rr, old_lower = old_rr_lower, old_upper = old_rr_upper,
                                  new_estimate = rr, new_lower = rr_lower, new_upper = rr_upper, metric = "RR vs None"),
  cmp_age |> dplyr::transmute(quantity, outcome, new_model = model_id, old_model, age_group, item = core_profile,
                              n_events, old_estimate = old_risk_pct, old_lower = old_risk_lower_pct,
                              old_upper = old_risk_upper_pct, new_estimate = std_risk_pct,
                              new_lower = std_risk_lower_pct, new_upper = std_risk_upper_pct,
                              metric = "Standardised risk (%)"),
  cmp_age |> dplyr::transmute(quantity, outcome, new_model = model_id, old_model, age_group, item = core_profile,
                              n_events, old_estimate = old_rr, old_lower = old_rr_lower, old_upper = old_rr_upper,
                              new_estimate = rr, new_lower = rr_lower, new_upper = rr_upper, metric = "RR vs None"),
  cmp_rd |> dplyr::transmute(quantity, outcome, new_model = model_id, old_model, age_group = "All ages",
                             item = paste(profile_a, "vs", profile_b), n_events = NA_real_,
                             old_estimate = old_rd_pp, old_lower = old_rd_lower_pp, old_upper = old_rd_upper_pp,
                             new_estimate = rd_pp, new_lower = rd_lower_pp, new_upper = rd_upper_pp,
                             metric = "RD (percentage points)")
) |>
  dplyr::mutate(abs_diff = new_estimate - old_estimate,
                pct_diff = 100 * (new_estimate / old_estimate - 1),
                old_ci_width = old_upper - old_lower, new_ci_width = new_upper - new_lower,
                ci_width_pct_change = 100 * (new_ci_width / old_ci_width - 1),
                material_change = abs(pct_diff) >= 10 | abs(ci_width_pct_change) >= 25)
readr::write_csv(old_vs_new, file.path(pm_dir, "validation", "old_vs_new_primary_estimates.csv"))

## (d) Reproduction of the historical model versions (same specification).
repro <- dplyr::bind_rows(
  cmp_age |> dplyr::filter(outcome == "Hospitalisation") |>
    dplyr::summarise(check = "H2 age-specific risks vs script 52 H2 (same spec, bam)",
                     n_values = dplyr::n(), max_abs_rel_diff_point = max(abs(std_risk_pct / old_risk_pct - 1)),
                     max_abs_rel_diff_ci = max(abs(c(std_risk_lower_pct / old_risk_lower_pct,
                                                     std_risk_upper_pct / old_risk_upper_pct) - 1))),
  cmp_overall |> dplyr::filter(outcome == "Chikungunya-attributed death") |>
    dplyr::summarise(check = "H0 mortality overall risks vs script 28 cache (gam REML vs bam fREML)",
                     n_values = dplyr::n(), max_abs_rel_diff_point = max(abs(risk / old_risk - 1)),
                     max_abs_rel_diff_ci = max(abs(c(risk_lower / old_risk_lower, risk_upper / old_risk_upper) - 1))),
  cmp_rd |> dplyr::filter(outcome == "Chikungunya-attributed death") |>
    dplyr::summarise(check = "H0 mortality RDs vs script 56 (same spec, bam)", n_values = dplyr::n(),
                     max_abs_rel_diff_point = max(abs(rd_pp / old_rd_pp - 1)),
                     max_abs_rel_diff_ci = max(abs(c(rd_lower_pp / old_rd_lower_pp, rd_upper_pp / old_rd_upper_pp) - 1)))
)
readr::write_csv(repro, file.path(pm_dir, "validation", "historical_reproduction_check.csv"))
print(repro)

## Model comparison summary (primary models + historical AIC context).
hist_aic_h <- readr::read_csv("03_Output/figure_revision/figure3_final/hospital_age_model_comparison.csv",
                              show_col_types = FALSE) |>
  dplyr::transmute(outcome = "Hospitalisation", model, label, aic, delta_aic_vs_H0, total_edf, source = "script 52")
hist_aic_m <- readr::read_csv("03_Output/validation/figure5_age_validation/age_interaction_diagnostics.csv",
                              show_col_types = FALSE)
hist_aic_m <- hist_aic_m |>
  dplyr::transmute(outcome = "Chikungunya-attributed death", model, label = if ("model_label" %in% names(hist_aic_m)) model_label else model,
                   aic, delta_aic_vs_H0 = aic - aic[model == "no_interaction"],
                   total_edf = if ("total_edf" %in% names(hist_aic_m)) total_edf else NA_real_, source = "script 46")
model_summary <- dplyr::bind_rows(
  verif |> dplyr::transmute(outcome = ifelse(model_id == "hospitalisation_H2", "Hospitalisation",
                                             "Chikungunya-attributed death"),
                            model = paste0(model_id, " (this pipeline, PRIMARY)"), label = model_id, aic,
                            delta_aic_vs_H0 = NA_real_, total_edf, source = "script 62"),
  hist_aic_h, hist_aic_m)
readr::write_csv(model_summary, file.path(pm_dir, "validation", "model_comparison_summary.csv"))

sink(file.path(pm_dir, "validation", "sessionInfo_62.txt")); print(utils::sessionInfo()); sink()
log62("done")
