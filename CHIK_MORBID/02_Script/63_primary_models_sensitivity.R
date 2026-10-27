# ---------------------------------------------------------------------------
# 63_primary_models_sensitivity.R
#
# Sensitivity analyses around the harmonised primary models (script 62).
# None of these estimates enters Figures 1-3 or Table 2.
#
# Hospitalisation (vs hospitalisation_H2):
#   H0            historical no-interaction specification (Figure 1 before)
#   H2_k5/H2_k15  basis dimension of the age smooths
#   H2_sp_x0.25 / H2_sp_x4  profile-deviation smoothing parameters fixed at
#                 1/4 and 4 x the primary estimate
#   H2_age15plus  refitted on cases aged >= 15 (removes childhood comorbidity
#                 records of doubtful validity); compared with the primary
#                 H2 standardised to the same >= 15 target
#   H2_C1         refitted in state-years with >= 70% hospitalisation
#                 recording (>= 100 eligible cases; Gate 3.2B scenario C1);
#                 compared with the primary H2 on the same restricted target
# Mortality (vs mortality_H0):
#   fs_original   historical age-specific Figure model (profile factor smooth)
#   fs_global     common s(age) + penalised profile deviations (H2-type)
#   H0_k20        richer common age smooth
#
# All use the same cohorts, covariates and g-computation as script 62, with
# 1,000 coefficient draws each. Output: <pm_dir>/supplementary/ and validation/.
# Runtime: ~45-90 min.
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

B_sens <- 1000
n_threads <- max(1, min(8, parallel::detectCores() - 2))
log63 <- function(...) pm_log("63", ...)

co <- pm_build_cohorts()
cohorts <- list(hosp = co$hosp, death = co$death)
stopifnot(nrow(cohorts$hosp) == expected_counts$hosp_n, nrow(cohorts$death) == expected_counts$death_n)
support <- lapply(cohorts, pm_support)

primary <- list(
  hospitalisation_H2 = readRDS(file.path(pm_dir, "models", "hospitalisation_H2_primary.rds")),
  mortality_H0 = readRDS(file.path(pm_dir, "models", "mortality_H0_primary.rds")))

base <- "y ~ core_profile + sex + year + s(uf_residence, bs = 're')"
h2_terms <- function(k) sprintf("+ s(age_years, k = %d, bs = 'ts') + s(age_years, core_profile, bs = 'fs', k = %d, m = 1)", k, k)

## Restricted targets.
hosp15 <- dplyr::filter(cohorts$hosp, age_years >= 15) |> dplyr::mutate(age_group = droplevels(age_group))
completeness <- co$eligible |>
  dplyr::group_by(uf_residence, year) |>
  dplyr::summarise(n_eligible = dplyr::n(), n_known = sum(hospitalised %in% c("no", "yes")), .groups = "drop") |>
  dplyr::filter(n_eligible >= 100, n_known / n_eligible >= 0.70)
hosp_c1 <- cohorts$hosp |> dplyr::semi_join(completeness, by = c("uf_residence", "year")) |>
  dplyr::mutate(uf_residence = droplevels(uf_residence), year = droplevels(year))
rm(co); invisible(gc())

sp_fixed <- function(mult) {
  sp <- primary$hospitalisation_H2$sp
  i <- grepl("core_profile", names(sp), fixed = TRUE); sp[i] <- sp[i] * mult; sp
}

runs <- list(
  list(id = "hosp_H0", outcome = "hosp", f = paste(base, "+ s(age_years, k = 10, bs = 'ts')"), data = "full"),
  list(id = "hosp_H2_k5", outcome = "hosp", f = paste(base, "+ s(age_years, k = 10, bs = 'ts') + s(age_years, core_profile, bs = 'fs', k = 5, m = 1)"), data = "full"),
  list(id = "hosp_H2_k15", outcome = "hosp", f = paste(base, h2_terms(15)), data = "full"),
  list(id = "hosp_H2_sp_x0.25", outcome = "hosp", f = paste(base, h2_terms(10)), data = "full", sp = sp_fixed(0.25)),
  list(id = "hosp_H2_sp_x4", outcome = "hosp", f = paste(base, h2_terms(10)), data = "full", sp = sp_fixed(4)),
  list(id = "hosp_H2_age15plus", outcome = "hosp", f = paste(base, h2_terms(10)), data = "age15"),
  list(id = "hosp_H2_C1_completeness70", outcome = "hosp", f = paste(base, h2_terms(10)), data = "c1"),
  list(id = "death_fs_original", outcome = "death", f = paste(base, "+ s(age_years, core_profile, bs = 'fs', k = 10)"), data = "full"),
  list(id = "death_fs_global", outcome = "death", f = paste(base, h2_terms(10)), data = "full"),
  list(id = "death_H0_k20", outcome = "death", f = paste(base, "+ s(age_years, k = 20, bs = 'ts')"), data = "full")
)
data_for <- function(r) switch(r$data, full = cohorts[[r$outcome]], age15 = hosp15, c1 = hosp_c1)
groups_for <- function(r) if (r$data == "age15") age_levels[-1] else age_levels
prim_id <- c(hosp = "hospitalisation_H2", death = "mortality_H0")

overall_rows <- list(); age_rows <- list(); fit_rows <- list()
for (i in seq_along(runs)) {
  r <- runs[[i]]; d <- data_for(r); grp <- groups_for(r)
  log63("fitting %s (n = %d)", r$id, nrow(d))
  fit <- pm_fit(r$f, d, n_threads, sp = r$sp)
  bd <- pm_draw_coefs(fit, B_sens, 6300 + i)
  s <- pm_standardise(fit, d, bd, groups = grp)
  sup <- pm_support(d)
  overall_rows[[r$id]] <- pm_overall_table(s, r$id, r$outcome, sup) |> dplyr::mutate(target = r$data)
  age_rows[[r$id]] <- pm_age_table(s, r$id, r$outcome) |> dplyr::mutate(target = r$data)
  fit_rows[[r$id]] <- tibble::tibble(model_id = r$id, outcome = r$outcome, target = r$data, formula = r$f,
                                     smoothing = ifelse(is.null(r$sp), "estimated (fREML)", "fixed"),
                                     n = nrow(d), events = sum(d$y), aic = stats::AIC(fit),
                                     total_edf = sum(fit$edf), n_warnings = length(fit$audit_warnings))
  ## Primary model on the same restricted target (common-target comparison).
  if (r$data != "full") {
    pf <- primary[[prim_id[[r$outcome]]]]
    pbd <- pm_draw_coefs(pf, B_sens, 6350 + i)
    ps <- pm_standardise(pf, d, pbd, groups = grp)
    pid <- paste0(prim_id[[r$outcome]], "_on_", r$data, "_target")
    overall_rows[[pid]] <- pm_overall_table(ps, pid, r$outcome, sup) |> dplyr::mutate(target = r$data)
    age_rows[[pid]] <- pm_age_table(ps, pid, r$outcome) |> dplyr::mutate(target = r$data)
  }
  rm(fit); invisible(gc())
}

## Primary estimates (script 62, 4,000 draws) as the comparator on the full target.
prim_overall <- readr::read_csv(file.path(pm_dir, "estimates", "overall_standardised_RRs.csv"), show_col_types = FALSE) |>
  dplyr::left_join(readr::read_csv(file.path(pm_dir, "estimates", "overall_standardised_risks.csv"), show_col_types = FALSE) |>
                     dplyr::select(model_id, core_profile, risk, risk_lower, risk_upper), by = c("model_id", "core_profile")) |>
  dplyr::mutate(outcome = ifelse(model_id == "hospitalisation_H2", "hosp", "death"), target = "full")
prim_age <- readr::read_csv(file.path(pm_dir, "supplementary", "age_specific_profile_estimates.csv"), show_col_types = FALSE) |>
  dplyr::mutate(outcome = ifelse(model_id == "hospitalisation_H2", "hosp", "death"), target = "full")

all_overall <- dplyr::bind_rows(
  dplyr::select(prim_overall, outcome, model_id, target, core_profile, n_cases, n_events, risk, risk_lower,
                risk_upper, rr, rr_lower, rr_upper),
  dplyr::bind_rows(overall_rows) |>
    dplyr::select(outcome, model_id, target, core_profile, n_cases, n_events, risk, risk_lower, risk_upper,
                  rr, rr_lower, rr_upper)) |>
  dplyr::mutate(role = ifelse(model_id %in% prim_id, "PRIMARY",
                              ifelse(grepl("_on_", model_id), "primary model on restricted target", "sensitivity")))
## Comparator for each row: primary model on the same target.
comparator <- all_overall |>
  dplyr::filter(role != "sensitivity") |>
  dplyr::transmute(outcome, target, core_profile, ref_risk = risk, ref_rr = rr)
all_overall <- all_overall |>
  dplyr::left_join(comparator, by = c("outcome", "target", "core_profile")) |>
  dplyr::mutate(risk_ratio_vs_primary = risk / ref_risk, rr_ratio_vs_primary = rr / ref_rr,
                rr_excludes_1 = rr_lower > 1 | rr_upper < 1)
readr::write_csv(all_overall, file.path(pm_dir, "supplementary", "primary_vs_alternative_models.csv"))

age_cols <- c("outcome", "model_id", "target", "age_group", "core_profile", "std_risk_pct", "std_risk_lower_pct",
              "std_risk_upper_pct", "rr", "rr_lower", "rr_upper")
age_support <- prim_age |> dplyr::select(outcome, age_group, core_profile, n_cases, n_events, support_category)
all_age <- dplyr::bind_rows(dplyr::select(prim_age, dplyr::all_of(age_cols)),
                            dplyr::select(dplyr::bind_rows(age_rows), dplyr::all_of(age_cols))) |>
  dplyr::left_join(age_support, by = c("outcome", "age_group", "core_profile"))
age_cmp <- all_age |>
  dplyr::filter(model_id %in% prim_id | grepl("_on_", model_id)) |>
  dplyr::transmute(outcome, target, age_group, core_profile, ref_risk_pct = std_risk_pct, ref_rr = rr)
all_age <- all_age |>
  dplyr::left_join(age_cmp, by = c("outcome", "target", "age_group", "core_profile")) |>
  dplyr::mutate(rr_ratio_vs_primary = rr / ref_rr, risk_ratio_vs_primary = std_risk_pct / ref_risk_pct)
readr::write_csv(all_age, file.path(pm_dir, "supplementary", "age_specific_RR_sensitivity.csv"))
readr::write_csv(dplyr::bind_rows(fit_rows), file.path(pm_dir, "validation", "sensitivity_model_fits.csv"))

## Compact summary: per model, how far overall RRs move from the primary model.
sens_summary <- all_overall |>
  dplyr::filter(role == "sensitivity", core_profile != "None") |>
  dplyr::group_by(outcome, model_id, target) |>
  dplyr::summarise(max_abs_log_rr_diff = max(abs(log(rr_ratio_vs_primary))),
                   profile_max_diff = core_profile[which.max(abs(log(rr_ratio_vs_primary)))],
                   n_profiles_rr_excludes_1 = sum(rr_excludes_1), .groups = "drop")
readr::write_csv(sens_summary, file.path(pm_dir, "validation", "sensitivity_overall_summary.csv"))
print(as.data.frame(sens_summary), digits = 3)
log63("done")
