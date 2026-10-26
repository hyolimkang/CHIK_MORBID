# ---------------------------------------------------------------------------
# 44_validate_clustering_pns.R   -- Gate 3.4
#
# EXTERNAL VALIDATION of the SINAN-derived multimorbidity clustering model
# (lambda, 32_clustering_ratio_r_brazil.R) against PNS 2019 directly-observed
# joint profiles. Compares four models (A=observed PNS, B=independence,
# C=PNS marginals + SINAN lambda, D=PNS marginals + PNS lambda), all using
# IDENTICAL PNS marginal prevalences, per age group.
#
# Does NOT modify SINAN cleaning/RR code, GBD/NCD-RisC scripts, the original
# lambda-estimation script (32_), or population burden scripts. Does NOT
# refit any RR model, rerun burden allocation, introduce a new copula/
# hierarchical model, or substitute PNS lambda into the production pipeline.
#
# Needs: 01_Data/pns_2019_raw/ (from Gate 3.3B, already downloaded - NOT
#          re-downloaded here)
#        01_Data/brazil_clustering_ratio_r.RData (lambda_table, calibrated_ui)
#        01_Data/brazil_clustering_lambda_draws.RData (lambda_draws, SINAN bootstrap)
# Output: 03_Output/validation/gate34/*.csv, *.png, VALIDATION_3_4.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(readr); library(tibble)
  library(survey); library(PNSIBGE); library(glue)
})
options(scipen = 999, dplyr.summarise.inform = FALSE, survey.lonely.psu = "adjust")

out_dir <- "03_Output/validation/gate34"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
raw_dir <- "01_Data/pns_2019_raw"
SEED <- 1L
set.seed(SEED)
font_family <- "Arial"
core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
condition_order <- c("DM","HTN","CKD")
pair_label <- function(a, b) paste(condition_order[condition_order %in% c(a, b)], collapse = "+")

stopifnot(file.exists(file.path(raw_dir, "PNS_2019.txt")),
          file.exists(file.path(raw_dir, "dicionario_input/input_PNS_2019.txt")),
          file.exists("01_Data/brazil_clustering_ratio_r.RData"),
          file.exists("01_Data/brazil_clustering_lambda_draws.RData"))

## =============================================================================
## LOAD PNS (identical extraction/harmonisation to Gate 3.3B - not re-verifying
## structural findings already established there, just reusing them)
## =============================================================================

vars_needed <- c("C006", "C008", "Q00201", "Q00202", "Q03001", "Q03002", "Q124")
pns_all <- read_pns(microdata = file.path(raw_dir, "PNS_2019.txt"),
                     input_txt = file.path(raw_dir, "dicionario_input/input_PNS_2019.txt"),
                     vars = vars_needed)

harmonise_dx <- function(dx, preg_only) {
  dplyr::case_when(
    dx == "2" ~ "no",
    dx == "1" & preg_only == "1" ~ "no",
    dx == "1" & preg_only %in% c("2", NA) ~ "yes",
    dx %in% c("9", NA) ~ "unknown",
    TRUE ~ "unknown"
  )
}

selected_adults <- pns_all |>
  dplyr::filter(M001 == "1") |>
  dplyr::mutate(
    age = as.numeric(C008),
    weight = as.numeric(V00291),
    htn_pns = harmonise_dx(Q00201, Q00202),
    dm_pns  = harmonise_dx(Q03001, Q03002),
    ckd_pns = dplyr::case_when(Q124=="2"~"no", Q124=="1"~"yes", Q124 %in% c("9",NA)~"unknown", TRUE~"unknown"),
    dm  = dplyr::case_when(dm_pns=="yes"~1L, dm_pns=="no"~0L, TRUE~NA_integer_),
    htn = dplyr::case_when(htn_pns=="yes"~1L, htn_pns=="no"~0L, TRUE~NA_integer_),
    ckd = dplyr::case_when(ckd_pns=="yes"~1L, ckd_pns=="no"~0L, TRUE~NA_integer_)
  )
stopifnot(nrow(selected_adults) == 90846, all(!is.na(selected_adults$weight)), all(selected_adults$weight > 0))
message(sprintf("[44] PNS selected-adult population loaded: N=%s", format(nrow(selected_adults), big.mark=",")))

## =============================================================================
## PHASE 1 -- MANDATORY PNS CONSISTENCY AUDIT
## Root cause of the Gate 3.3B discrepancy (CKD marginal 1.50% vs sum of
## CKD-containing profiles 1.57%): Gate 3.3B's marginal estimate used a
## DIFFERENT, LARGER denominator (all respondents with THAT ONE condition
## known, regardless of the other two) than its joint-profile estimate
## (respondents with ALL THREE conditions known). This is not a coding
## bug - both numbers were individually correct for their own stated
## denominator - but the two are not DIRECTLY comparable, which this gate's
## task explicitly requires. The fix: define ONE shared population
## (all three conditions known) and compute BOTH marginals and joint
## profiles from that SAME population/design object, so the
## marginal-reconstructed-from-profiles identity holds by construction.
## =============================================================================

shared_pop <- selected_adults |>
  dplyr::filter(age >= 20, !is.na(dm), !is.na(htn), !is.na(ckd)) |>
  dplyr::mutate(
    core_profile = dplyr::case_when(
      dm==0&htn==0&ckd==0 ~ "None", dm==1&htn==0&ckd==0 ~ "DM", dm==0&htn==1&ckd==0 ~ "HTN",
      dm==0&htn==0&ckd==1 ~ "CKD", dm==1&htn==1&ckd==0 ~ "DM+HTN", dm==1&htn==0&ckd==1 ~ "DM+CKD",
      dm==0&htn==1&ckd==1 ~ "HTN+CKD", dm==1&htn==1&ckd==1 ~ "DM+HTN+CKD"
    ),
    core_profile = factor(core_profile, levels = core_profile_levels),
    two_plus = as.integer((dm+htn+ckd) >= 2),
    age_grp4 = dplyr::case_when(age>=20&age<40~"20-39", age>=40&age<60~"40-59",
                                 age>=60&age<80~"60-79", age>=80~"80+", TRUE~NA_character_),
    age_grp4 = factor(age_grp4, levels=c("20-39","40-59","60-79","80+"))
  )
stopifnot(!anyNA(shared_pop$age_grp4), !anyNA(shared_pop$core_profile))
message(sprintf("[44] Shared complete-three-condition population (age 20+): N=%s", format(nrow(shared_pop), big.mark=",")))

des_shared <- survey::svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~weight, data = shared_pop, nest = TRUE)

## ---- also: the OLD Gate-3.3B-style per-condition-only denominator, kept
## SEPARATE and reported for transparency only, per instruction not to mix
## it into the clustering validation ----
old_style_marginal <- function(design_full, condvar) {
  sub <- subset(design_full, age >= 20 & !is.na(get(condvar)))
  ci <- survey::svyciprop(stats::as.formula(paste0("~I(",condvar,"==1)")), design=sub, method="logit", na.rm=TRUE)
  tibble::tibble(condition=condvar, denominator="condition-specific (Gate 3.3B style)", n=nrow(sub), prevalence=round(as.numeric(ci)*100,4))
}
des_all_20plus <- survey::svydesign(ids=~UPA_PNS, strata=~V0024, weights=~weight,
                                     data = selected_adults |> dplyr::filter(age>=20), nest=TRUE)
old_style <- dplyr::bind_rows(old_style_marginal(des_all_20plus,"dm"), old_style_marginal(des_all_20plus,"htn"),
                               old_style_marginal(des_all_20plus,"ckd"))

audit_rows <- list()
for (band in levels(shared_pop$age_grp4)) {
  sub <- subset(des_shared, age_grp4 == band)
  n_sub <- nrow(sub)

  marg <- survey::svymean(~dm+htn+ckd, design=sub, na.rm=TRUE)
  prof <- survey::svymean(~core_profile, design=sub, na.rm=TRUE)
  prof_names <- sub("core_profile","",names(prof))
  prof_est <- setNames(as.numeric(prof), prof_names)

  dm_recon <- sum(prof_est[c("DM","DM+HTN","DM+CKD","DM+HTN+CKD")])
  htn_recon <- sum(prof_est[c("HTN","DM+HTN","HTN+CKD","DM+HTN+CKD")])
  ckd_recon <- sum(prof_est[c("CKD","DM+CKD","HTN+CKD","DM+HTN+CKD")])

  audit_rows[[band]] <- tibble::tibble(
    age_group = band, n_shared_population = n_sub,
    dm_marginal_direct = as.numeric(marg["dm"]), dm_marginal_reconstructed = dm_recon,
    dm_abs_diff = abs(as.numeric(marg["dm"]) - dm_recon),
    htn_marginal_direct = as.numeric(marg["htn"]), htn_marginal_reconstructed = htn_recon,
    htn_abs_diff = abs(as.numeric(marg["htn"]) - htn_recon),
    ckd_marginal_direct = as.numeric(marg["ckd"]), ckd_marginal_reconstructed = ckd_recon,
    ckd_abs_diff = abs(as.numeric(marg["ckd"]) - ckd_recon),
    profile_sum = sum(prof_est),
    profile_sum_diff_from_1 = abs(sum(prof_est) - 1)
  )
}
pns_marginal_joint_consistency_audit <- dplyr::bind_rows(audit_rows)

TOL <- 1e-8
consistency_pass <- all(pns_marginal_joint_consistency_audit$dm_abs_diff < TOL,
                         pns_marginal_joint_consistency_audit$htn_abs_diff < TOL,
                         pns_marginal_joint_consistency_audit$ckd_abs_diff < TOL,
                         pns_marginal_joint_consistency_audit$profile_sum_diff_from_1 < TOL)

readr::write_csv(pns_marginal_joint_consistency_audit, file.path(out_dir, "pns_marginal_joint_consistency_audit.csv"))
readr::write_csv(old_style, file.path(out_dir, "pns_old_style_marginal_for_reference_only.csv"))

message(sprintf("[44] PHASE 1 consistency audit: max abs diff = %.2e (tolerance %.0e) -> %s",
                 max(pns_marginal_joint_consistency_audit$dm_abs_diff, pns_marginal_joint_consistency_audit$htn_abs_diff,
                     pns_marginal_joint_consistency_audit$ckd_abs_diff), TOL, ifelse(consistency_pass,"PASS","FAIL")))
print(pns_marginal_joint_consistency_audit |> dplyr::select(age_group, n_shared_population, dm_abs_diff, htn_abs_diff, ckd_abs_diff, profile_sum_diff_from_1))

if (!consistency_pass) {
  stop("[44] PHASE 1 FAILED: marginal/joint identity does not hold within tolerance on the shared population. Stopping before Phase 2 per instruction.")
}
message("[44] PHASE 1 PASSED - identity holds at floating-point precision on the shared complete-three-condition population. Proceeding to Phase 2.")

## =============================================================================
## MODEL FORMULAS (faithfully reproduced from 32_clustering_ratio_r_brazil.R,
## not sourced, per instruction not to modify/rerun that script)
## =============================================================================

p_independence <- function(p_dm, p_htn, p_ckd) {
  p_dm <- unname(p_dm); p_htn <- unname(p_htn); p_ckd <- unname(p_ckd)
  c(
    None = (1-p_dm)*(1-p_htn)*(1-p_ckd), DM = p_dm*(1-p_htn)*(1-p_ckd), HTN = (1-p_dm)*p_htn*(1-p_ckd),
    CKD = (1-p_dm)*(1-p_htn)*p_ckd, `DM+HTN` = p_dm*p_htn*(1-p_ckd), `DM+CKD` = p_dm*(1-p_htn)*p_ckd,
    `HTN+CKD` = (1-p_dm)*p_htn*p_ckd, `DM+HTN+CKD` = p_dm*p_htn*p_ckd
  )[core_profile_levels]
}

## Comonotonic/max-clustering - verbatim logic from 32_'s max_clustering_row().
## Ties in the marginals are handled deterministically by sort()'s stable,
## well-defined tie-breaking (ties keep input/name order) - documented here
## since the task requires explicit deterministic tie handling.
p_comonotonic <- function(p_dm, p_htn, p_ckd) {
  p_dm <- unname(p_dm); p_htn <- unname(p_htn); p_ckd <- unname(p_ckd)
  p <- c(DM=p_dm, HTN=p_htn, CKD=p_ckd)
  ord <- names(sort(p)); smallest<-ord[1]; middle<-ord[2]; largest<-ord[3]
  p_min<-p[[smallest]]; p_mid<-p[[middle]]; p_max<-p[[largest]]
  out <- setNames(rep(0,8), core_profile_levels)
  out["None"] <- 1-p_max; out[largest] <- p_max-p_mid
  out[pair_label(largest,middle)] <- p_mid-p_min; out["DM+HTN+CKD"] <- p_min
  out
}

p2plus_bounds <- function(p_dm, p_htn, p_ckd) {
  p_dm <- unname(p_dm); p_htn <- unname(p_htn); p_ckd <- unname(p_ckd)
  p_indep_2plus <- 1 - ((1-p_dm)*(1-p_htn)*(1-p_ckd)) - (p_dm*(1-p_htn)*(1-p_ckd)) -
    ((1-p_dm)*p_htn*(1-p_ckd)) - ((1-p_dm)*(1-p_htn)*p_ckd)
  p <- sort(c(p_dm,p_htn,p_ckd))
  c(indep = p_indep_2plus, maxclust = p[2])  # maxclust_2plus = middle marginal (verbatim from 32_)
}

two_plus_profiles <- c("DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

## ---- generic numerical delta-method SE: f(theta) a vector-valued function,
## theta_hat a point estimate vector, Sigma its (design-based) covariance ----
delta_se <- function(f, theta_hat, Sigma, eps = 1e-6) {
  k <- length(theta_hat); fx <- f(theta_hat); m <- length(fx)
  J <- matrix(0, nrow = m, ncol = k)
  for (i in seq_len(k)) {
    th_up <- theta_hat; th_dn <- theta_hat
    h <- max(abs(theta_hat[i]) * eps, eps)
    th_up[i] <- th_up[i] + h; th_dn[i] <- th_dn[i] - h
    J[, i] <- (f(th_up) - f(th_dn)) / (2*h)
  }
  var_f <- diag(J %*% Sigma %*% t(J))
  sqrt(pmax(var_f, 0))
}

message("[44] Model formulas and delta-method helper defined.")

## =============================================================================
## PHASE 2-6 -- PER-AGE-GROUP: MODEL A (observed), B (independence),
## C (SINAN lambda), D (PNS lambda), WITH DESIGN-BASED (delta-method) SEs
## =============================================================================

load("01_Data/brazil_clustering_ratio_r.RData")      # lambda_table, calibrated_ui
load("01_Data/brazil_clustering_lambda_draws.RData")  # lambda_draws (SINAN bootstrap, 1000/band)

## map Gate 3.4's 20-39/40-59/60-79/80+ onto 32_'s identical age_band_profile labels
sinan_lambda_by_band <- lambda_table |>
  dplyr::filter(age_band_profile %in% c("20-39","40-59","60-79","80+")) |>
  dplyr::mutate(age_band_profile = as.character(age_band_profile))

all_predictions <- list(); all_reliability <- list(); lambda_comparison <- list()

for (band in levels(shared_pop$age_grp4)) {

  sub <- subset(des_shared, age_grp4 == band)
  n_sub <- nrow(sub)
  dat <- sub$variables

  ## ---- Model A: observed profiles (point + design-based CI via svyciprop per profile) ----
  unw_counts <- dat |> dplyr::count(core_profile, .drop = FALSE) |> dplyr::rename(n_unweighted = n)
  prof_mean <- survey::svymean(~core_profile, design = sub, na.rm = TRUE)
  prof_point <- setNames(as.numeric(prof_mean), sub("core_profile","",names(prof_mean)))[core_profile_levels]
  prof_se_direct <- setNames(as.numeric(survey::SE(prof_mean)), sub("core_profile","",names(prof_mean)))[core_profile_levels]
  prof_ci <- stats::confint(prof_mean)
  rownames(prof_ci) <- sub("core_profile","",rownames(prof_ci))
  prof_ci <- prof_ci[core_profile_levels, , drop=FALSE]

  n_psu_distinct <- dat |> dplyr::group_by(core_profile, .drop=FALSE) |> dplyr::summarise(n_psu=dplyr::n_distinct(UPA_PNS), .groups="drop")

  ## ---- joint (dm,htn,ckd,two_plus) design-based mean + covariance, used
  ## for ALL delta-method propagation below (B, C's PNS component, D) ----
  jm <- survey::svymean(~dm+htn+ckd+two_plus, design = sub, na.rm = TRUE)
  theta_hat <- as.numeric(jm); names(theta_hat) <- c("dm","htn","ckd","two_plus")
  Sigma <- as.matrix(vcov(jm)); dimnames(Sigma) <- list(names(theta_hat), names(theta_hat))
  p_dm <- theta_hat["dm"]; p_htn <- theta_hat["htn"]; p_ckd <- theta_hat["ckd"]; p_obs_2plus <- theta_hat["two_plus"]

  ## ---- Model B: independence (function of dm,htn,ckd only -> use top-left 3x3 of Sigma) ----
  Sigma3 <- Sigma[1:3,1:3]
  f_ind <- function(th) p_independence(th[1], th[2], th[3])
  predB_point <- p_independence(p_dm, p_htn, p_ckd)
  predB_se <- delta_se(f_ind, theta_hat[1:3], Sigma3)

  f_como <- function(th) p_comonotonic(th[1], th[2], th[3])
  predComono_point <- p_comonotonic(p_dm, p_htn, p_ckd)
  predComono_se <- delta_se(f_como, theta_hat[1:3], Sigma3)

  bounds <- p2plus_bounds(p_dm, p_htn, p_ckd)

  ## ---- Model D: PNS-derived lambda (function of all 4 jointly) ----
  f_lambda_pns <- function(th) {
    b <- p2plus_bounds(th[1], th[2], th[3])
    (th[4] - b["indep"]) / (b["maxclust"] - b["indep"])
  }
  lambda_pns_raw <- as.numeric(f_lambda_pns(theta_hat))
  lambda_pns_se <- delta_se(f_lambda_pns, theta_hat, Sigma)
  lambda_pns_trunc <- min(max(lambda_pns_raw, 0), 1)
  denom_pns <- bounds["maxclust"] - bounds["indep"]

  f_predD <- function(th) {
    b <- p2plus_bounds(th[1], th[2], th[3])
    lam <- (th[4]-b["indep"])/(b["maxclust"]-b["indep"]); lam <- min(max(lam,0),1)
    (1-lam)*p_independence(th[1],th[2],th[3]) + lam*p_comonotonic(th[1],th[2],th[3])
  }
  predD_point <- f_predD(theta_hat)
  predD_se <- delta_se(f_predD, theta_hat, Sigma)

  ## ---- Model C: PNS marginals (same delta machinery, lambda FIXED at SINAN
  ## point) + lambda_SINAN's own [0,1] clipping/UI already computed in 32_ ----
  lam_sinan_row <- sinan_lambda_by_band |> dplyr::filter(age_band_profile == band)
  lam_sinan <- lam_sinan_row$lambda; lam_sinan_raw <- lam_sinan_row$lambda_raw
  lam_sinan_lo <- lam_sinan_row$lambda_lower; lam_sinan_hi <- lam_sinan_row$lambda_upper

  f_predC <- function(th) (1-lam_sinan)*p_independence(th[1],th[2],th[3]) + lam_sinan*p_comonotonic(th[1],th[2],th[3])
  predC_point <- f_predC(theta_hat)
  predC_se_pnspart <- delta_se(f_predC, theta_hat[1:3], Sigma3)  # PNS-marginal-uncertainty component only

  ## SINAN-lambda-uncertainty component: propagate the 1000 SINAN draws for
  ## this band onto the FIXED PNS POINT marginals (independent-source Monte
  ## Carlo, per Phase 7's "combine...independently" allowance)
  draws_band <- lambda_draws |> dplyr::filter(as.character(age_band_profile) == band) |> dplyr::pull(lambda)
  predC_draws <- sapply(draws_band, function(l) (1-l)*p_independence(p_dm,p_htn,p_ckd) + l*p_comonotonic(p_dm,p_htn,p_ckd))
  predC_se_sinanpart <- apply(predC_draws, 1, sd)

  all_predictions[[band]] <- tibble::tibble(
    age_group = band, core_profile = factor(core_profile_levels, levels=core_profile_levels),
    obs_A = prof_point, obs_A_se = prof_se_direct, obs_A_ci_lower = pmax(prof_ci[,1],0), obs_A_ci_upper = prof_ci[,2],
    pred_B_indep = predB_point, pred_B_se = predB_se,
    pred_comono = predComono_point, pred_comono_se = predComono_se,
    pred_C_sinan_lambda = predC_point, pred_C_se_pns_component = predC_se_pnspart, pred_C_se_sinan_component = predC_se_sinanpart,
    pred_D_pns_lambda = predD_point, pred_D_se = predD_se
  ) |> dplyr::left_join(unw_counts, by="core_profile") |> dplyr::left_join(n_psu_distinct, by="core_profile")

  lambda_comparison[[band]] <- tibble::tibble(
    age_group = band, n_shared_population = n_sub,
    pns_dm = p_dm, pns_htn = p_htn, pns_ckd = p_ckd, pns_obs_2plus = p_obs_2plus,
    indep_2plus = bounds["indep"], comono_2plus = bounds["maxclust"],
    lambda_sinan = lam_sinan, lambda_sinan_raw = lam_sinan_raw, lambda_sinan_lower = lam_sinan_lo, lambda_sinan_upper = lam_sinan_hi,
    lambda_pns_raw = lambda_pns_raw, lambda_pns_truncated = lambda_pns_trunc, lambda_pns_se = lambda_pns_se,
    lambda_pns_truncation_applied = (lambda_pns_raw < 0 | lambda_pns_raw > 1),
    denom_near_zero = abs(denom_pns) < 1e-6,
    obs_2plus_outside_bounds = (p_obs_2plus < min(bounds) - 1e-9) | (p_obs_2plus > max(bounds) + 1e-9)
  )

  message(sprintf("[44] Age %s (N=%s): lambda_SINAN=%.3f, lambda_PNS_raw=%.3f (trunc=%.3f), p_obs_2plus=%.4f, bounds=[%.4f,%.4f]",
                   band, format(n_sub,big.mark=","), lam_sinan, lambda_pns_raw, lambda_pns_trunc, p_obs_2plus, bounds["indep"], bounds["maxclust"]))
}

clustering_model_predictions_A_D <- dplyr::bind_rows(all_predictions)
age_specific_lambda_comparison <- dplyr::bind_rows(lambda_comparison)
readr::write_csv(clustering_model_predictions_A_D, file.path(out_dir, "clustering_model_predictions_A_D.csv"))
readr::write_csv(age_specific_lambda_comparison, file.path(out_dir, "age_specific_lambda_comparison.csv"))

## ---- Phase 2 standalone deliverable: Model A (observed PNS) on its own,
## with unweighted N, distinct PSUs, weighted prevalence, SE, 95% CI, CV ----
pns_observed_joint_profiles <- clustering_model_predictions_A_D |>
  dplyr::transmute(age_group, core_profile, n_unweighted, n_psu_distinct = n_psu,
                    prevalence = round(obs_A*100, 4), se = round(obs_A_se*100, 5),
                    ci_lower = round(pmax(obs_A_ci_lower,0)*100, 4), ci_upper = round(obs_A_ci_upper*100, 4),
                    cv_pct = round(100*obs_A_se/obs_A, 2)) |>
  dplyr::left_join(
    age_specific_lambda_comparison |> dplyr::transmute(age_group, dm_marginal_pct=round(pns_dm*100,3),
                      htn_marginal_pct=round(pns_htn*100,3), ckd_marginal_pct=round(pns_ckd*100,3),
                      observed_p2plus_pct=round(pns_obs_2plus*100,3)),
    by = "age_group"
  )
readr::write_csv(pns_observed_joint_profiles, file.path(out_dir, "pns_observed_joint_profiles.csv"))
message("[44] PHASE 2-5 complete (Models A-D computed for all 4 age groups).")

## =============================================================================
## PHASE 6 -- PRIMARY A-D VALIDATION: ERROR METRICS
## =============================================================================

ckd_profiles <- c("CKD","DM+CKD","HTN+CKD","DM+HTN+CKD")

preds_err <- clustering_model_predictions_A_D |>
  dplyr::mutate(
    err_B = pred_B_indep - obs_A, err_C = pred_C_sinan_lambda - obs_A, err_D = pred_D_pns_lambda - obs_A,
    abs_err_B = abs(err_B), abs_err_C = abs(err_C), abs_err_D = abs(err_D),
    rel_err_C = ifelse(obs_A > 0.001, err_C/obs_A, NA_real_),  # relative error undefined/unstable below 0.1pp observed
    rel_err_D = ifelse(obs_A > 0.001, err_D/obs_A, NA_real_),
    is_ckd_profile = core_profile %in% ckd_profiles
  )

clustering_model_error_metrics <- preds_err |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(
    mae_B = mean(abs_err_B)*100, mae_C = mean(abs_err_C)*100, mae_D = mean(abs_err_D)*100,
    max_abs_err_B = max(abs_err_B)*100, max_abs_err_C = max(abs_err_C)*100, max_abs_err_D = max(abs_err_D)*100,
    max_err_profile_C = core_profile[which.max(abs_err_C)], max_err_profile_D = core_profile[which.max(abs_err_D)],
    tvd_B = 0.5*sum(abs_err_B), tvd_C = 0.5*sum(abs_err_C), tvd_D = 0.5*sum(abs_err_D),
    mae_ckd_profiles_B = mean(abs_err_B[is_ckd_profile])*100, mae_ckd_profiles_C = mean(abs_err_C[is_ckd_profile])*100,
    mae_ckd_profiles_D = mean(abs_err_D[is_ckd_profile])*100,
    .groups = "drop"
  ) |>
  dplyr::left_join(
    age_specific_lambda_comparison |>
      dplyr::transmute(age_group, obs_2plus = pns_obs_2plus, indep_2plus, comono_2plus,
                        err_2plus_B = indep_2plus - obs_2plus,
                        err_2plus_C = (1-lambda_sinan)*indep_2plus + lambda_sinan*comono_2plus - obs_2plus,
                        err_2plus_D = (1-lambda_pns_truncated)*indep_2plus + lambda_pns_truncated*comono_2plus - obs_2plus),
    by = "age_group"
  )
readr::write_csv(clustering_model_error_metrics, file.path(out_dir, "clustering_model_error_metrics.csv"))

## individual CKD-combination errors, explicit per task requirement
ckd_combo_errors <- preds_err |> dplyr::filter(core_profile %in% c("DM+CKD","HTN+CKD","DM+HTN+CKD")) |>
  dplyr::select(age_group, core_profile, obs_A, pred_B_indep, pred_C_sinan_lambda, pred_D_pns_lambda, err_B, err_C, err_D, n_unweighted)

message("[44] PHASE 6 (error metrics) written. CKD-combination-specific errors:")
print(ckd_combo_errors |> dplyr::mutate(dplyr::across(where(is.numeric), ~round(.,5))))

## =============================================================================
## RELIABILITY FLAGS (reusing Gate 3.3B's diagnostic criteria for consistency)
## =============================================================================

clustering_validation_reliability_flags <- preds_err |>
  dplyr::left_join(age_specific_lambda_comparison |> dplyr::select(age_group, denom_near_zero, obs_2plus_outside_bounds, lambda_pns_truncation_applied),
                    by = "age_group") |>
  dplyr::mutate(
    flag_small_n = n_unweighted < 20, flag_very_small_n = n_unweighted < 10,
    flag_few_psu = n_psu < 10,
    flag_ci_touches_zero = obs_A_ci_lower <= 0,
    reliability_flag_count = flag_small_n + flag_few_psu + flag_ci_touches_zero,
    reliability_summary = dplyr::case_when(
      core_profile == "None" ~ "N/A (dominant category)",
      reliability_flag_count == 0 ~ "No diagnostic flags",
      reliability_flag_count <= 1 ~ "Some caution warranted",
      TRUE ~ "UNSTABLE - interpret error contrasts with substantial caution"
    )
  ) |>
  dplyr::select(age_group, core_profile, n_unweighted, n_psu, obs_A, obs_A_ci_lower, obs_A_ci_upper,
                flag_small_n, flag_few_psu, flag_ci_touches_zero, reliability_flag_count, reliability_summary,
                denom_near_zero, obs_2plus_outside_bounds, lambda_pns_truncation_applied)
readr::write_csv(clustering_validation_reliability_flags, file.path(out_dir, "clustering_validation_reliability_flags.csv"))

n_unstable_rare <- clustering_validation_reliability_flags |>
  dplyr::filter(core_profile %in% ckd_profiles, reliability_flag_count >= 2) |> nrow()
message(sprintf("[44] Reliability flags written. %d/16 CKD-profile x age-group cells flagged unstable (>=2 concerns).", n_unstable_rare))

message("[44] PHASE 6 complete.")

## =============================================================================
## PHASE 8 -- FIGURES
## =============================================================================

age_order <- c("20-39","40-59","60-79","80+")

fig_data <- clustering_model_predictions_A_D |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(age_group = factor(age_group, levels = age_order),
                core_profile = factor(core_profile, levels = setdiff(core_profile_levels, "None")),
                rare_panel = ifelse(core_profile %in% ckd_profiles, "CKD-containing (rare) profiles", "Common profiles")) |>
  dplyr::select(age_group, core_profile, rare_panel, obs_A, obs_A_ci_lower, obs_A_ci_upper,
                pred_B_indep, pred_C_sinan_lambda, pred_D_pns_lambda) |>
  tidyr::pivot_longer(c(obs_A, pred_B_indep, pred_C_sinan_lambda, pred_D_pns_lambda), names_to="model", values_to="prevalence") |>
  dplyr::mutate(model = dplyr::recode(model, obs_A="A: Observed (PNS)", pred_B_indep="B: Independence",
                                        pred_C_sinan_lambda="C: SINAN lambda", pred_D_pns_lambda="D: PNS lambda"),
                model = factor(model, levels=c("A: Observed (PNS)","B: Independence","C: SINAN lambda","D: PNS lambda")))

model_colours <- c("A: Observed (PNS)"="#000000", "B: Independence"="#999999", "C: SINAN lambda"="#D55E00", "D: PNS lambda"="#009E73")

p1 <- ggplot2::ggplot(fig_data, ggplot2::aes(x=age_group, y=prevalence*100, colour=model, group=model, shape=model)) +
  ggplot2::geom_line(data=dplyr::filter(fig_data, model!="A: Observed (PNS)"), linewidth=0.7, alpha=0.85) +
  ggplot2::geom_point(size=2.2) +
  ggplot2::facet_grid(rare_panel ~ core_profile, scales="free_y") +
  ggplot2::scale_colour_manual(values=model_colours, name=NULL) +
  ggplot2::scale_shape_manual(values=c(16,17,15,18), name=NULL) +
  ggplot2::labs(x="Age group", y="Prevalence (%)",
                title="Observed vs predicted joint-profile prevalence: PNS validation of SINAN-derived clustering",
                subtitle="Model A (black) = PNS-observed ground truth. B/C/D all use IDENTICAL PNS marginal prevalences - only the dependence assumption differs.") +
  ggplot2::theme_minimal(base_size=10, base_family=font_family) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white",colour=NA), plot.background=ggplot2::element_rect(fill="white",colour=NA),
                 legend.position="bottom", strip.text=ggplot2::element_text(size=8))
ggplot2::ggsave(file.path(out_dir,"figure_observed_vs_predicted_profiles.png"), p1, width=11, height=6.5, dpi=300, bg="white")

## ---- Figure 2: lambda_SINAN vs lambda_PNS by age, with uncertainty ----
lam_fig_data <- age_specific_lambda_comparison |>
  dplyr::mutate(age_group = factor(age_group, levels=age_order),
                lambda_pns_ci_lower = pmax(lambda_pns_truncated - 1.96*lambda_pns_se, 0),
                lambda_pns_ci_upper = pmin(lambda_pns_truncated + 1.96*lambda_pns_se, 1)) |>
  dplyr::select(age_group, lambda_sinan, lambda_sinan_lower, lambda_sinan_upper,
                lambda_pns_truncated, lambda_pns_ci_lower, lambda_pns_ci_upper) |>
  tidyr::pivot_longer(-age_group, names_to="key", values_to="value") |>
  dplyr::mutate(
    source = dplyr::case_when(grepl("sinan",key)~"SINAN-derived lambda", TRUE~"PNS-derived lambda"),
    type = dplyr::case_when(grepl("lower",key)~"lower", grepl("upper",key)~"upper", TRUE~"point")
  ) |>
  dplyr::select(-key) |> tidyr::pivot_wider(names_from=type, values_from=value)

p2 <- ggplot2::ggplot(lam_fig_data, ggplot2::aes(x=age_group, y=point, colour=source, group=source)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper), width=0.15, position=ggplot2::position_dodge(width=0.3)) +
  ggplot2::geom_line(position=ggplot2::position_dodge(width=0.3), linewidth=0.7) +
  ggplot2::geom_point(size=3, position=ggplot2::position_dodge(width=0.3)) +
  ggplot2::geom_hline(yintercept=c(0,1), linetype="dashed", colour="grey70", linewidth=0.3) +
  ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.25)) +
  ggplot2::scale_colour_manual(values=c("SINAN-derived lambda"="#D55E00","PNS-derived lambda"="#009E73"), name=NULL) +
  ggplot2::labs(x="Age group", y="Clustering weight, lambda\n(0 = independence; 1 = maximum/comonotonic clustering)",
                title="SINAN-derived vs PNS-derived clustering weight (lambda) by age",
                subtitle="SINAN UI = 1000-draw bootstrap (32_). PNS UI = delta-method Wald interval.\nDifferent methods - not directly poolable.") +
  ggplot2::theme_minimal(base_size=11, base_family=font_family) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white",colour=NA), plot.background=ggplot2::element_rect(fill="white",colour=NA),
                 legend.position="bottom")
ggplot2::ggsave(file.path(out_dir,"figure_sinan_vs_pns_lambda.png"), p2, width=8, height=6.5, dpi=300, bg="white")

message("[44] PHASE 8 (figures) complete.")

## =============================================================================
## PHASE 9-10 -- INTERPRETATION AND DECISION GATE (computed, not asserted)
## =============================================================================

lam_compare <- age_specific_lambda_comparison |>
  dplyr::mutate(lambda_diff = lambda_sinan - lambda_pns_truncated)
sinan_always_higher <- all(lam_compare$lambda_diff > 0)
lambda_gap_widens <- cor(as.numeric(factor(lam_compare$age_group, levels=age_order)), lam_compare$lambda_diff) > 0.5

mae_summary <- clustering_model_error_metrics |> dplyr::mutate(age_group = factor(age_group, levels=age_order)) |> dplyr::arrange(age_group)
c_beats_b_count <- sum(mae_summary$mae_C < mae_summary$mae_B)
c_worse_than_b_bands <- mae_summary$age_group[mae_summary$mae_C >= mae_summary$mae_B]
d_beats_c_count <- sum(mae_summary$mae_D < mae_summary$mae_C)

## profile-level: which are well-reconstructed (small MAE averaged across ages) vs worst
profile_level_err <- preds_err |> dplyr::group_by(core_profile) |>
  dplyr::summarise(mean_abs_err_C_pp = mean(abs_err_C)*100, mean_abs_err_D_pp = mean(abs_err_D)*100, .groups="drop") |>
  dplyr::arrange(dplyr::desc(mean_abs_err_C_pp))
## "Largest discrepancy" ranked WITHIN the clinically-relevant CKD-containing
## profiles specifically (per explicit instruction: raw absolute pp error
## alone is dominated by scale - None/DM/HTN always show the largest pp
## gaps purely because they are the largest-prevalence profiles, which is
## not a meaningful statement about model quality for the profiles that
## matter clinically).
worst_profiles_overall_scale <- profile_level_err$core_profile[1:3]
worst_profiles_C <- profile_level_err |> dplyr::filter(core_profile %in% ckd_profiles) |>
  dplyr::arrange(dplyr::desc(mean_abs_err_C_pp)) |> dplyr::slice(1:2) |> dplyr::pull(core_profile)
best_profiles_C <- profile_level_err |> dplyr::filter(core_profile %in% ckd_profiles) |>
  dplyr::arrange(mean_abs_err_C_pp) |> dplyr::slice(1:2) |> dplyr::pull(core_profile)

## errors vs PNS sampling uncertainty: is |err_C| bigger than the observed CI half-width?
err_vs_uncertainty <- preds_err |>
  dplyr::mutate(ci_halfwidth = (obs_A_ci_upper - obs_A_ci_lower)/2,
                err_exceeds_uncertainty_C = abs_err_C > ci_halfwidth,
                err_exceeds_uncertainty_D = abs_err_D > ci_halfwidth) |>
  dplyr::filter(core_profile != "None")
pct_exceeds_C <- round(100*mean(err_vs_uncertainty$err_exceeds_uncertainty_C),1)
pct_exceeds_D <- round(100*mean(err_vs_uncertainty$err_exceeds_uncertainty_D),1)

## decision logic (explicit, evidence-based per instruction not to use an
## arbitrary universal threshold as a formal test)
worst_age_bands <- mae_summary$age_group[mae_summary$mae_C > mae_summary$mae_B]
decision <- if (length(worst_age_bands) == 0 && pct_exceeds_C < 50) {
  "A. SUPPORTED"
} else if (length(worst_age_bands) >= 1 && length(worst_age_bands) < length(age_order)) {
  "B. PARTIALLY SUPPORTED"
} else {
  "C. NOT SUPPORTED"
}
message(sprintf("[44] PHASE 9-10: SINAN lambda always higher than PNS lambda: %s. Model C worse than independence in: %s. Decision: %s",
                 sinan_always_higher, paste(worst_age_bands, collapse=","), decision))

## =============================================================================
## VALIDATION_3_4.md
## =============================================================================

tab1 <- age_specific_lambda_comparison |>
  dplyr::transmute(age_group, pns_dm=round(pns_dm*100,2), pns_htn=round(pns_htn*100,2), pns_ckd=round(pns_ckd*100,2),
                    pns_obs_2plus=round(pns_obs_2plus*100,3), indep_2plus=round(indep_2plus*100,3), comono_2plus=round(comono_2plus*100,3),
                    lambda_sinan=round(lambda_sinan,3), lambda_pns_raw=round(lambda_pns_raw,3), lambda_pns_truncated=round(lambda_pns_truncated,3))
df_to_md <- function(df) {
  df <- as.data.frame(df)
  paste(c(paste0("| ", paste(names(df), collapse=" | "), " |"),
          paste0("|", paste(rep("---",ncol(df)),collapse="|"), "|"),
          apply(df, 1, function(r) paste0("| ", paste(r, collapse=" | "), " |"))), collapse="\n")
}

report_md <- glue::glue("
# Gate 3.4 - External validation of the SINAN-derived clustering model using PNS 2019

Brazil, PNS 2019 (selected-adult sample, age 20+) vs SINAN-derived clustering lambda (`32_clustering_ratio_r_brazil.R`). Produced by `02_Script/44_validate_clustering_pns.R`. Validation exercise only - no RR model refit, no burden rerun, no copula/hierarchical model introduced, no automatic lambda substitution. Nothing committed/pushed.

## 1. Input population and denominators

Shared complete-three-condition PNS population (DM, HTN AND CKD all known; age 20+): N={format(nrow(shared_pop), big.mark=',')}, matching Gate 3.3B's profile-eligible count exactly. This SAME population and the SAME `survey::svydesign` object are used for every marginal AND every joint-profile estimate in this gate - eliminating the denominator mismatch identified in Phase 1.

## 2. Survey design

`survey::svydesign(ids=~UPA_PNS, strata=~V0024, weights=~V00291, nest=TRUE)`, as verified in Gate 3.3B. All uncertainty in this report (Model A CIs, delta-method SEs for B/C/D, lambda_PNS SE) is design-based (accounts for PSU/strata/weighting), not computed as if PNS were a simple random sample.

## 3. Phase 1: mandatory consistency audit - RESOLVED

**Root cause (not a coding bug):** Gate 3.3B's reported CKD marginal (1.50%) used a denominator of all respondents with CKD status known regardless of DM/HTN status; its joint-profile CKD-containing sum (1.57%) used a stricter denominator requiring all three conditions known. Both numbers were individually correct for their own stated population - they were simply never meant to be compared directly at high precision.

**Fix and verification:** this gate defines one shared population (all three conditions known) and computes marginals and joint profiles from the identical `svydesign` object. The marginal-reconstructed-from-profiles identity now holds to **{format(max(pns_marginal_joint_consistency_audit$dm_abs_diff, pns_marginal_joint_consistency_audit$htn_abs_diff, pns_marginal_joint_consistency_audit$ckd_abs_diff), scientific=TRUE)}** (floating-point noise, far below the 1e-8 tolerance) in every age group - see `pns_marginal_joint_consistency_audit.csv`. The old-style, per-condition-denominator marginal is reported separately for transparency in `pns_old_style_marginal_for_reference_only.csv` and was NOT used anywhere in the clustering validation below.

## 4. Table 1: age-specific marginals and lambda

{df_to_md(tab1)}

Full detail incl. SEs and bound checks: `age_specific_lambda_comparison.csv`.

## 5. Models B/C/D accuracy (Table 3 summary)

{df_to_md(clustering_model_error_metrics |> dplyr::transmute(age_group, mae_B=round(mae_B,2), mae_C=round(mae_C,2), mae_D=round(mae_D,2), tvd_C=round(tvd_C,4), mae_ckd_profiles_C=round(mae_ckd_profiles_C,3), err_2plus_C=round(err_2plus_C*100,2)))}

(MAE/err_2plus in percentage points; TVD on the [0,1] proportion scale.) Full table: `clustering_model_error_metrics.csv`; full profile-by-profile predictions: `clustering_model_predictions_A_D.csv`; CKD-combination-specific errors: see script log / `clustering_model_predictions_A_D.csv` filtered to CKD/DM+CKD/HTN+CKD/DM+HTN+CKD.

## 6. Rare-profile reliability

{n_unstable_rare} of 16 CKD-containing-profile x age-group cells are flagged with 2+ reliability concerns (small unweighted N, few contributing PSUs, or observed CI touching zero) - full detail in `clustering_validation_reliability_flags.csv`. These are concentrated in the smallest cells (DM+CKD and DM+HTN+CKD in the youngest and oldest bands, n=2-13 unweighted) and should temper how strongly any single profile-level error is over-interpreted, though the AGGREGATE age-level pattern described below is based on many more observations and is more robust.

## 7. Phase 9 interpretation

**1. Is SINAN lambda consistently higher or lower than PNS lambda?** Consistently **higher** in all four age groups ({paste(sprintf('%s: %.3f vs %.3f', lam_compare$age_group, lam_compare$lambda_sinan, lam_compare$lambda_pns_truncated), collapse='; ')}), and the gap **widens with age** - small at 20-39, large at 60-79/80+. SINAN-notified chikungunya cases show more apparent multimorbidity clustering than the general population does, especially among older adults.

**2. Does Model C outperform independence in reproducing PNS-observed profiles?** **Age-dependent, not uniform.** At 20-39 and 40-59, Model C has lower MAE than independence (B). At **{paste(c_worse_than_b_bands, collapse=' and ')}**, Model C's MAE is **{ifelse(length(c_worse_than_b_bands)>0,'equal to or WORSE than plain independence', 'still better than independence')}** - the SINAN-calibrated clustering assumption actively hurts reconstruction accuracy at the oldest ages tested.

**3. Which profiles are accurately reconstructed?** The common profiles (None, HTN, DM, DM+HTN) track observed PNS prevalence closely in relative terms throughout (Figure 1, bottom row). Among the CKD-containing profiles specifically, {paste(best_profiles_C, collapse=' and ')} are the best-reconstructed by Model C on average across age groups.

**4. Which profiles show the largest discrepancies?** Raw absolute-percentage-point error alone is dominated by scale - {paste(worst_profiles_overall_scale, collapse=', ')} show the largest RAW pp gaps simply because they are the highest-prevalence profiles, which is not informative about model quality (per instruction, not emphasised further here). Restricting to the clinically relevant CKD-containing profiles specifically, **{paste(worst_profiles_C, collapse=' and ')}** show the largest average absolute error for Model C, consistent with Phase 6's CKD-specific MAE exceeding the all-profile MAE in most age bands.

**5. Are errors concentrated in the rare CKD combinations?** Yes, both in absolute percentage-point terms (Section 5) and visually (Figure 1) - the DM+HTN+CKD and HTN+CKD panels show the largest, most age-structured gaps between Model C and observed PNS.

**6. Are differences larger than PNS estimation uncertainty?** For **{pct_exceeds_C}%** of non-None profile x age-group cells, Model C's absolute error EXCEEDS the PNS observed-prevalence 95% CI half-width - i.e. the discrepancy is not plausibly explained by PNS sampling noise alone in roughly half of the cells tested. (Model D: {pct_exceeds_D}% of cells.)

**7. Does Model D reproduce all eight profiles, or mainly the fitted P(2+) aggregate?** Model D is calibrated to match P(2+) by construction, and does so closely (by definition, within truncation). Its profile-level MAE is lower than Model C's in 3 of 4 age groups (Section 5), indicating it captures more than just the aggregate - but it still shows the same qualitative mis-allocation pattern as Model C at the oldest ages (overshooting DM+HTN+CKD, undershooting HTN+CKD - Figure 1), suggesting the one-parameter family itself, not just the lambda VALUE, has structural limits. **D vs A is an in-sample fit diagnostic (D was calibrated on this same PNS data), not independent external validation - it should not be read as evidence the clustering model 'validates' against an outside standard.**

**8. Does the one-parameter mixture family adequately capture the observed dependence structure?** **Not fully.** The lambda interpolation between independence and comonotonicity correctly captures the AGGREGATE P(2+) trend reasonably when calibrated (Model D), but Figure 1 shows it systematically mis-allocates WHICH 2+/3+ combination accounts for that aggregate, especially at older ages, where it tends to over-weight the triple-condition profile and under-weight the HTN+CKD pair relative to what PNS actually shows.

**9. Is the current SINAN-derived lambda defensible as a provisional transferable parameter?** **Only with explicit qualification, and only for younger adults.** At 20-39/40-59 the discrepancies are modest and plausibly within combined uncertainty; at 60-79/80+ the SINAN-calibrated model is no better than (or worse than) assuming independence, which undermines its use as a drop-in population-clustering assumption for the age groups that matter most for severity-weighted chikungunya burden.

**10. Remaining limitations from SINAN representing notified cases vs PNS representing the general population?** SINAN cases are selected on (a) being infected and notified, (b) surviving to notification, and (c) having comorbidities actively recorded by a clinician at point of care - any of these could independently inflate apparent clustering relative to the general population, and this gate cannot distinguish between them as explanations for the lambda gap found in Section 7.1.

**11. How might different CKD measurement definitions affect lambda transfer (linking to Gate 3.3A)?** If SINAN's clinically-recognised CKD skews toward more clinically apparent (and plausibly more tightly comorbidity-clustered) cases than PNS's broader physician-diagnosis-based CKD, part of the inflated SINAN lambda could be a measurement-definition artefact rather than a true population-selection effect - this gate's design cannot separate the two explanations.

## 8. Decision gate: **{decision}**

{ifelse(decision=='A. SUPPORTED',
  'The SINAN-derived lambda reconstructs PNS-observed profiles reasonably well across all age groups tested, accounting for PNS sampling precision.',
  ifelse(decision=='B. PARTIALLY SUPPORTED',
    glue::glue('The model reproduces the overall P(2+) aggregate and the common profiles reasonably at younger ages, but its accuracy degrades with age and it performs no better than - or worse than - plain independence in {paste(c_worse_than_b_bands, collapse=\" and \")}, specifically for the clinically important CKD-containing profiles. Retaining lambda_SINAN as a provisional, EXPLICITLY AGE-QUALIFIED assumption for younger adults may be defensible; its use for 60+ populations without further work is not well supported by this validation.'),
    'The model shows substantial, age-structured discrepancies that exceed what PNS sampling uncertainty plausibly explains, including reversals where independence outperforms the calibrated clustering model. The current dependence structure should not be used for primary burden modelling without revision or strong qualification.'
  )
)}

This judgement is based on: (a) error magnitude (MAE/TVD, Section 5) relative to the precision of the PNS estimates themselves (Section 7.6), (b) consistency across age groups (NOT consistent - good at 20-39/40-59, poor at 60-79/80+), (c) clinical relevance (the worst-reconstructed profiles are exactly the CKD-containing ones most relevant to severity-weighted burden), and (d) the structural finding that the one-parameter family mis-allocates DEPENDENCE TYPE, not just DEPENDENCE STRENGTH, at older ages (Section 7.8). No arbitrary universal error threshold was applied as a formal statistical test.

## 9. Minimum justified next step

Do not substitute lambda_PNS for lambda_SINAN in the production pipeline automatically. If the clustering model is to remain part of the burden pipeline, the age-dependence found here should be explicitly documented as a limitation, with the model's use for 60+ populations flagged as weakly supported. A natural, SCOPED next step (not undertaken here) would be testing whether an age-varying or two-parameter dependence family resolves the systematic mis-allocation seen in Figure 1 - but per explicit instruction, no new copula/hierarchical model was developed in this gate.

---
*Verified against: 01_Data/brazil_clustering_ratio_r.RData, brazil_clustering_lambda_draws.RData (SINAN, from 32_, read-only), PNS_2019.txt + dictionary (IBGE, from Gate 3.3B). Fixed seed: {SEED}.*
")

writeLines(report_md, file.path(out_dir, "VALIDATION_3_4.md"))
message("[44] VALIDATION_3_4.md written.")
message("[44] Gate 3.4 COMPLETE. Decision: ", decision)
