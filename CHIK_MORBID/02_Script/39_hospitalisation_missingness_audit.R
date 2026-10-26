# ---------------------------------------------------------------------------
# 39_hospitalisation_missingness_audit.R   -- Gate 3.2A
#
# DESCRIPTIVE AUDIT of missing hospitalisation outcome data (HOSPITALIZ ->
# `hospitalised`) among confirmed chikungunya cases in Brazil, 2017-2025.
#
# Scope for this pass (per explicit instruction): descriptive missingness
# characterisation ONLY - recording completeness and selection patterns
# across age/sex/year/state/region/condition/profile/lab-confirmation, and
# recorded-vs-unrecorded characteristic contrasts (standardised differences).
# No IPW-weighted outcome model, no multiple imputation, no MNAR sensitivity
# analysis is implemented here. The optional preliminary recording-
# probability model (original Phase 5) is DEFERRED, not computed in this
# pass, so it cannot delay or dilute the descriptive audit - see
# VALIDATION_3_2A.md section on this decision.
#
# Official SINAN coding (verified against 01_Data/sinan_chik_docs/
# dic_dados_chikungunya.pdf, field 62 "Ocorreu Hospitalizacao?" / HOSPITALIZ):
#   1 = Sim        (yes)
#   2 = Nao         (no)
#   9 = Ignorado    (unknown - the field was actively answered "don't know")
#   blank           (missing - the field was never answered at all)
# 02_clean_chik_sinan_brazil.R's `recode(HOSPITALIZ, map_sim_nao)` already
# handles this correctly (unlike the Gate 3.1 EVOLUCAO bug): code 9 maps to
# the explicit string "unknown", and blank/unmapped codes map to NA via the
# shared `recode()` helper's `out[x == "" | is.na(x)] <- NA_character_` line.
# So `hospitalised` already has FOUR distinct observed states in the cleaned
# data: "yes", "no", "unknown", NA. This script treats both "unknown" and
# NA as "hospitalisation outcome not usable" (R_hosp = 0), matching how
# 12_core3_condition_specific_rr.R's `dplyr::filter(hospitalised %in%
# c("no","yes"))` already excludes both - but reports them SEPARATELY
# throughout, since they are not the same missingness mechanism (one is an
# active "don't know" response, the other is a field never completed).
#
# This script does NOT modify 02_clean_chik_sinan_brazil.R or
# 12_core3_condition_specific_rr.R, does NOT change any primary
# hospitalisation RR, and does NOT rerun 34_-37_. It independently
# reconstructs the eligibility criteria from 12_ (Phase 2) and re-derives
# region from the same muni-code convention already used in
# 11_population_burden_integration.R.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds
# Output: 03_Output/validation/gate32a/cohort_reconciliation.csv
#         03_Output/validation/gate32a/hospitalisation_status_overall.csv
#         03_Output/validation/gate32a/hospitalisation_missingness_by_age_profile.csv
#         03_Output/validation/gate32a/hospitalisation_missingness_by_year_state.csv
#         03_Output/validation/gate32a/recorded_vs_unrecorded_characteristics.csv
#         03_Output/validation/gate32a/recording_probability_diagnostics.csv (placeholder - deferred)
#         03_Output/validation/gate32a/diagnostic_ipw_weight_distribution.csv (placeholder - deferred)
#         03_Output/validation/gate32a/figure_missingness_by_profile.png
#         03_Output/validation/gate32a/figure_missingness_by_year.png
#         03_Output/validation/gate32a/figure_state_year_missingness_heatmap.png
#         03_Output/validation/gate32a/VALIDATION_3_2A.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(lubridate); library(MASS)
})
options(scipen = 999, dplyr.summarise.inform = FALSE)

val_dir <- "03_Output/validation/gate32a"
if (!dir.exists(val_dir)) dir.create(val_dir, recursive = TRUE)

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
profile_age_levels  <- c("0-19","20-39","40-59","60-79","80+")

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
## PHASE 1 -- HOSPITALIZ CODING (verified against the dictionary in comments
## above). No code changes. Just confirm the four observed states exist.
## =============================================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

message(sprintf("[39] raw hospitalised value counts (all %s rows, before any eligibility filter):",
                 format(nrow(ind), big.mark = ",")))
print(table(ind$hospitalised, useNA = "always"))

## =============================================================================
## PHASE 2 -- RECONSTRUCT THE ELIGIBLE COHORT (independent of 12_, same logic)
## =============================================================================

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
    year = lubridate::year(event_date),
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
    age_band_5  = cut(age_years, breaks = c(0,20,40,60,80,101),
                       labels = profile_age_levels, right = FALSE, include.lowest = TRUE),
    age_band_10 = cut(age_years, breaks = c(seq(0,90,10),101),
                       labels = c(paste(seq(0,80,10), seq(9,89,10), sep="-"), "90+"),
                       right = FALSE, include.lowest = TRUE),
    muni_any = dplyr::coalesce(muni_residence6, muni_notif6),
    region = dplyr::case_when(
      substr(muni_any,1,1) == "1" ~ "Norte",
      substr(muni_any,1,1) == "2" ~ "Nordeste",
      substr(muni_any,1,1) == "3" ~ "Sudeste",
      substr(muni_any,1,1) == "4" ~ "Sul",
      substr(muni_any,1,1) == "5" ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    region = factor(region, levels = c("Sudeste","Nordeste","Norte","Centro-Oeste","Sul")),
    R_hosp = dplyr::if_else(hospitalised %in% c("no","yes"), 1L, 0L),
    hosp_missing_type = dplyr::case_when(
      hospitalised %in% c("no","yes") ~ "recorded",
      hospitalised == "unknown"       ~ "explicit_unknown",
      is.na(hospitalised)             ~ "blank_unrecorded",
      TRUE ~ "other_unmapped"
    )
  ) |>
  dplyr::left_join(uf_lookup |> dplyr::select(uf_residence, state_abbrev), by = "uf_residence")

stopifnot(nrow(eligible) == 1074796)
n_eligible <- nrow(eligible)

hosp_known <- eligible |> dplyr::filter(R_hosp == 1) |>
  dplyr::mutate(hosp_only = hospitalised == "yes")
stopifnot(nrow(hosp_known) == 776072)
stopifnot(sum(hosp_known$hosp_only) == 25363)

message(sprintf("[39] eligible core-3 cohort N = %s (expected 1,074,796) -- PASS",
                 format(n_eligible, big.mark=",")))
message(sprintf("[39] hospitalisation-known cohort N = %s (expected 776,072) -- PASS",
                 format(nrow(hosp_known), big.mark=",")))

## ---- silent-exclusion check: does uf_residence (the GAM's state random
## effect) drop any additional rows when fit_hosp_core3 actually runs? ----
n_uf_na_eligible   <- sum(is.na(eligible$uf_residence) | eligible$uf_residence == "")
n_uf_na_hospknown  <- sum(is.na(hosp_known$uf_residence) | hosp_known$uf_residence == "")
n_age_na_hospknown <- sum(is.na(hosp_known$age_years))
n_sex_na_hospknown <- sum(is.na(hosp_known$sex))
n_year_na_hospknown<- sum(is.na(hosp_known$year))
n_dmhtnckd_na      <- sum(is.na(hosp_known$dm) | is.na(hosp_known$htn) | is.na(hosp_known$ckd))
expected_fitted_n  <- nrow(hosp_known) - length(union(union(union(
  which(is.na(hosp_known$uf_residence) | hosp_known$uf_residence==""),
  which(is.na(hosp_known$age_years))),
  which(is.na(hosp_known$sex))),
  which(is.na(hosp_known$year))))

message(sprintf("[39] uf_residence missing within eligible cohort: %d", n_uf_na_eligible))
message(sprintf("[39] uf_residence missing within hosp-known cohort: %d", n_uf_na_hospknown))
message(sprintf("[39] expected fit_hosp_core3 fitted N (no silent covariate drop expected): %s",
                 format(expected_fitted_n, big.mark=",")))

cohort_reconciliation <- tibble::tribble(
  ~step, ~description, ~n, ~pct_of_eligible, ~notes,
  1, "Core-three eligible population", n_eligible, 100,
     "is_confirmed_chik & year>=2017 & age 0-100 & sex known & DM/HTN/CKD all explicit yes/no",
  2, "Hospitalisation outcome recorded (yes/no)", nrow(hosp_known),
     round(100*nrow(hosp_known)/n_eligible,2),
     "R_hosp=1; matches 12_core3_condition_specific_rr.R's hosp_cohort exactly",
  3, "Hospitalisation outcome NOT usable (R_hosp=0), total", n_eligible-nrow(hosp_known),
     round(100*(n_eligible-nrow(hosp_known))/n_eligible,2),
     "sum of rows 4+5 below",
  4, "  .. of which: explicit 'unknown' (HOSPITALIZ=9, Ignorado)",
     sum(eligible$hosp_missing_type=="explicit_unknown"),
     round(100*sum(eligible$hosp_missing_type=="explicit_unknown")/n_eligible,2),
     "field was actively answered 'do not know'",
  5, "  .. of which: blank / never recorded (HOSPITALIZ missing)",
     sum(eligible$hosp_missing_type=="blank_unrecorded"),
     round(100*sum(eligible$hosp_missing_type=="blank_unrecorded")/n_eligible,2),
     "field was never completed - distinct mechanism from row 4",
  6, "Hospitalisation events among recorded (hosp-known) cohort", sum(hosp_known$hosp_only),
     round(100*sum(hosp_known$hosp_only)/nrow(hosp_known),2),
     "pct_of_eligible column here is instead pct hospitalised WITHIN the known cohort",
  7, "uf_residence missing within eligible cohort", n_uf_na_eligible,
     round(100*n_uf_na_eligible/n_eligible,2),
     "state random-effect covariate used by fit_hosp_core3's s(uf_residence,bs='re')",
  8, "uf_residence missing within hospitalisation-known cohort", n_uf_na_hospknown,
     round(100*n_uf_na_hospknown/nrow(hosp_known),2),
     "if >0, fit_hosp_core3's actual fitted N would be LESS than row 2's N",
  9, "age_years / sex / year missing within hospitalisation-known cohort",
     n_age_na_hospknown + n_sex_na_hospknown + n_year_na_hospknown, 0,
     "all should be 0 by construction (eligibility already requires these non-missing)",
  10, "Expected fit_hosp_core3 fitted-model N (eligible hosp-known minus any covariate NA)",
     expected_fitted_n, round(100*expected_fitted_n/n_eligible,2),
     "CONCLUSION: see VALIDATION_3_2A.md Finding 1 -- equals row 2 exactly if row 8 = 0"
)
readr::write_csv(cohort_reconciliation, file.path(val_dir, "cohort_reconciliation.csv"))

## =============================================================================
## PHASE 3A -- OVERALL HOSPITALISATION STATUS
## =============================================================================

hospitalisation_status_overall <- eligible |>
  dplyr::count(hospitalised, hosp_missing_type, name = "n") |>
  dplyr::mutate(pct_of_eligible = round(100*n/n_eligible, 3)) |>
  dplyr::arrange(dplyr::desc(n))
readr::write_csv(hospitalisation_status_overall, file.path(val_dir, "hospitalisation_status_overall.csv"))

## =============================================================================
## PHASE 3B -- MISSINGNESS BY PATIENT CHARACTERISTIC (age / sex / conditions /
## profile / lab confirmation)
## =============================================================================

missingness_table <- function(df, group_var, domain_name) {
  gv <- rlang::ensym(group_var)
  df |>
    dplyr::group_by(level = !!gv) |>
    dplyr::summarise(
      n_eligible = dplyr::n(),
      n_known    = sum(R_hosp == 1),
      n_explicit_unknown = sum(hosp_missing_type == "explicit_unknown"),
      n_blank_unrecorded = sum(hosp_missing_type == "blank_unrecorded"),
      n_hosp_events_among_known = sum(R_hosp==1 & hospitalised=="yes"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      stratifier_domain = domain_name,
      pct_known   = round(100*n_known/n_eligible, 2),
      pct_missing = round(100*(n_eligible-n_known)/n_eligible, 2),
      pct_hosp_among_known = round(100*n_hosp_events_among_known/pmax(n_known,1), 2),
      sparse_flag = n_eligible < 30,
      near_complete_nonrecording_flag = pct_missing > 95
    ) |>
    dplyr::relocate(stratifier_domain, level) |>
    dplyr::arrange(stratifier_domain, level)
}

by_age5    <- missingness_table(eligible, age_band_5, "age_band_5")
by_age10   <- missingness_table(eligible, age_band_10, "age_band_10")
by_sex     <- missingness_table(eligible, sex, "sex")
by_dm      <- missingness_table(eligible |> dplyr::mutate(dm_f = ifelse(dm==1,"DM present","DM absent")), dm_f, "diabetes")
by_htn     <- missingness_table(eligible |> dplyr::mutate(htn_f = ifelse(htn==1,"HTN present","HTN absent")), htn_f, "hypertension")
by_ckd     <- missingness_table(eligible |> dplyr::mutate(ckd_f = ifelse(ckd==1,"CKD present","CKD absent")), ckd_f, "renal_disease")
by_profile <- missingness_table(eligible, core_profile, "core_profile")
by_lab     <- missingness_table(eligible |> dplyr::mutate(criterion_f = ifelse(is.na(criterion),"NA (unmapped/blank)", criterion)), criterion_f, "lab_confirmation_criterion")

by_age_profile <- dplyr::bind_rows(
  by_age5 |> dplyr::mutate(level = as.character(level)),
  by_age10 |> dplyr::mutate(level = as.character(level)),
  by_sex |> dplyr::mutate(level = as.character(level)),
  by_dm, by_htn, by_ckd,
  by_profile |> dplyr::mutate(level = as.character(level)),
  by_lab
)
readr::write_csv(by_age_profile, file.path(val_dir, "hospitalisation_missingness_by_age_profile.csv"))

## =============================================================================
## PHASE 3C -- MISSINGNESS BY YEAR / STATE / REGION + CROSS-TABS
## =============================================================================

by_year    <- missingness_table(eligible, year, "calendar_year") |> dplyr::mutate(level = as.character(level))
by_state   <- missingness_table(eligible |> dplyr::mutate(state_f = dplyr::coalesce(state_abbrev, paste0("UF_",uf_residence))), state_f, "state")
by_region  <- missingness_table(eligible, region, "region") |> dplyr::mutate(level = as.character(level))

year_x_state <- eligible |>
  dplyr::mutate(state_f = dplyr::coalesce(state_abbrev, paste0("UF_",uf_residence))) |>
  dplyr::group_by(year, state_f) |>
  dplyr::summarise(n_eligible = dplyr::n(), n_known = sum(R_hosp==1),
                    n_hosp_events_among_known = sum(R_hosp==1 & hospitalised=="yes"), .groups="drop") |>
  dplyr::mutate(
    stratifier_domain = "year_x_state",
    level = paste(year, state_f, sep = " | "),
    n_explicit_unknown = NA_integer_, n_blank_unrecorded = NA_integer_,
    pct_known = round(100*n_known/n_eligible,2),
    pct_missing = round(100*(n_eligible-n_known)/n_eligible,2),
    pct_hosp_among_known = round(100*n_hosp_events_among_known/pmax(n_known,1),2),
    sparse_flag = n_eligible < 30,
    near_complete_nonrecording_flag = pct_missing > 95
  ) |>
  dplyr::select(stratifier_domain, level, n_eligible, n_known, n_explicit_unknown, n_blank_unrecorded,
                 n_hosp_events_among_known, pct_known, pct_missing, pct_hosp_among_known,
                 sparse_flag, near_complete_nonrecording_flag)

year_x_profile <- eligible |>
  dplyr::group_by(year, core_profile) |>
  dplyr::summarise(n_eligible = dplyr::n(), n_known = sum(R_hosp==1),
                    n_hosp_events_among_known = sum(R_hosp==1 & hospitalised=="yes"), .groups="drop") |>
  dplyr::mutate(
    stratifier_domain = "year_x_profile",
    level = paste(year, core_profile, sep = " | "),
    n_explicit_unknown = NA_integer_, n_blank_unrecorded = NA_integer_,
    pct_known = round(100*n_known/n_eligible,2),
    pct_missing = round(100*(n_eligible-n_known)/n_eligible,2),
    pct_hosp_among_known = round(100*n_hosp_events_among_known/pmax(n_known,1),2),
    sparse_flag = n_eligible < 30,
    near_complete_nonrecording_flag = pct_missing > 95
  ) |>
  dplyr::select(stratifier_domain, level, n_eligible, n_known, n_explicit_unknown, n_blank_unrecorded,
                 n_hosp_events_among_known, pct_known, pct_missing, pct_hosp_among_known,
                 sparse_flag, near_complete_nonrecording_flag)

by_year_state <- dplyr::bind_rows(by_year, by_state, by_region, year_x_state, year_x_profile)
readr::write_csv(by_year_state, file.path(val_dir, "hospitalisation_missingness_by_year_state.csv"))

## =============================================================================
## PHASE 4 -- RECORDED VS UNRECORDED CHARACTERISTICS (standardised differences)
## =============================================================================

smd_binary <- function(p1, p0) {
  pooled_sd <- sqrt((p1*(1-p1) + p0*(1-p0)) / 2)
  if (pooled_sd == 0) return(0)
  (p1 - p0) / pooled_sd
}
smd_continuous <- function(m1, sd1, m0, sd0) {
  pooled_sd <- sqrt((sd1^2 + sd0^2)/2)
  if (pooled_sd == 0) return(0)
  (m1 - m0) / pooled_sd
}
## Generalised (multi-category) SMD, Yang & Dalton (2012); drops reference
## level, uses the average multinomial covariance of the two groups, with a
## pseudo-inverse fallback for near-singular categories.
smd_categorical <- function(levels1_prop, levels0_prop) {
  k <- length(levels1_prop)
  if (k < 2) return(NA_real_)
  p1 <- levels1_prop[-k]; p0 <- levels0_prop[-k]
  S1 <- diag(p1) - outer(p1,p1); S0 <- diag(p0) - outer(p0,p0)
  S <- (S1+S0)/2
  Sinv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
  d <- as.numeric(sqrt(t(p1-p0) %*% Sinv %*% (p1-p0)))
  d
}

grp1 <- eligible |> dplyr::filter(R_hosp==1)   # recorded
grp0 <- eligible |> dplyr::filter(R_hosp==0)   # unrecorded (unknown+blank combined)

rows <- list()

## -- age (continuous) --
m1<-mean(grp1$age_years); sd1<-sd(grp1$age_years); m0<-mean(grp0$age_years); sd0<-sd(grp0$age_years)
rows$age <- tibble::tibble(variable="age_years", level=NA_character_, variable_type="continuous",
  mean_recorded=round(m1,2), sd_recorded=round(sd1,2), mean_unrecorded=round(m0,2), sd_unrecorded=round(sd0,2),
  n_recorded=NA_integer_, pct_recorded=NA_real_, n_unrecorded=NA_integer_, pct_unrecorded=NA_real_,
  smd=round(smd_continuous(m1,sd1,m0,sd0),4))

## -- sex (binary: male) --
p1<-mean(grp1$sex=="male"); p0<-mean(grp0$sex=="male")
rows$sex <- tibble::tibble(variable="sex_male", level="male", variable_type="binary",
  mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
  n_recorded=sum(grp1$sex=="male"), pct_recorded=round(100*p1,2),
  n_unrecorded=sum(grp0$sex=="male"), pct_unrecorded=round(100*p0,2),
  smd=round(smd_binary(p1,p0),4))

## -- DM / HTN / CKD (binary present) --
for (cond in c("dm","htn","ckd")) {
  p1<-mean(grp1[[cond]]); p0<-mean(grp0[[cond]])
  rows[[cond]] <- tibble::tibble(variable=cond, level="present", variable_type="binary",
    mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
    n_recorded=sum(grp1[[cond]]), pct_recorded=round(100*p1,2),
    n_unrecorded=sum(grp0[[cond]]), pct_unrecorded=round(100*p0,2),
    smd=round(smd_binary(p1,p0),4))
}

## -- lab confirmation (binary: laboratory-confirmed) --
p1<-mean(grp1$criterion=="laboratory", na.rm=TRUE); p0<-mean(grp0$criterion=="laboratory", na.rm=TRUE)
rows$lab <- tibble::tibble(variable="lab_confirmed", level="laboratory", variable_type="binary",
  mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
  n_recorded=sum(grp1$criterion=="laboratory", na.rm=TRUE), pct_recorded=round(100*p1,2),
  n_unrecorded=sum(grp0$criterion=="laboratory", na.rm=TRUE), pct_unrecorded=round(100*p0,2),
  smd=round(smd_binary(p1,p0),4))

## -- multi-category: year, region, core_profile --
add_categorical <- function(var, name) {
  t1 <- table(factor(grp1[[var]])); t0 <- table(factor(grp0[[var]]))
  lv <- union(names(t1), names(t0))
  ## NB: `t1[lv]` on a 1-d "table" array retains its dim attribute (a known R
  ## array-indexing quirk), and `ifelse(test, ...)` then copies that dim from
  ## `test` into the result - silently producing a 1-d-array-flavoured integer
  ## column that readr's write_csv later rejects as a "matrix column". as.vector()
  ## strips it explicitly rather than relying on as.integer() alone to do so.
  n1 <- as.vector(as.integer(t1[lv])); n1[is.na(n1)] <- 0L
  n0 <- as.vector(as.integer(t0[lv])); n0[is.na(n0)] <- 0L
  p1v <- n1/sum(n1); p0v <- n0/sum(n0)
  overall_smd <- smd_categorical(as.numeric(p1v), as.numeric(p0v))
  per_level <- tibble::tibble(
    variable = name, level = lv, variable_type = "categorical",
    mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
    n_recorded = n1, pct_recorded = round(100*p1v,2),
    n_unrecorded = n0, pct_unrecorded = round(100*p0v,2),
    smd = NA_real_
  )
  summary_row <- tibble::tibble(variable=name, level="__OVERALL_SMD__", variable_type="categorical_summary",
    mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
    n_recorded=NA_integer_, pct_recorded=NA_real_, n_unrecorded=NA_integer_, pct_unrecorded=NA_real_,
    smd=round(overall_smd,4))
  dplyr::bind_rows(per_level, summary_row)
}
rows$year    <- add_categorical("year", "calendar_year")
rows$region  <- add_categorical("region", "region")
rows$profile <- add_categorical("core_profile", "core_profile")

## -- secondary diagnostic: corrected chikungunya-death rate, among those
## with KNOWN death status, contrasted by hosp-recording group (NOT
## restricting the hosp population to the mortality cohort) --
death_known <- eligible |> dplyr::filter(!is.na(died_from_chik))
d1 <- death_known |> dplyr::filter(R_hosp==1); d0 <- death_known |> dplyr::filter(R_hosp==0)
p1<-mean(d1$died_from_chik); p0<-mean(d0$died_from_chik)
rows$death <- tibble::tibble(variable="chik_death_rate_among_death_known", level="died_from_chik=TRUE",
  variable_type="binary_secondary_diagnostic",
  mean_recorded=NA_real_, sd_recorded=NA_real_, mean_unrecorded=NA_real_, sd_unrecorded=NA_real_,
  n_recorded=sum(d1$died_from_chik), pct_recorded=round(100*p1,3),
  n_unrecorded=sum(d0$died_from_chik), pct_unrecorded=round(100*p0,3),
  smd=round(smd_binary(p1,p0),4))

recorded_vs_unrecorded_characteristics <- dplyr::bind_rows(rows)
readr::write_csv(recorded_vs_unrecorded_characteristics, file.path(val_dir, "recorded_vs_unrecorded_characteristics.csv"))

## =============================================================================
## PHASE 5 -- DEFERRED THIS PASS (per explicit updated scope: descriptive
## audit takes priority; the preliminary recording-probability model /
## diagnostic IPW weights are optional and must not delay this gate).
## Placeholder files written so downstream tooling expecting these paths to
## exist does not break; VALIDATION_3_2A.md documents this decision and
## what Gate 3.2B would need to compute them.
## =============================================================================

readr::write_csv(
  tibble::tibble(status = "DEFERRED",
    reason = "Preliminary recording-probability model and diagnostic IPW weights were explicitly scoped out of Gate 3.2A to prioritise the descriptive missingness audit; see VALIDATION_3_2A.md.",
    next_step = "Compute in Gate 3.2B if the descriptive patterns in this report warrant it."),
  file.path(val_dir, "recording_probability_diagnostics.csv")
)
readr::write_csv(
  tibble::tibble(status = "DEFERRED",
    reason = "No recording-probability model was fit in Gate 3.2A, so no IPW weights exist yet to summarise.",
    next_step = "Compute in Gate 3.2B if warranted."),
  file.path(val_dir, "diagnostic_ipw_weight_distribution.csv")
)

## =============================================================================
## FIGURES
## =============================================================================

## -- figure_missingness_by_profile.png --
fig_profile_df <- by_profile |>
  dplyr::mutate(level = factor(level, levels = core_profile_levels))
p_profile <- ggplot2::ggplot(fig_profile_df, ggplot2::aes(x = level)) +
  ggplot2::geom_col(ggplot2::aes(y = pct_known), fill = "#2c7fb8") +
  ggplot2::geom_text(ggplot2::aes(y = pct_known, label = paste0(pct_known,"%\n(n=",format(n_eligible,big.mark=","),")")),
                      vjust = -0.3, size = 3) +
  ggplot2::scale_y_continuous(limits = c(0, max(fig_profile_df$pct_known)*1.25), expand = c(0,0)) +
  ggplot2::labs(x = NULL, y = "Hospitalisation status recorded (%)",
                title = "Hospitalisation recording completeness by multimorbidity profile",
                subtitle = "Brazil, confirmed chikungunya cases, 2017-2025 (core-three eligible cohort)") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
ggplot2::ggsave(file.path(val_dir, "figure_missingness_by_profile.png"), p_profile, width = 9, height = 6, dpi = 300)

## -- figure_missingness_by_year.png --
fig_year_df <- by_year |> dplyr::mutate(level = as.integer(level))
p_year <- ggplot2::ggplot(fig_year_df, ggplot2::aes(x = level, y = pct_known)) +
  ggplot2::geom_line(colour = "#2c7fb8", linewidth = 1) +
  ggplot2::geom_point(size = 2.5, colour = "#2c7fb8") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(pct_known,"%")), vjust = -1, size = 3.2) +
  ggplot2::scale_x_continuous(breaks = fig_year_df$level) +
  ggplot2::scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  ggplot2::labs(x = "Calendar year", y = "Hospitalisation status recorded (%)",
                title = "Hospitalisation recording completeness over time",
                subtitle = "Brazil, confirmed chikungunya cases (core-three eligible cohort)") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(val_dir, "figure_missingness_by_year.png"), p_year, width = 9, height = 6, dpi = 300)

## -- figure_state_year_missingness_heatmap.png --
heat_df <- year_x_state |>
  dplyr::mutate(
    year = as.integer(stringr::str_extract(level, "^[0-9]+")),
    state_f = stringr::str_extract(level, "(?<=\\| ).*$")
  )
p_heat <- ggplot2::ggplot(heat_df, ggplot2::aes(x = factor(year), y = state_f, fill = pct_missing)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(name = "% missing", option = "C", direction = 1) +
  ggplot2::labs(x = "Calendar year", y = "State of residence",
                title = "Hospitalisation-status missingness by state and year",
                subtitle = "% of eligible core-three cases with hospitalisation outcome NOT recorded") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(file.path(val_dir, "figure_state_year_missingness_heatmap.png"), p_heat, width = 9, height = 8, dpi = 300)

message("[39] Gate 3.2A descriptive outputs written to ", val_dir)
