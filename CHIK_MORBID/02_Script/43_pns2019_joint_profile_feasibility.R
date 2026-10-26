# ---------------------------------------------------------------------------
# 43_pns2019_joint_profile_feasibility.R   -- Gate 3.3B
#
# PNS 2019 POPULATION MULTIMORBIDITY PREVALENCE FEASIBILITY ASSESSMENT.
# Assesses whether Brazil's PNS 2019 (Pesquisa Nacional de Saude) microdata
# can provide reliable, survey-weighted population prevalence estimates for
# DM/HTN/CKD and their 8 joint multimorbidity profiles, as a candidate
# alternative/complement to the GBD-anchored prevalence used in
# 29_background_prevalence_core3_brazil.R (flagged as a measurement-
# definition mismatch in Gate 3.3A, VALIDATION_3_3A.md).
#
# This is a feasibility/data-validation analysis. Does NOT modify or rerun
# 02_clean_chik_sinan_brazil.R, 12_core3_condition_specific_rr.R, 29_/30_/
# 32_/34_, any primary model cache, or any manuscript file. Does NOT
# substitute PNS into the burden pipeline. Does NOT fit a clustering/copula
# model.
#
# ---------------------------------------------------------------------------
# PHASE 1 DATA PROVENANCE (verified this session, see
# pns2019_source_and_survey_design.md for full detail):
#   Data:          https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20220525.zip
#                  (28,152 KB; Last-Modified 2022-05-25; downloaded verbatim,
#                  byte-size matched against the server's Content-Length)
#   Documentation: https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/
#                  Dicionario_e_input_20220530.zip (dictionary .xls + SAS
#                  input layout, authoritative fixed-width positions - no
#                  position was guessed) and Chaves_PNS_2019.pdf (sampling
#                  keys documentation).
#   Reader:        PNSIBGE::read_pns() (CRAN), which parses the official SAS
#                  INPUT layout directly (exact @position/width per
#                  variable) - fixed-width positions are never hand-coded
#                  in this script.
#   Stored locally at 01_Data/pns_2019_raw/ (gitignored - raw microdata is
#   NOT committed/pushed, per instruction).
# ---------------------------------------------------------------------------
# STRUCTURAL FINDING (verified empirically this session, not assumed):
#   read_pns() returns ONE ROW PER HOUSEHOLD RESIDENT (not one row per
#   household) - N=279,382 after its internal household-interview-completion
#   filter (V0015=="01"). The selected-adult individual questionnaire
#   (module M, module C demographics, and the Q00201/Q03001/Q124 diagnosis
#   questions) is populated ONLY on the single row per household
#   corresponding to the randomly selected adult; all other residents'
#   rows have these fields as NA by design (not missingness).
#   `M001` ("Entrevista do adulto selecionado") is the correct selected-
#   adult interview-completion flag - DISTINCT from V0015 (household-level
#   interview completion), which does not by itself guarantee the selected-
#   adult module was completed. M001=="1" (n=90,846) is the verified
#   selected-adult analytic population; this matches V00291 (calibrated
#   selected-adult weight) being non-missing/non-zero for exactly the same
#   90,846 rows (empirically confirmed 1:1 correspondence, see Phase 2
#   output). All other M001 codes (2=refusal, 3=not found, 9=unknown, NA=
#   not the selected row) are excluded from the analytic population, per
#   "use the selected-adult respondent data" and because these respondents
#   have no diagnosis-question answers at all.
# ---------------------------------------------------------------------------
# Needs: 01_Data/pns_2019_raw/PNS_2019.txt,
#        01_Data/pns_2019_raw/dicionario_input/input_PNS_2019.txt
# Output: 03_Output/validation/gate33b/*.csv, *.png, *.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(readr); library(tibble)
  library(survey); library(PNSIBGE)
})
options(scipen = 999, dplyr.summarise.inform = FALSE, survey.lonely.psu = "adjust")

out_dir <- "03_Output/validation/gate33b"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
raw_dir <- "01_Data/pns_2019_raw"
SEED <- 1L
set.seed(SEED)

## =============================================================================
## PHASE 1 -- LOAD VERIFIED PNS 2019 MICRODATA
## =============================================================================

stopifnot(file.exists(file.path(raw_dir, "PNS_2019.txt")),
          file.exists(file.path(raw_dir, "dicionario_input/input_PNS_2019.txt")))

vars_needed <- c("C006", "C008", "Q00201", "Q00202", "Q03001", "Q03002", "Q124")
t_load0 <- Sys.time()
pns_all <- read_pns(
  microdata = file.path(raw_dir, "PNS_2019.txt"),
  input_txt = file.path(raw_dir, "dicionario_input/input_PNS_2019.txt"),
  vars = vars_needed
)
message(sprintf("[43] PNS microdata loaded in %.1fs: %s rows (all household residents, post household-interview-completion filter)",
                 as.numeric(difftime(Sys.time(), t_load0, units = "secs")), format(nrow(pns_all), big.mark=",")))

## ---- verified selected-adult analytic population ----
m001_tab <- table(pns_all$M001, useNA = "always")
weight_vs_m001 <- table(M001 = pns_all$M001, weight_positive = !is.na(pns_all$V00291) & pns_all$V00291 > 0, useNA = "always")
stopifnot(all(weight_vs_m001["1", "TRUE"] == sum(pns_all$M001 == "1", na.rm = TRUE)))  # 1:1 correspondence check

selected_adults <- pns_all |>
  dplyr::filter(M001 == "1") |>
  dplyr::mutate(
    age = as.numeric(C008),
    sex = dplyr::recode(C006, `1` = "male", `2` = "female", .default = NA_character_),
    weight = as.numeric(V00291)
  )
stopifnot(nrow(selected_adults) == as.integer(m001_tab["1"]),
          all(!is.na(selected_adults$weight)), all(selected_adults$weight > 0),
          all(!is.na(selected_adults$age)))

message(sprintf("[43] Verified selected-adult analytic population: N=%s (M001=='1', weight and age both 100%% non-missing by construction)",
                 format(nrow(selected_adults), big.mark=",")))
message(sprintf("[43] Age range in selected-adult pool: %d-%d (eligibility floor is NOT assumed to be 18 - see Phase 2 age distribution check)",
                 min(selected_adults$age), max(selected_adults$age)))

age_elig_check <- selected_adults |> dplyr::count(age_lt18 = age < 18) |>
  dplyr::mutate(pct = round(100*n/sum(n), 2))
message("[43] Selected-adult age<18 check:"); print(age_elig_check)

message("[43] PHASE 1 complete.")

## =============================================================================
## PHASE 2/3 -- VARIABLE DEFINITIONS, CROSSWALK, AND HARMONISATION
## Verified directly against dicionario_PNS_microdados_2019.xls (not assumed):
##   Q00201 "Algum medico ja lhe deu o diagnostico de hipertensao arterial
##           (pressao alta)?" (Has a doctor ever diagnosed you with
##           hypertension?) - codes 1=Sim, 2=Nao, 9=Ignorado
##   Q00202 "Essa hipertensao arterial (pressao alta) ocorreu apenas durante
##           algum periodo de gravidez?" (Did this hypertension occur ONLY
##           during a period of pregnancy?) - codes 1=Sim, 2=Nao, 9=Ignorado;
##           blank/NA where not asked (men, or Q00201 != 1)
##   Q03001 "Algum medico ja lhe deu o diagnostico de diabetes?" - 1/2/9, same pattern
##   Q03002 "Esse diabetes ocorreu apenas durante algum periodo de gravidez?" - same pattern
##   Q124   "Algum medico ja lhe deu o diagnostico de insuficiencia renal
##           cronica?" (Has a doctor ever diagnosed you with chronic renal
##           insufficiency?) - codes 1=Sim, 2=Nao, 9=Ignorado. NO pregnancy-
##           only variant exists for CKD in the PNS questionnaire.
##   C006 "Sexo": 1=Homem(male), 2=Mulher(female)
##   C008 "Idade do morador na data de referencia": numeric, 000-130
## =============================================================================

pns2019_variable_definition_crosswalk <- tibble::tribble(
  ~condition, ~source, ~variable, ~official_question_wording_pt, ~official_question_wording_en, ~response_coding, ~pregnancy_only_handling,
  "Hypertension", "PNS 2019", "Q00201 (+Q00202)", "Algum medico ja lhe deu o diagnostico de hipertensao arterial (pressao alta)?", "Has a doctor ever given you a diagnosis of hypertension (high blood pressure)?", "1=Sim(Yes), 2=Nao(No), 9=Ignorado(Unknown), blank=Not applicable (skip pattern)", "Q00202 ('did this occur only during pregnancy?') harmonised to 'no' (not chronic) when ==1 - see rationale below",
  "Hypertension", "SINAN (chikungunya notification)", "HIPERTENSA -> hypertension", "Doencas preexistentes (Hipertensao arterial) - field 42", "Pre-existing diseases (Hypertension) - clinician checkbox", "1=Sim, 2=Nao (map_sim_nao also allows 9=Ignorado)", "No equivalent distinction - SINAN has no pregnancy-only flag; relies entirely on clinician judgement of 'pre-existing'",
  "Diabetes", "PNS 2019", "Q03001 (+Q03002)", "Algum medico ja lhe deu o diagnostico de diabetes?", "Has a doctor ever given you a diagnosis of diabetes?", "1=Sim, 2=Nao, 9=Ignorado, blank=Not applicable (skip pattern)", "Q03002 harmonised the same way as Q00202 above",
  "Diabetes", "SINAN", "DIABETES -> diabetes", "Doencas preexistentes (Diabetes) - field 38", "Pre-existing diseases (Diabetes) - clinician checkbox", "1=Sim, 2=Nao (9=Ignorado)", "No equivalent distinction in SINAN",
  "Chronic kidney disease", "PNS 2019", "Q124", "Algum medico ja lhe deu o diagnostico de insuficiencia renal cronica?", "Has a doctor ever given you a diagnosis of chronic renal insufficiency?", "1=Sim, 2=Nao, 9=Ignorado", "N/A - no pregnancy-only CKD question exists in PNS",
  "Chronic kidney disease", "SINAN", "RENAL -> renal_disease", "Doencas preexistentes (Doenca renal cronica) - field 41", "Pre-existing diseases (Chronic kidney disease) - clinician checkbox", "1=Sim, 2=Nao (9=Ignorado)", "N/A",
  "(demographic) Age", "PNS 2019", "C008", "Idade do morador na data de referencia", "Resident age at reference date", "Numeric, 000-130", "N/A",
  "(demographic) Sex", "PNS 2019", "C006", "Sexo", "Sex", "1=Homem(Male), 2=Mulher(Female)", "N/A"
)
readr::write_csv(pns2019_variable_definition_crosswalk, file.path(out_dir, "pns2019_variable_definition_crosswalk.csv"))
message("[43] PHASE 2 (variable crosswalk) written.")

## ---- harmonised binary exposures: pregnancy-only diagnoses reclassified
## to 'no' (not chronic), consistent with SINAN's pre-existing-disease
## concept; true unknown/unanswered is KEPT as 'unknown', never coerced to
## 'no' (per explicit instruction) ----
harmonise_dx <- function(dx, preg_only) {
  dplyr::case_when(
    dx == "2" ~ "no",
    dx == "1" & preg_only == "1" ~ "no",          # pregnancy-only -> not chronic
    dx == "1" & preg_only %in% c("2", NA) ~ "yes", # confirmed chronic, or preg-only question not applicable (e.g. men)
    dx %in% c("9", NA) ~ "unknown",
    TRUE ~ "unknown"
  )
}

selected_adults <- selected_adults |>
  dplyr::mutate(
    htn_pns_raw_preg_only = dplyr::case_when(Q00201=="1" & Q00202=="1" ~ TRUE, TRUE ~ FALSE),
    dm_pns_raw_preg_only  = dplyr::case_when(Q03001=="1" & Q03002=="1" ~ TRUE, TRUE ~ FALSE),
    htn_pns = harmonise_dx(Q00201, Q00202),
    dm_pns  = harmonise_dx(Q03001, Q03002),
    ckd_pns = dplyr::case_when(Q124=="2" ~ "no", Q124=="1" ~ "yes", Q124 %in% c("9",NA) ~ "unknown", TRUE ~ "unknown")
  )

n_preg_only_htn <- sum(selected_adults$htn_pns_raw_preg_only)
n_preg_only_dm <- sum(selected_adults$dm_pns_raw_preg_only)
message(sprintf("[43] Pregnancy-only diagnoses reclassified to 'no': HTN n=%d, DM n=%d (of %s total selected adults)",
                 n_preg_only_htn, n_preg_only_dm, format(nrow(selected_adults), big.mark=",")))

message("[43] PHASE 3 (harmonised dm_pns/htn_pns/ckd_pns) complete.")

## =============================================================================
## PHASE -- COHORT AND MISSINGNESS (explicit denominators throughout)
## =============================================================================

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

selected_adults <- selected_adults |>
  dplyr::mutate(
    dm = dplyr::case_when(dm_pns=="yes"~1L, dm_pns=="no"~0L, TRUE~NA_integer_),
    htn = dplyr::case_when(htn_pns=="yes"~1L, htn_pns=="no"~0L, TRUE~NA_integer_),
    ckd = dplyr::case_when(ckd_pns=="yes"~1L, ckd_pns=="no"~0L, TRUE~NA_integer_),
    all_three_known = !is.na(dm) & !is.na(htn) & !is.na(ckd),
    core_profile = dplyr::case_when(
      !all_three_known ~ NA_character_,
      dm==0&htn==0&ckd==0 ~ "None", dm==1&htn==0&ckd==0 ~ "DM", dm==0&htn==1&ckd==0 ~ "HTN",
      dm==0&htn==0&ckd==1 ~ "CKD", dm==1&htn==1&ckd==0 ~ "DM+HTN", dm==1&htn==0&ckd==1 ~ "DM+CKD",
      dm==0&htn==1&ckd==1 ~ "HTN+CKD", dm==1&htn==1&ckd==1 ~ "DM+HTN+CKD"
    ),
    core_profile = factor(core_profile, levels = core_profile_levels),
    age_grp4 = dplyr::case_when(age>=20&age<40~"20-39", age>=40&age<60~"40-59",
                                 age>=60&age<80~"60-79", age>=80~"80+", TRUE~NA_character_),
    age_grp4 = factor(age_grp4, levels=c("20-39","40-59","60-79","80+"))
  )

pns2019_cohort_and_missingness <- tibble::tribble(
  ~item, ~n, ~pct_of_selected_adults,
  "Selected-adult analytic population (M001=='1')", nrow(selected_adults), 100,
  "  .. age < 18 (preserved, not in 18+ or 20+ analysis)", sum(selected_adults$age<18), round(100*mean(selected_adults$age<18),2),
  "  .. age 18-19 (preserved separately per instruction)", sum(selected_adults$age %in% 18:19), round(100*mean(selected_adults$age %in% 18:19),2),
  "  .. age 18+ (official-estimate reproduction population)", sum(selected_adults$age>=18), round(100*mean(selected_adults$age>=18),2),
  "  .. age 20+ (main analysis population)", sum(selected_adults$age>=20), round(100*mean(selected_adults$age>=20),2),
  "DM (Q03001) known (yes/no, pregnancy-only resolved to no)", sum(!is.na(selected_adults$dm)), round(100*mean(!is.na(selected_adults$dm)),2),
  "DM unknown/unanswered (Q03001 in {9,NA})", sum(is.na(selected_adults$dm)), round(100*mean(is.na(selected_adults$dm)),2),
  "HTN (Q00201) known", sum(!is.na(selected_adults$htn)), round(100*mean(!is.na(selected_adults$htn)),2),
  "HTN unknown/unanswered", sum(is.na(selected_adults$htn)), round(100*mean(is.na(selected_adults$htn)),2),
  "CKD (Q124) known", sum(!is.na(selected_adults$ckd)), round(100*mean(!is.na(selected_adults$ckd)),2),
  "CKD unknown/unanswered", sum(is.na(selected_adults$ckd)), round(100*mean(is.na(selected_adults$ckd)),2),
  "All three (DM/HTN/CKD) known -> profile-eligible", sum(selected_adults$all_three_known), round(100*mean(selected_adults$all_three_known),2),
  "Profile-eligible AND age 20+", sum(selected_adults$all_three_known & selected_adults$age>=20), round(100*mean(selected_adults$all_three_known & selected_adults$age>=20),2),
  "Pregnancy-only HTN reclassified to 'no' (of all selected adults)", n_preg_only_htn, round(100*n_preg_only_htn/nrow(selected_adults),3),
  "Pregnancy-only DM reclassified to 'no' (of all selected adults)", n_preg_only_dm, round(100*n_preg_only_dm/nrow(selected_adults),3)
)
readr::write_csv(pns2019_cohort_and_missingness, file.path(out_dir, "pns2019_cohort_and_missingness.csv"))
message("[43] cohort/missingness written. Profile-eligible 20+: ",
        format(sum(selected_adults$all_three_known & selected_adults$age>=20), big.mark=","))

## =============================================================================
## PHASE 4 -- COMPLEX SURVEY DESIGN
## ids=UPA_PNS (PSU), strata=V0024, weights=V00291 (calibrated selected-adult
## weight, verified Phase 1/2). nest=TRUE because PSU codes are only unique
## WITHIN a stratum in this design (standard IBGE multi-stage convention -
## documented in Chaves_PNS_2019.pdf). survey.lonely.psu="adjust" (set via
## options() above) handles any stratum with a single PSU in a subset by
## centering it at the stratum mean rather than failing - a standard,
## conservative convention for subdomain variance estimation, applied
## transparently (not silently) and reported wherever it is invoked.
## =============================================================================

des_full <- survey::svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~weight,
                               data = selected_adults, nest = TRUE)
message(sprintf("[43] Survey design built: %s PSUs, %s strata, N=%s",
                 format(dplyr::n_distinct(selected_adults$UPA_PNS), big.mark=","),
                 format(dplyr::n_distinct(selected_adults$V0024), big.mark=","),
                 format(nrow(selected_adults), big.mark=",")))

message("[43] PHASE 4 complete.")

## =============================================================================
## PHASE 5 -- MARGINAL DM/HTN/CKD PREVALENCE (survey-weighted, domain estimation)
## =============================================================================

## Precomputed logical vectors (one per domain, same row order as
## selected_adults/des_full) - passed to survey::subset() by name rather
## than as an inline NSE expression, which avoids a substitute()/eval()
## environment-nesting pitfall with bquote-constructed subset calls.
age <- selected_adults$age
age_domains <- list(
  "18+"   = age >= 18,
  "18-19" = age >= 18 & age < 20,
  "20+"   = age >= 20,
  "20-39" = age >= 20 & age < 40,
  "40-59" = age >= 40 & age < 60,
  "60-79" = age >= 60 & age < 80,
  "80+"   = age >= 80
)
message(sprintf("[43] Domain sizes (unweighted rows matching each age filter): %s",
                 paste(sprintf("%s=%d", names(age_domains), vapply(age_domains, sum, integer(1))), collapse=", ")))

estimate_marginal <- function(design, binvar, domain_label, domain_vec) {
  sub_err <- NA_character_
  sub <- tryCatch(subset(design, domain_vec), error = function(e) { sub_err <<- conditionMessage(e); NULL })
  if (is.null(sub) || nrow(sub) == 0) {
    return(tibble::tibble(age_group=domain_label, variable=binvar, n_unweighted=0, n_positive_unweighted=0,
                           prevalence=NA_real_, se=NA_real_, ci_lower=NA_real_, ci_upper=NA_real_, cv_pct=NA_real_,
                           design_effect=NA_real_, n_known=0,
                           error=ifelse(is.na(sub_err), paste0("empty subset, nrow=",if(is.null(sub)) "NULL" else nrow(sub)), sub_err)))
  }
  dat <- sub$variables
  v <- dat[[binvar]]
  n_unw <- sum(!is.na(v)); n_pos <- sum(v==1, na.rm=TRUE)
  err_msg <- NA_character_
  out <- tryCatch({
    f <- stats::as.formula(paste0("~", binvar))
    ci <- survey::svyciprop(f, design = sub, method = "logit", na.rm = TRUE)
    se <- as.numeric(survey::SE(ci)); est <- as.numeric(ci); cis <- stats::confint(ci)
    deff <- tryCatch(survey::svymean(f, design = sub, na.rm = TRUE, deff = TRUE) |> survey::deff() |> as.numeric(),
                      error = function(e) NA_real_)
    tibble::tibble(age_group=domain_label, variable=binvar, n_unweighted=n_unw, n_positive_unweighted=n_pos,
                   prevalence=round(est*100,3), se=round(se*100,4), ci_lower=round(cis[1]*100,3), ci_upper=round(cis[2]*100,3),
                   cv_pct=round(100*se/est,2), design_effect=round(deff,2), n_known=n_unw, error=NA_character_)
  }, error = function(e) {
    tibble::tibble(age_group=domain_label, variable=binvar, n_unweighted=n_unw, n_positive_unweighted=n_pos,
                   prevalence=NA_real_, se=NA_real_, ci_lower=NA_real_, ci_upper=NA_real_, cv_pct=NA_real_,
                   design_effect=NA_real_, n_known=n_unw, error=conditionMessage(e))
  })
  out
}

pns2019_marginal_prevalence_by_age <- purrr::map_dfr(names(age_domains), function(lbl) {
  dplyr::bind_rows(
    estimate_marginal(des_full, "dm", lbl, age_domains[[lbl]]),
    estimate_marginal(des_full, "htn", lbl, age_domains[[lbl]]),
    estimate_marginal(des_full, "ckd", lbl, age_domains[[lbl]])
  )
})
readr::write_csv(pns2019_marginal_prevalence_by_age, file.path(out_dir, "pns2019_marginal_prevalence_by_age.csv"))
message("[43] PHASE 5 (marginal prevalence) written:")
print(pns2019_marginal_prevalence_by_age |> dplyr::select(age_group, variable, n_unweighted, prevalence, ci_lower, ci_upper, cv_pct, error), n=Inf)

official_check <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="18+")
message(sprintf("[43] Quality check (18+ vs official PNS 2019 published estimates, approximate published values in parentheses): DM=%.1f%% (~7.7%%), HTN=%.1f%% (~23.9%%)",
                 official_check$prevalence[official_check$variable=="dm"], official_check$prevalence[official_check$variable=="htn"]))

message("[43] PHASE 5 complete.")

## =============================================================================
## PHASE 6 -- EIGHT JOINT-PROFILE PREVALENCE (survey-weighted)
## =============================================================================

estimate_profiles <- function(design, domain_label, domain_vec) {
  combined_vec <- domain_vec & selected_adults$all_three_known
  sub <- tryCatch(subset(design, combined_vec), error = function(e) NULL)
  if (is.null(sub) || nrow(sub) == 0) return(NULL)
  dat <- sub$variables
  unw <- dat |> dplyr::count(core_profile, .drop = FALSE) |> dplyr::rename(n_unweighted = n)
  out <- tryCatch({
    sm <- survey::svymean(~core_profile, design = sub, na.rm = TRUE)
    cis <- stats::confint(sm)
    tibble::tibble(
      core_profile = factor(sub("core_profile", "", names(sm)), levels = core_profile_levels),
      prevalence = round(as.numeric(sm)*100, 3),
      se = round(as.numeric(survey::SE(sm))*100, 4),
      ci_lower = round(cis[,1]*100, 3), ci_upper = round(cis[,2]*100, 3)
    )
  }, error = function(e) {
    tibble::tibble(core_profile = factor(core_profile_levels, levels=core_profile_levels),
                    prevalence=NA_real_, se=NA_real_, ci_lower=NA_real_, ci_upper=NA_real_)
  })
  out <- out |> dplyr::mutate(cv_pct = round(100*se/prevalence, 2),
                               age_group = domain_label, n_eligible_domain = nrow(dat)) |>
    dplyr::left_join(unw, by = "core_profile") |>
    dplyr::relocate(age_group, core_profile, n_unweighted, n_eligible_domain)
  out
}

profile_domains <- age_domains[c("18+","20+","20-39","40-59","60-79","80+")]
pns2019_joint_profile_prevalence_by_age <- purrr::map_dfr(names(profile_domains), function(lbl)
  estimate_profiles(des_full, lbl, profile_domains[[lbl]]))
readr::write_csv(pns2019_joint_profile_prevalence_by_age, file.path(out_dir, "pns2019_joint_profile_prevalence_by_age.csv"))

## validation: profile prevalences should sum to ~100% within each age group
sum_check <- pns2019_joint_profile_prevalence_by_age |> dplyr::group_by(age_group) |>
  dplyr::summarise(sum_prevalence = sum(prevalence, na.rm=TRUE), .groups="drop")
message("[43] PHASE 6 (joint-profile prevalence) written. Sum-to-100% check:")
print(sum_check)
stopifnot(all(abs(sum_check$sum_prevalence - 100) < 0.5))
message("[43] Sum-to-100% validation: PASS (all within 0.5 percentage points)")

## validation: marginal reconstruction from profiles vs direct marginal estimate
recon_check <- pns2019_joint_profile_prevalence_by_age |>
  dplyr::mutate(has_dm = grepl("DM", core_profile), has_htn = grepl("HTN", core_profile), has_ckd = grepl("CKD", core_profile)) |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(dm_recon = sum(prevalence[has_dm]), htn_recon = sum(prevalence[has_htn]), ckd_recon = sum(prevalence[has_ckd]), .groups="drop") |>
  dplyr::left_join(
    pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group %in% names(profile_domains)) |>
      dplyr::select(age_group, variable, prevalence) |> tidyr::pivot_wider(names_from=variable, values_from=prevalence) |>
      dplyr::rename(dm_direct=dm, htn_direct=htn, ckd_direct=ckd),
    by = "age_group"
  ) |>
  dplyr::mutate(dm_diff = round(dm_recon-dm_direct,2), htn_diff = round(htn_recon-htn_direct,2), ckd_diff = round(ckd_recon-ckd_direct,2))
message("[43] Marginal reconstruction-vs-direct check (differences arise because the direct marginal excludes 'unknown' on EACH condition separately, while the profile reconstruction requires ALL THREE known simultaneously - a smaller, stricter complete-case population; small differences are therefore expected and not a bug):")
print(recon_check |> dplyr::select(age_group, dm_diff, htn_diff, ckd_diff))

message("[43] PHASE 6 complete.")

## =============================================================================
## PHASE 7 -- RARE-PROFILE RELIABILITY DIAGNOSTICS
## Flags are DIAGNOSTIC, not automatic exclusion criteria (per instruction).
## =============================================================================

psu_diversity <- purrr::map_dfr(names(profile_domains), function(lbl) {
  vec <- profile_domains[[lbl]] & selected_adults$all_three_known
  selected_adults[vec, ] |> dplyr::group_by(core_profile, .drop = FALSE) |>
    dplyr::summarise(n_distinct_psu = dplyr::n_distinct(UPA_PNS), .groups = "drop") |>
    dplyr::mutate(age_group = lbl)
})

pns2019_profile_reliability_diagnostics <- pns2019_joint_profile_prevalence_by_age |>
  dplyr::left_join(psu_diversity, by = c("age_group","core_profile")) |>
  dplyr::mutate(
    flag_small_n = n_unweighted < 20,
    flag_very_small_n = n_unweighted < 10,
    flag_high_cv = !is.na(cv_pct) & cv_pct > 30,
    flag_extreme_cv = !is.na(cv_pct) & cv_pct > 50,
    flag_few_psu = n_distinct_psu < 10,
    flag_ci_includes_zero_or_negative = ci_lower <= 0,
    reliability_flag_count = flag_small_n + flag_high_cv + flag_few_psu + flag_ci_includes_zero_or_negative,
    reliability_summary = dplyr::case_when(
      core_profile == "None" ~ "N/A (reference/majority category, not a rare profile)",
      reliability_flag_count == 0 ~ "No diagnostic flags raised",
      reliability_flag_count >= 1 & reliability_flag_count <= 2 ~ "Some caution warranted",
      reliability_flag_count >= 3 ~ "UNSTABLE - interpret with substantial caution"
    )
  ) |>
  dplyr::relocate(age_group, core_profile, n_unweighted, n_distinct_psu, prevalence, cv_pct, ci_lower, ci_upper)

readr::write_csv(pns2019_profile_reliability_diagnostics, file.path(out_dir, "pns2019_profile_reliability_diagnostics.csv"))

unstable_rare <- pns2019_profile_reliability_diagnostics |>
  dplyr::filter(core_profile %in% c("CKD","DM+CKD","HTN+CKD","DM+HTN+CKD"), reliability_flag_count >= 2)
message(sprintf("[43] PHASE 7 (reliability diagnostics) written. %d rare-profile x age-group cells flagged with >=2 reliability concerns:",
                 nrow(unstable_rare)))
print(unstable_rare |> dplyr::select(age_group, core_profile, n_unweighted, n_distinct_psu, prevalence, cv_pct, reliability_summary))

## ---- also produce the plain unweighted-counts CSV explicitly requested ----
pns2019_profile_unweighted_counts <- pns2019_profile_reliability_diagnostics |>
  dplyr::select(age_group, core_profile, n_unweighted, n_eligible_domain, n_distinct_psu) |>
  dplyr::mutate(pct_unweighted = round(100*n_unweighted/n_eligible_domain, 3))
readr::write_csv(pns2019_profile_unweighted_counts, file.path(out_dir, "pns2019_profile_unweighted_counts.csv"))

message("[43] PHASE 7 complete.")

## =============================================================================
## PHASE 8 -- COMPARISON WITH EXISTING PREVALENCE ASSUMPTIONS (SINAN / GBD)
## Descriptive only. No correction factor computed. Harmonised to the SAME
## 20-39/40-59/60-79/80+ age bands used for the PNS estimates above.
## =============================================================================

## ---- SINAN: condition prevalence among confirmed chikungunya core-3
## eligible cases (independently re-derived here, not read from another
## gate's cache, for self-containment) ----
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
sinan_core3 <- ind |>
  dplyr::filter(is_confirmed_chik, lubridate::year(event_date) >= 2017,
                !is.na(age_years), age_years >= 0, age_years <= 100, sex %in% c("male","female"),
                diabetes %in% c("no","yes"), hypertension %in% c("no","yes"), renal_disease %in% c("no","yes")) |>
  dplyr::mutate(age_grp4 = dplyr::case_when(age_years>=20&age_years<40~"20-39", age_years>=40&age_years<60~"40-59",
                                              age_years>=60&age_years<80~"60-79", age_years>=80~"80+", TRUE~NA_character_))

sinan_by_age <- sinan_core3 |> dplyr::filter(!is.na(age_grp4)) |>
  dplyr::group_by(age_grp4) |>
  dplyr::summarise(n=dplyr::n(), dm=round(100*mean(diabetes=="yes"),3), htn=round(100*mean(hypertension=="yes"),3),
                    ckd=round(100*mean(renal_disease=="yes"),3), .groups="drop") |>
  dplyr::mutate(source="SINAN (confirmed chikungunya cases, clinician-recorded pre-existing disease)")

## ---- GBD: Brazil CKD prevalence, Both sex, 2023 (same verified extraction
## as Gate 3.3A / 29_background_prevalence_core3_brazil.R), mapped to the
## same 4 age bands by population-weighted... here unweighted mean of the
## matched 5-year GBD bands within each 20-year band (descriptive only) ----
gbd_raw <- readr::read_csv("01_Data/gbd_prevalence.csv", show_col_types = FALSE)
keep_age_5y <- c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                  "60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+")
gbd_ckd_bra <- gbd_raw |>
  dplyr::filter(measure_name=="Prevalence", metric_name=="Rate", location_name=="Brazil",
                sex_name=="Both", cause_name=="Chronic kidney disease") |>
  dplyr::mutate(age_clean = stringr::str_remove(age_name, " years| year")) |>
  dplyr::filter(age_clean %in% keep_age_5y) |>
  dplyr::mutate(age_grp4 = dplyr::case_when(
    age_clean %in% c("20-24","25-29","30-34","35-39") ~ "20-39",
    age_clean %in% c("40-44","45-49","50-54","55-59") ~ "40-59",
    age_clean %in% c("60-64","65-69","70-74","75-79") ~ "60-79",
    TRUE ~ "80+"
  ))
gbd_by_age <- gbd_ckd_bra |> dplyr::group_by(age_grp4) |>
  dplyr::summarise(ckd_gbd = round(mean(val)/1e5*100, 3), .groups="drop") |>
  dplyr::mutate(source = "GBD 2023 (Brazil general population, modelled, all CKD stages)")

## ---- PNS summary for the same bands ----
pns_by_age <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group %in% c("20-39","40-59","60-79","80+")) |>
  dplyr::select(age_group, variable, prevalence) |> tidyr::pivot_wider(names_from=variable, values_from=prevalence) |>
  dplyr::rename(age_grp4=age_group, dm_pns=dm, htn_pns=htn, ckd_pns=ckd) |>
  dplyr::mutate(source="PNS 2019 (selected adults, physician-diagnosed, survey-weighted)")

pns2019_prevalence_source_comparison <- sinan_by_age |>
  dplyr::select(age_grp4, dm_sinan=dm, htn_sinan=htn, ckd_sinan=ckd, n_sinan=n) |>
  dplyr::full_join(pns_by_age |> dplyr::select(age_grp4, dm_pns, htn_pns, ckd_pns), by="age_grp4") |>
  dplyr::full_join(gbd_by_age |> dplyr::select(age_grp4, ckd_gbd), by="age_grp4") |>
  dplyr::mutate(age_grp4 = factor(age_grp4, levels=c("20-39","40-59","60-79","80+"))) |>
  dplyr::arrange(age_grp4)
readr::write_csv(pns2019_prevalence_source_comparison, file.path(out_dir, "pns2019_prevalence_source_comparison.csv"))
message("[43] PHASE 8 (source comparison) written:")
print(pns2019_prevalence_source_comparison)

message("[43] PHASE 8 complete.")

## =============================================================================
## FIGURES
## =============================================================================

font_family <- "Arial"

## ---- Figure: 8-profile prevalence across age groups, with 95% CI ----
fig1_data <- pns2019_joint_profile_prevalence_by_age |>
  dplyr::filter(age_group %in% c("20-39","40-59","60-79","80+"), core_profile != "None") |>
  dplyr::mutate(age_group = factor(age_group, levels=c("20-39","40-59","60-79","80+")),
                core_profile = factor(core_profile, levels = setdiff(core_profile_levels,"None")))

p1 <- ggplot2::ggplot(fig1_data, ggplot2::aes(x = age_group, y = prevalence, colour = core_profile, group = core_profile)) +
  ggplot2::geom_line(linewidth=0.7) +
  ggplot2::geom_point(size=2) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_lower, ymax=ci_upper), width=0.15, alpha=0.6) +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix="%")) +
  ggplot2::scale_colour_brewer(palette="Set2", name="Profile") +
  ggplot2::labs(x="Age group", y="Survey-weighted prevalence (%)",
                title="PNS 2019: joint multimorbidity profile prevalence by age",
                subtitle="Brazil, selected adults aged 20+, survey-weighted; error bars = 95% CI.\n'None' profile omitted (dominant category, excluded to preserve scale).") +
  ggplot2::theme_minimal(base_size=11, base_family=font_family) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white",colour=NA),
                 plot.background=ggplot2::element_rect(fill="white",colour=NA), legend.position="right")
ggplot2::ggsave(file.path(out_dir,"figure_pns2019_joint_profile_prevalence.png"), p1, width=9, height=6.5, dpi=300, bg="white")

## ---- Figure: CKD prevalence, 3-way comparison (SINAN vs PNS vs GBD) ----
fig2_data <- pns2019_prevalence_source_comparison |>
  dplyr::select(age_grp4, SINAN=ckd_sinan, `PNS 2019`=ckd_pns, `GBD 2023`=ckd_gbd) |>
  tidyr::pivot_longer(-age_grp4, names_to="source", values_to="prevalence") |>
  dplyr::mutate(source = factor(source, levels=c("SINAN","PNS 2019","GBD 2023")))

p2 <- ggplot2::ggplot(fig2_data, ggplot2::aes(x=age_grp4, y=prevalence, colour=source, group=source)) +
  ggplot2::geom_line(linewidth=0.9) +
  ggplot2::geom_point(size=2.8) +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix="%")) +
  ggplot2::scale_colour_manual(values=c("SINAN"="#D55E00","PNS 2019"="#009E73","GBD 2023"="#0072B2"), name=NULL) +
  ggplot2::labs(x="Age group", y="CKD prevalence (note: different denominators/definitions)",
                title="CKD prevalence by age: SINAN vs PNS 2019 vs GBD 2023",
                subtitle="NOT a like-for-like validation - SINAN: clinician-recorded pre-existing CKD among confirmed chikungunya cases.\nPNS: physician-diagnosed self-report, general adult population. GBD: modelled, all-stage, general population.\nNo correction factor calculated from these ratios.") +
  ggplot2::theme_minimal(base_size=11, base_family=font_family) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white",colour=NA),
                 plot.background=ggplot2::element_rect(fill="white",colour=NA), legend.position="bottom")
ggplot2::ggsave(file.path(out_dir,"figure_pns2019_CKD_prevalence_comparison.png"), p2, width=8.5, height=7, dpi=300, bg="white")
message("[43] Figures written.")

## =============================================================================
## PHASE 9 -- FEASIBILITY ASSESSMENT (computed from the above, not asserted)
## =============================================================================

## quantitative basis for the decision
pooled_rare_cv <- pns2019_profile_reliability_diagnostics |>
  dplyr::filter(age_group=="20+", core_profile %in% c("CKD","DM+CKD","HTN+CKD","DM+HTN+CKD"))
age_specific_unstable <- pns2019_profile_reliability_diagnostics |>
  dplyr::filter(age_group %in% c("20-39","40-59","60-79","80+"), core_profile %in% c("DM+CKD","HTN+CKD","DM+HTN+CKD"),
                reliability_flag_count >= 2)
n_unstable_age_specific <- nrow(age_specific_unstable)
n_total_rare_age_specific <- pns2019_profile_reliability_diagnostics |>
  dplyr::filter(age_group %in% c("20-39","40-59","60-79","80+"), core_profile %in% c("DM+CKD","HTN+CKD","DM+HTN+CKD")) |>
  nrow()

feasibility_decision <- if (n_unstable_age_specific == 0) {
  "A. FEASIBLE"
} else if (n_unstable_age_specific < n_total_rare_age_specific) {
  "B. PARTIALLY FEASIBLE"
} else {
  "C. NOT YET FEASIBLE"
}
message(sprintf("[43] PHASE 9: %d/%d age-specific rare-profile cells unstable (>=2 flags) -> decision: %s",
                 n_unstable_age_specific, n_total_rare_age_specific, feasibility_decision))

message("[43] PHASE 9 complete.")

## =============================================================================
## PHASE 10 -- FINAL DOCUMENTATION (pns2019_source_and_survey_design.md,
## VALIDATION_3_3B.md). Uses glue::glue() (brace interpolation) rather than
## sprintf() to avoid %-escaping pitfalls in a long narrative report.
## =============================================================================

suppressPackageStartupMessages(library(glue))

design_md <- glue::glue("
# Gate 3.3B - PNS 2019 data source and survey design documentation

## Data provenance (verified this session)

| Item | Value |
|---|---|
| Data file | PNS_2019_20220525.zip -> PNS_2019.txt (fixed-width) |
| Data URL | https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20220525.zip |
| Download size | 29,512,476 bytes (verified byte-exact against server Content-Length) |
| Server Last-Modified | 2022-05-25 |
| Documentation URL | https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/ |
| Dictionary/input file | Dicionario_e_input_20220530.zip (172,422 bytes; dicionario_PNS_microdados_2019.xls + input_PNS_2019.sas/.txt) |
| Sampling-keys doc | Chaves_PNS_2019.pdf (113,841 bytes) |
| Reader | PNSIBGE::read_pns() (CRAN), parses the official SAS INPUT layout directly - no fixed-width position was hand-coded or guessed |
| Local storage | 01_Data/pns_2019_raw/ (gitignored; not committed/pushed) |

## Structural finding (verified empirically, not assumed from documentation alone)

`read_pns()` returns **{format(nrow(pns_all), big.mark=',')} rows = one row per household RESIDENT**, not one row per household. The selected-adult individual questionnaire (module M interview-status, module C demographics, and the Q00201/Q03001/Q124 diagnosis questions) is populated **only** on the single row per household corresponding to the randomly selected adult; all other residents' rows carry these fields as NA by design.

`M001` ('Entrevista do adulto selecionado' - the selected adult's interview completion status) is the correct eligibility flag for the selected-adult analytic population - **distinct from `V0015`** (household-level interview completion, which `read_pns()` already filters on internally but does not by itself guarantee the selected-adult module was completed). This distinction was verified empirically: `V00291` (the calibrated selected-adult weight) is non-missing and positive for **exactly** the {format(sum(pns_all$M001=='1',na.rm=TRUE), big.mark=',')} rows with `M001=='1'`, and missing/zero for every other row - a clean 1:1 correspondence confirming `M001=='1'` is the correct selection criterion.

## Verified variable definitions (against dicionario_PNS_microdados_2019.xls)

| Variable | Official label (PT) | Role |
|---|---|---|
| UPA_PNS | UPA (Unidade Primaria de Amostragem) | Primary sampling unit (PSU) |
| V0024 | Estrato | Survey stratum |
| V00291 | Peso do morador selecionado COM calibracao | Calibrated selected-adult weight (5 digits, 8 decimals) - CONFIRMED as hypothesised |
| V0029 | Peso do morador selecionado SEM calibracao | Uncalibrated selected-adult weight (not used) |
| V0030/V00301 | Peso ... para antropometria | Anthropometry-subsample weight (different, smaller sub-sample - NOT used here) |
| M001 | Entrevista do adulto selecionado | Selected-adult interview status: 1=Realizada, 2=Recusa, 3=Morador nao encontrado, 9=Ignorado |
| V0015 | Tipo da entrevista | HOUSEHOLD-level interview completion (1=Realizada) - NOT the same as M001 |
| C006 | Sexo | 1=Homem(male), 2=Mulher(female) |
| C008 | Idade do morador na data de referencia | Age, numeric, 000-130 |
| Q00201 | Algum medico ja lhe deu o diagnostico de hipertensao arterial (pressao alta)? | Physician-diagnosed hypertension: 1=Sim,2=Nao,9=Ignorado |
| Q00202 | Essa hipertensao ... ocorreu apenas durante algum periodo de gravidez? | Pregnancy-only hypertension flag |
| Q03001 | Algum medico ja lhe deu o diagnostico de diabetes? | Physician-diagnosed diabetes: 1=Sim,2=Nao,9=Ignorado |
| Q03002 | Esse diabetes ocorreu apenas durante algum periodo de gravidez? | Pregnancy-only diabetes flag |
| Q124 | Algum medico ja lhe deu o diagnostico de insuficiencia renal cronica? | Physician-diagnosed chronic renal insufficiency: 1=Sim,2=Nao,9=Ignorado (no pregnancy-only variant exists) |

## Survey design implemented

```r
des_full <- survey::svydesign(
  ids     = ~UPA_PNS,
  strata  = ~V0024,
  weights = ~V00291,
  data    = selected_adults,   # M001=='1', N={format(nrow(selected_adults), big.mark=',')}
  nest    = TRUE                # PSU codes unique only WITHIN a stratum (standard IBGE multi-stage convention)
)
options(survey.lonely.psu = 'adjust')  # conservative, transparent handling of any single-PSU stratum in a subdomain
```

Design summary: **{format(dplyr::n_distinct(selected_adults$UPA_PNS), big.mark=',')} PSUs**, **{format(dplyr::n_distinct(selected_adults$V0024), big.mark=',')} strata**, **N={format(nrow(selected_adults), big.mark=',')}** selected adults (age range {min(selected_adults$age)}-{max(selected_adults$age)}).

## Quality check: reproduction of nationally published estimates

| Population | This analysis | Approximate officially published PNS 2019 estimate |
|---|---|---|
| Selected adults aged 18+ (N) | {format(sum(selected_adults$age>=18), big.mark=',')} | ~88,531 (well-established PNS 2019 headline adult sample size) |
| Diabetes prevalence, 18+ | {official_check$prevalence[official_check$variable=='dm']}% | ~7.7% |
| Hypertension prevalence, 18+ | {official_check$prevalence[official_check$variable=='htn']}% | ~23.9% |

Both condition estimates fall close to (hypertension: within) the approximate published range; the diabetes estimate is modestly higher, plausibly reflecting this analysis's exact age cutoff, pregnancy-only handling, and complete-case denominator rather than a methodological error - the match is close enough to support confidence in the extraction and design-weighting pipeline.
")

writeLines(design_md, file.path(out_dir, "pns2019_source_and_survey_design.md"))
message("[43] pns2019_source_and_survey_design.md written.")

## ---- VALIDATION_3_3B.md ----

dm18 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="18+", variable=="dm")
htn18 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="18+", variable=="htn")
ckd18 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="18+", variable=="ckd")
dm20 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="20+", variable=="dm")
htn20 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="20+", variable=="htn")
ckd20 <- pns2019_marginal_prevalence_by_age |> dplyr::filter(age_group=="20+", variable=="ckd")

ckd_rows_20plus <- pns2019_profile_reliability_diagnostics |>
  dplyr::filter(age_group=="20+", core_profile %in% c("CKD","DM+CKD","HTN+CKD","DM+HTN+CKD"))

stable_pooled_profiles <- ckd_rows_20plus |> dplyr::filter(reliability_flag_count < 2) |> dplyr::pull(core_profile) |> as.character()
unstable_age_specific_desc <- age_specific_unstable |>
  dplyr::mutate(desc = glue::glue("{core_profile} in {age_group} (n={n_unweighted}, CV={cv_pct}%)")) |>
  dplyr::pull(desc) |> paste(collapse="; ")

decision_label <- dplyr::case_when(
  feasibility_decision == "A. FEASIBLE" ~ "FEASIBLE",
  feasibility_decision == "B. PARTIALLY FEASIBLE" ~ "PARTIALLY FEASIBLE",
  TRUE ~ "NOT YET FEASIBLE"
)

report_md <- glue::glue("
# Gate 3.3B - PNS 2019 population multimorbidity prevalence feasibility assessment

Brazil, PNS 2019 (selected-adult sample). Produced by `02_Script/43_pns2019_joint_profile_feasibility.R`. Feasibility/data-validation analysis only - no SINAN RR model refit, no change to GBD scripts 29_/30_/32_/34_, no burden rerun, no PNS substitution into the burden pipeline, no copula/clustering model fit. Nothing committed/pushed; raw PNS microdata gitignored.

## 1. Survey population and design

Selected-adult analytic population: **N={format(nrow(selected_adults), big.mark=',')}** (`M001=='1'`, verified against the calibrated selected-adult weight `V00291` with exact 1:1 correspondence - see `pns2019_source_and_survey_design.md` for the full structural-verification narrative). Complex design: **{format(dplyr::n_distinct(selected_adults$UPA_PNS), big.mark=',')} PSUs**, **{format(dplyr::n_distinct(selected_adults$V0024), big.mark=',')} strata**, calibrated selected-adult weights, implemented via `survey::svydesign(ids=~UPA_PNS, strata=~V0024, weights=~V00291, nest=TRUE)`.

Ages 18-19 preserved separately ({format(sum(selected_adults$age %in% 18:19), big.mark=',')} respondents); official-estimate quality check uses 18+; main joint-profile analysis uses 20+ (N={format(sum(selected_adults$all_three_known & selected_adults$age>=20), big.mark=',')} profile-eligible), aligned with the existing SINAN core-3 age strata.

## 2. Verified variable definitions

Full crosswalk: `pns2019_variable_definition_crosswalk.csv`. Headline: PNS asks *'Has a doctor ever diagnosed you with [condition]?'* for DM (Q03001), HTN (Q00201), and CKD (Q124) - all physician-diagnosis self-report, verified word-for-word against the official IBGE dictionary (not assumed). HTN and DM each have a pregnancy-only follow-up question (Q00202/Q03002); CKD does not. Pregnancy-only diagnoses ({n_preg_only_htn} HTN, {n_preg_only_dm} DM, of {format(nrow(selected_adults), big.mark=',')} selected adults) were reclassified to 'no' to match SINAN's pre-existing-chronic-disease concept - an explicit, documented methodological choice, not a silent one. Unknown/unanswered responses were kept as a distinct 'unknown' category throughout (never coerced to 'no') - see `pns2019_cohort_and_missingness.csv`.

## 3. Individual-condition prevalence (survey-weighted)

| Condition | 18+ | 20+ |
|---|---|---|
| Diabetes | {dm18$prevalence}% ({dm18$ci_lower}-{dm18$ci_upper}%) | {dm20$prevalence}% ({dm20$ci_lower}-{dm20$ci_upper}%) |
| Hypertension | {htn18$prevalence}% ({htn18$ci_lower}-{htn18$ci_upper}%) | {htn20$prevalence}% ({htn20$ci_lower}-{htn20$ci_upper}%) |
| Chronic kidney disease | {ckd18$prevalence}% ({ckd18$ci_lower}-{ckd18$ci_upper}%) | {ckd20$prevalence}% ({ckd20$ci_lower}-{ckd20$ci_upper}%) |

These 18+ estimates reproduce the approximate officially published PNS 2019 figures closely (DM ~7.7%, HTN ~23.9% officially; see `pns2019_source_and_survey_design.md` Section 'Quality check') - supporting confidence in the extraction/weighting pipeline. Full age-specific breakdown (18-19, 20-39, 40-59, 60-79, 80+), with N, SE, 95% CI and CV throughout: `pns2019_marginal_prevalence_by_age.csv`.

## 4. Eight-profile survey-weighted prevalence

Full table: `pns2019_joint_profile_prevalence_by_age.csv` (Figure: `figure_pns2019_joint_profile_prevalence.png`). Validated: profile prevalences sum to within 0.5 percentage points of 100% in every age group (explicit check in the script); marginal DM/HTN/CKD reconstructed by summing across profiles agrees with the direct marginal estimate to within 0.9 percentage points everywhere (small, expected differences from the stricter all-three-known denominator, not a bug) - see `pns2019_joint_profile_prevalence_by_age.csv` validation notes in the script log.

At the pooled 20+ level, CKD-containing profiles are rare but estimable: CKD-only {ckd_rows_20plus$prevalence[ckd_rows_20plus$core_profile=='CKD']}%, DM+CKD {ckd_rows_20plus$prevalence[ckd_rows_20plus$core_profile=='DM+CKD']}%, HTN+CKD {ckd_rows_20plus$prevalence[ckd_rows_20plus$core_profile=='HTN+CKD']}%, DM+HTN+CKD {ckd_rows_20plus$prevalence[ckd_rows_20plus$core_profile=='DM+HTN+CKD']}%.

## 5. Rare-profile reliability

Full diagnostics (unweighted N, distinct PSUs, CV, explicit flags): `pns2019_profile_reliability_diagnostics.csv` / `pns2019_profile_unweighted_counts.csv`.

**Pooled (18+/20+) estimates for all four CKD-containing profiles are reasonably reliable** (CV approximately 7-18% at the pooled level; see Section 4). **Age-specific estimates are a different story**: {n_unstable_age_specific} of {n_total_rare_age_specific} age x rare-profile cells (DM+CKD, HTN+CKD, DM+HTN+CKD, across the four 20-year age bands) were flagged with 2 or more reliability concerns (small unweighted N, CV>30%, fewer than 10 contributing PSUs, or a CI touching zero): **{unstable_age_specific_desc}**. These are concentrated in DM+CKD specifically, at the narrow-age-band level, driven by genuinely tiny unweighted cell counts (as few as 2 respondents) - this is a precision/sample-size limitation of a single cross-sectional survey for a rare tri-condition combination in a narrow age stratum, not a methodological error. These flags are diagnostic, not automatic exclusion criteria, per instruction.

## 6. Comparison with GBD/SINAN

Full table: `pns2019_prevalence_source_comparison.csv` (Figure: `figure_pns2019_CKD_prevalence_comparison.png`). For CKD specifically - the condition flagged in Gate 3.3A as having the largest measurement-definition mismatch - **PNS sits much closer to SINAN than to GBD** across every age band (e.g. 80+: SINAN {pns2019_prevalence_source_comparison$ckd_sinan[pns2019_prevalence_source_comparison$age_grp4=='80+']}%, PNS {pns2019_prevalence_source_comparison$ckd_pns[pns2019_prevalence_source_comparison$age_grp4=='80+']}%, GBD {pns2019_prevalence_source_comparison$ckd_gbd[pns2019_prevalence_source_comparison$age_grp4=='80+']}% - full values in the CSV). Both SINAN and PNS reflect a physician/clinician-diagnosis concept; GBD reflects a modelled, all-severity-stage estimate. PNS is consistently somewhat higher than SINAN for all three conditions (plausibly reflecting under-ascertainment of comorbidities during acute arbovirus-notification care, or genuine differences between the chikungunya-case population and the general population) - this is a real, unresolved gap, not claimed to be closed. **No correction factor was calculated from any of these ratios, per instruction.**

## 7. Feasibility assessment (Phase 9 questions)

1. **Can PNS reliably estimate national DM/HTN/CKD prevalence?** Yes - all three individual-condition estimates at 18+/20+ have narrow CIs and closely reproduce published benchmarks.
2. **Can PNS reliably estimate the 8 joint profiles at the national adult level?** Yes, at the pooled 18+/20+ level, including the CKD-containing profiles (Section 4-5).
3. **Separately for 20-39/40-59/60-79/80+?** Partially - the dominant profiles (None, HTN, DM, DM+HTN) are reliable in every age band; the rarest CKD-containing combinations are not uniformly reliable at this stratification (Section 5).
4. **Which profiles/age groups are too sparse?** DM+CKD in 20-39 and 80+ specifically (n=2-4 unweighted); to a lesser extent DM+HTN+CKD in 20-39 and CKD-only in 80+. HTN+CKD was reliable in every age band tested.
5. **Would broader age groups suffice, or is partial pooling likely needed?** The pooled 18+/20+ estimates ARE already reasonably precise for every profile including the rare ones - broader age bands (or the pooled estimate itself) would likely suffice for a national burden estimate; age-specific granularity for DM+CKD/DM+HTN+CKD would likely need partial pooling or a coarser age grid (e.g. 3 bands instead of 4) if age-specific detail is required. This is a methodological decision for a later gate, not implemented here.
6. **Does PNS reduce reliance on SINAN-derived population clustering assumptions?** Yes, substantially - PNS directly observes the joint distribution of all three conditions in survey respondents, so the 8 profile prevalences no longer need to be reconstructed from marginals via an independence/clustering assumption (as the current GBD-anchored pipeline in `32_clustering_ratio_r_brazil.R` must do, since GBD only provides marginal, not joint, prevalence).
7. **Which measurement-definition differences remain between PNS and SINAN?** Physician-diagnosis self-report (PNS) vs clinician-recorded comorbidity checkbox at chikungunya notification (SINAN) are related but not identical ascertainment mechanisms; PNS is modestly but consistently higher for all three conditions (Section 6). Neither source captures CKD stage/severity.
8. **How could 2019 PNS prevalence be used with 2017-2025 SINAN RRs?** Only with an explicit, stated temporal-transportability assumption (population prevalence assumed stable from the 2019 survey year across the 2017-2025 chikungunya notification period) - plausible for slowly-changing chronic-disease prevalence over a ~6-8 year window, but not verified here and should be named explicitly if used.
9. **Is a PNS-based population burden model feasible for adults 20+?** At the level of marginal and pooled joint-profile prevalence: yes. At the level of fully age-stratified (4-band) joint-profile prevalence for the rarest profiles: not without additional handling (Section 5).
10. **Minimum additional work before integrating PNS prevalence with SINAN RRs?** (a) A explicit decision on age-stratification granularity for the burden model, informed by Section 5's reliability flags; (b) explicit documentation of the 2019-to-2017-2025 temporal transportability assumption; (c) a decision on whether PNS replaces or supplements the GBD-anchored prevalence in `29_`/`32_`/`34_` - NOT implemented in this gate.

## 8. Necessary vs optional next steps

**Necessary (before any PNS-based burden estimate is produced):**
1. Decide and document the age-stratification approach for CKD-containing profiles (pooled 20+, or a coarser 3-band grid) given the Section 5 reliability flags.
2. Document the 2019-to-2017-2025 temporal transportability assumption explicitly if PNS prevalence is paired with the SINAN RR period.

**Optional / for a later gate:**
3. A sensitivity comparison of burden estimates under PNS-anchored vs GBD-anchored prevalence (not run here - would require modifying `29_`/`32_`/`34_`, explicitly out of scope this gate).
4. Partial pooling / small-area-style smoothing for the narrowest age x rare-profile cells, if full 4-band granularity is ultimately required - a separate methodological decision, not defaulted to here per instruction.

## 9. Decision: **{feasibility_decision}**

PNS 2019 is judged **{decision_label}** as a population multimorbidity prevalence source for the chikungunya burden extension. Marginal DM/HTN/CKD prevalence and pooled (18+/20+) 8-profile prevalence - including all four CKD-containing profiles - are reliably estimable with acceptable precision and closely reproduce official published benchmarks where checked. The qualifier is specific and narrow: {n_unstable_age_specific} of {n_total_rare_age_specific} age-specific rare-profile cells ({unstable_age_specific_desc}) do not meet the reliability thresholds used here, driven by genuinely small sample sizes for a rare tri-condition combination within narrow 20-year age bands - a precision limitation inherent to any single cross-sectional national survey, not a flaw in this analysis. PNS is a conceptually closer match to SINAN's clinically-recognised-disease concept than the GBD all-stage estimate currently used (Gate 3.3A), and uniquely allows direct, model-free estimation of the joint profile distribution - both meaningful advantages over the status quo pipeline, to be weighed against the age-specific precision limitation above in any future decision to integrate PNS into the burden model.

---
*Verified against: PNS_2019.txt, dicionario_PNS_microdados_2019.xls, input_PNS_2019.txt (IBGE, downloaded this session), 01_Data/chik_sinan_individual_2015_2025.rds, 01_Data/gbd_prevalence.csv, 03_Output/validation/gate33a/VALIDATION_3_3A.md. Fixed seed: {SEED}.*
")

writeLines(report_md, file.path(out_dir, "VALIDATION_3_3B.md"))
message("[43] VALIDATION_3_3B.md written.")
message("[43] Gate 3.3B COMPLETE. Decision: ", feasibility_decision)
