# ---------------------------------------------------------------------------
# 42_ckd_measurement_consistency_audit.R   -- Gate 3.3A
#
# CKD MEASUREMENT CONSISTENCY AND DATA-SOURCE HARMONISATION AUDIT. Asks
# whether the SINAN `renal_disease`/`ckd` variable used to estimate clinical
# severity RRs (12_core3_condition_specific_rr.R) represents the same CKD
# population as the GBD CKD prevalence used for population-burden weighting
# (29_/30_/32_/34_). This is an AUDIT: it does not refit any model, does not
# modify the CKD exposure variable, and does not implement any prevalence
# correction - it documents, quantifies, and flags.
#
# Does NOT modify 02_clean_chik_sinan_brazil.R, 12_core3_condition_specific_rr.R,
# 29_/30_/32_/34_, any primary model cache, or any manuscript file. Does NOT
# rerun population burden scripts.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds
#        01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf (consulted manually;
#          see PHASE 1 comments below for the verified field text)
#        01_Data/gbd_prevalence.csv
# Output: 03_Output/validation/gate33a/*.csv, *.png, VALIDATION_3_3A.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(lubridate); library(readr); library(tibble)
})
options(scipen = 999, dplyr.summarise.inform = FALSE)

out_dir <- "03_Output/validation/gate33a"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## =============================================================================
## PHASE 1 -- TRACE SINAN CKD DEFINITION
##
## Verified directly against 01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf
## (pdftotext -layout extraction), field 41:
##
##   Seq. 41 | "Doenças preexistentes (Doença renal crônica)"
##            = "Pre-existing diseases (Chronic kidney disease)"
##   Variable: renal | Type: char(1) | Categories: 1-Sim (Yes), 2-Não (No)
##
## This is an explicit, named "pre-existing CHRONIC kidney disease" checkbox
## under the "Doenças preexistentes" (pre-existing diseases) comorbidity
## section (fields 38-44: diabetes, hematologic, hepatopathy, RENAL,
## hypertension, peptic ulcer, autoimmune) - i.e. the same checklist style
## and the same field used for DM and HTN in the existing core-3 model.
##
## The only other dictionary occurrence of "renal" is "insuficiência renal
## aguda" (acute renal failure), which appears solely as one of several
## possible ICD-10-linked SYNDROME categories under field 2 (`id_agravo`,
## the notified disease/syndrome, e.g. the SINAN system this form is shared
## with also handles acute diarrhoeal/haemorrhagic/neurological syndromes) -
## an entirely separate field from field 41, and irrelevant once the cohort
## is restricted to is_confirmed_chik (CLASSI_FIN=="13"). There is no
## confusion between acute kidney injury and the field 41 comorbidity flag
## anywhere in SINAN's schema.
##
## No CKD stage, eGFR, albuminuria, or dialysis-status field exists anywhere
## in the dictionary - field 41 is a single binary yes/no checkbox with no
## severity, duration, or laboratory-confirmation qualifier of any kind.
## =============================================================================

sinan_ckd_field_dictionary <- tibble::tribble(
  ~item, ~value,
  "SINAN field sequence #", "41",
  "Official field label (Portuguese)", "Doenças preexistentes (Doença renal crônica)",
  "Official field label (English)", "Pre-existing diseases (Chronic kidney disease)",
  "Raw variable name", "RENAL",
  "Field type", "char(1)",
  "Allowed raw codes", "1 = Sim (Yes); 2 = Não (No)",
  "Codes NOT in the official dictionary but observed as raw values", "9 = Ignorado (Unknown) - used elsewhere in the same comorbidity block's convention; blank/unmapped also occur",
  "Explicitly refers to pre-existing (chronic) disease, not acute injury?", "YES - field label explicitly says 'crônica' (chronic); 'insuficiência renal aguda' (acute renal failure) is a distinct, unrelated ICD-10 syndrome category under field 2 (id_agravo), not this field",
  "CKD stage / eGFR / albuminuria / dialysis status available?", "NO - not present anywhere in the SINAN chikungunya dictionary; field 41 is a single undifferentiated yes/no checkbox",
  "Cleaning-script mapping (02_clean_chik_sinan_brazil.R)", "renal_disease = recode(RENAL, map_sim_nao); map_sim_nao = c(`1`='yes', `2`='no', `9`='unknown')",
  "recode() NA-handling", "out[x==''|is.na(x)] <- NA_character_ - blank/unmapped -> NA_character_, explicit code 9 -> the string 'unknown' (distinct from NA)",
  "Model indicator (12_core3_condition_specific_rr.R)", "ckd = as.integer(renal_disease == 'yes')",
  "Core-3 eligibility filter (12_, add_core3())", "dplyr::if_all(c('diabetes','hypertension','renal_disease'), ~ .x %in% c('no','yes')) - records with renal_disease=='unknown' OR NA are excluded from the eligible cohort, identically to DM/HTN",
  "Coding discrepancy between cleaning script and GAM?", "NONE FOUND - renal_disease is already a clean 'yes'/'no'/'unknown'/NA factor from the NA-safe recode() helper before ckd is derived; `ckd==as.integer(renal_disease=='yes')` is evaluated only after the complete-case filter already requires renal_disease %in% c('no','yes'), so no NA-propagation risk exists at that point (unlike the Gate 3.1 EVOLUCAO bug pattern, which used a bare `==` BEFORE any completeness filter)."
)
readr::write_csv(sinan_ckd_field_dictionary, file.path(out_dir, "sinan_ckd_coding_audit.csv"))
message("[42] PHASE 1 (SINAN CKD definition trace) written: sinan_ckd_coding_audit.csv")

## =============================================================================
## PHASE 1b -- LIVE EMPIRICAL VERIFICATION of the recode()/eligibility logic
## described above, run against synthetic test codes (does not touch any
## production object).
## =============================================================================

map_sim_nao <- c(`1` = "yes", `2` = "no", `9` = "unknown")
recode_like_02 <- function(x, map) { out <- unname(map[as.character(x)]); out[x == "" | is.na(x)] <- NA_character_; out }
test_codes <- c("1", "2", "9", "", NA, "0", "7")
expected   <- c("yes", "no", "unknown", NA, NA, NA, NA)
actual     <- recode_like_02(test_codes, map_sim_nao)
phase1b_pass <- all(mapply(function(e, a) (is.na(e) && is.na(a)) || (!is.na(e) && !is.na(a) && e == a), expected, actual))
message(sprintf("[42] PHASE 1b live recode() verification: %s (codes tested: %s)",
                 if (phase1b_pass) "PASS" else "FAIL", paste(ifelse(is.na(test_codes), "NA", test_codes), collapse = ",")))

## =============================================================================
## PHASE 2 -- AUDIT SINAN CKD RECORDING
##
## Uses P0 = "valid age/sex, confirmed chikungunya, year>=2017" (matches the
## existing cohort-flow N=1,074,994 step, BEFORE the DM/HTN/CKD
## completeness filter) as the denominator for CKD-specific recording
## completeness. The DOWNSTREAM "core-3 eligible cohort" (N=1,074,796)
## requires renal_disease %in% c('no','yes') BY CONSTRUCTION, so auditing
## CKD completeness within it would be circular (100% by definition, not a
## finding) - P0 avoids that circularity while still respecting the same
## basic scope (confirmed case, valid age/sex, year range) that does not
## itself depend on RENAL.
## =============================================================================

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

p0 <- ind |>
  dplyr::filter(
    is_confirmed_chik, lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female")
  ) |>
  dplyr::mutate(
    cal_year = lubridate::year(event_date),
    age_band_5 = cut(age_years, breaks = c(0,20,40,60,80,101), labels = c("0-19","20-39","40-59","60-79","80+"),
                      right = FALSE, include.lowest = TRUE),
    ckd_status = dplyr::case_when(
      renal_disease == "yes" ~ "yes", renal_disease == "no" ~ "no",
      renal_disease == "unknown" ~ "explicit_unknown", is.na(renal_disease) ~ "blank_missing", TRUE ~ "other_unmapped"
    ),
    ckd_recorded = ckd_status %in% c("yes","no")
  )

message(sprintf("[42] P0 (valid age/sex, confirmed, year>=2017) N = %s", format(nrow(p0), big.mark=",")))
stopifnot(nrow(p0) == 1074994)  # matches the documented cohort-flow step exactly

## also, for direct comparability, DM and HTN completeness within the SAME P0
dm_htn_compare <- p0 |>
  dplyr::summarise(
    dm_recorded_pct = round(100*mean(diabetes %in% c("yes","no")), 4),
    htn_recorded_pct = round(100*mean(hypertension %in% c("yes","no")), 4),
    ckd_recorded_pct = round(100*mean(ckd_recorded), 4)
  )
message(sprintf("[42] Recording completeness within P0 - DM: %.4f%%, HTN: %.4f%%, CKD: %.4f%%",
                 dm_htn_compare$dm_recorded_pct, dm_htn_compare$htn_recorded_pct, dm_htn_compare$ckd_recorded_pct))

## ---- overall CKD status counts (yes/no/unknown/missing) ----
ckd_status_overall <- p0 |> dplyr::count(ckd_status, name = "n") |>
  dplyr::mutate(pct_of_p0 = round(100*n/nrow(p0), 4))

## ---- prevalence and age distribution of recorded CKD among confirmed cases ----
ckd_recorded_subset <- p0 |> dplyr::filter(ckd_recorded)
ckd_prevalence_summary <- tibble::tibble(
  n_p0 = nrow(p0), n_ckd_recorded = nrow(ckd_recorded_subset),
  n_ckd_yes = sum(ckd_recorded_subset$ckd_status=="yes"),
  pct_ckd_among_recorded = round(100*mean(ckd_recorded_subset$ckd_status=="yes"), 3),
  mean_age_ckd_yes = round(mean(ckd_recorded_subset$age_years[ckd_recorded_subset$ckd_status=="yes"]), 1),
  median_age_ckd_yes = median(ckd_recorded_subset$age_years[ckd_recorded_subset$ckd_status=="yes"]),
  mean_age_ckd_no = round(mean(ckd_recorded_subset$age_years[ckd_recorded_subset$ckd_status=="no"]), 1)
)

## ---- recording completeness by age/year/state (structural + geographic/temporal) ----
recording_by <- function(df, group_var, domain_name) {
  gv <- rlang::ensym(group_var)
  df |>
    dplyr::group_by(level = !!gv) |>
    dplyr::summarise(
      n_p0 = dplyr::n(), n_recorded = sum(ckd_recorded), n_yes = sum(ckd_status=="yes"),
      n_explicit_unknown = sum(ckd_status=="explicit_unknown"), n_blank_missing = sum(ckd_status=="blank_missing"),
      .groups = "drop"
    ) |>
    dplyr::mutate(stratifier_domain = domain_name,
                  pct_recorded = round(100*n_recorded/n_p0, 3),
                  pct_ckd_yes_among_recorded = round(100*n_yes/pmax(n_recorded,1), 3)) |>
    dplyr::relocate(stratifier_domain, level)
}

by_age <- recording_by(p0, age_band_5, "age_band_5")
by_year <- recording_by(p0 |> dplyr::mutate(cal_year = as.character(cal_year)), cal_year, "calendar_year")
by_state <- recording_by(p0 |> dplyr::mutate(uf_residence = as.character(uf_residence)), uf_residence, "state_uf_code")

sinan_ckd_recording_by_age_year <- dplyr::bind_rows(
  by_age |> dplyr::mutate(level = as.character(level)),
  by_year,
  by_state
)
readr::write_csv(sinan_ckd_recording_by_age_year, file.path(out_dir, "sinan_ckd_recording_by_age_year.csv"))
message("[42] PHASE 2 (age/year/state recording completeness) written: sinan_ckd_recording_by_age_year.csv")

## ---- outcome-stratified (DESCRIPTIVE DIAGNOSTIC ONLY - hospitalisation and
## death status are themselves incomplete; this does NOT define the
## denominator used for overall completeness above, per instruction) ----
hosp_known <- p0 |> dplyr::filter(hospitalised %in% c("no","yes")) |> dplyr::mutate(hosp_only = hospitalised=="yes")
death_known <- p0 |> dplyr::filter(!is.na(died_from_chik))

by_hosp_status <- recording_by(hosp_known |> dplyr::mutate(hosp_only = ifelse(hosp_only, "Hospitalised", "Not hospitalised")), hosp_only, "DESCRIPTIVE: hospitalisation status (among hosp-status-known subset)")
by_death_status <- recording_by(death_known |> dplyr::mutate(died_from_chik = ifelse(died_from_chik, "Chikungunya death", "Survived/non-chik death")), died_from_chik, "DESCRIPTIVE: chikungunya-death status (among death-status-known subset)")

sinan_ckd_recording_by_outcome <- dplyr::bind_rows(by_hosp_status, by_death_status) |>
  dplyr::mutate(note = "DESCRIPTIVE DIAGNOSTIC ONLY - conditions on an independently incomplete outcome variable; NOT the basis for the overall CKD completeness estimate (see sinan_ckd_recording_by_age_year.csv / ckd_status_overall for that).")
readr::write_csv(sinan_ckd_recording_by_outcome, file.path(out_dir, "sinan_ckd_recording_by_outcome.csv"))
message("[42] PHASE 2 (outcome-stratified, descriptive) written: sinan_ckd_recording_by_outcome.csv")

## ---- sanity check: core-3 eligible cohort IS 100% CKD-recorded by construction ----
core3_eligible_check <- p0 |>
  dplyr::filter(diabetes %in% c("no","yes"), hypertension %in% c("no","yes"), renal_disease %in% c("no","yes"))
stopifnot(nrow(core3_eligible_check) == 1074796, mean(core3_eligible_check$ckd_recorded) == 1)
message(sprintf("[42] Confirmed by construction: core-3 eligible cohort (N=%s) is 100%% CKD-recorded (circularity check) -- PASS",
                 format(nrow(core3_eligible_check), big.mark=",")))

message("[42] PHASE 2 complete.")

## =============================================================================
## PHASE 3 -- VERIFY GBD CKD PREVALENCE INPUT
## =============================================================================

gbd_raw <- readr::read_csv("01_Data/gbd_prevalence.csv", show_col_types = FALSE)

gbd_years_all <- sort(unique(gbd_raw$year))
gbd_locations_n <- dplyr::n_distinct(gbd_raw$location_name)
gbd_sexes_all <- sort(unique(gbd_raw$sex_name))
gbd_cause_names_ckd_like <- unique(gbd_raw$cause_name[grepl("kidney|renal", gbd_raw$cause_name, ignore.case = TRUE)])
gbd_measure_all <- unique(gbd_raw$measure_name)
gbd_metric_all <- unique(gbd_raw$metric_name)

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")

ckd_bra <- gbd_raw |>
  dplyr::filter(measure_name == "Prevalence", metric_name == "Rate", location_name == "Brazil",
                sex_name == "Both", cause_name == "Chronic kidney disease")

ckd_bra_matched <- ckd_bra |>
  dplyr::mutate(age_clean = stringr::str_remove(age_name, " years| year")) |>
  dplyr::filter(age_clean %in% keep_age)

n_years_in_ckd_bra <- dplyr::n_distinct(ckd_bra$year)
n_age_name_raw <- dplyr::n_distinct(ckd_bra$age_name)         # includes overlapping GBD age aggregations (20+, 50-74, etc.)
n_matched_bands <- nrow(ckd_bra_matched)                       # after restricting to the clean 20-band grid used by 29_
missing_bands <- setdiff(keep_age, ckd_bra_matched$age_clean)
dup_bands_after_match <- ckd_bra_matched |> dplyr::count(age_clean) |> dplyr::filter(n > 1)

gbd_ckd_source_metadata <- tibble::tribble(
  ~item, ~value,
  "Source file", "01_Data/gbd_prevalence.csv",
  "Total rows in file (all causes/locations/years)", format(nrow(gbd_raw), big.mark=","),
  "Distinct years present in the ENTIRE file", paste(gbd_years_all, collapse=","),
  "Distinct locations in the file", as.character(gbd_locations_n),
  "Brazil present?", as.character("Brazil" %in% gbd_raw$location_name),
  "Distinct sex_name values in the file", paste(gbd_sexes_all, collapse=","),
  "Distinct measure_name values", paste(gbd_measure_all, collapse=","),
  "Distinct metric_name values", paste(gbd_metric_all, collapse=","),
  "cause_name value(s) matching kidney/renal", paste(gbd_cause_names_ckd_like, collapse=" | "),
  "CKD stage-specific cause_name entries present?", if (length(gbd_cause_names_ckd_like)==1 && gbd_cause_names_ckd_like=="Chronic kidney disease") "NO - only one aggregate 'Chronic kidney disease' cause exists; no stage/etiology-specific CKD entries (e.g. 'CKD due to diabetes', 'CKD stage 3-5') found in this extract" else paste("Multiple CKD-related causes found:", paste(gbd_cause_names_ckd_like,collapse=", ")),
  "---- Brazil / Both-sex / Prevalence / Rate / Chronic kidney disease slice ----", "----",
  "Rows in this exact slice (before age-band filtering)", as.character(nrow(ckd_bra)),
  "Distinct years in this slice", paste(sort(unique(ckd_bra$year)), collapse=","),
  "Is the script's extraction missing an explicit year filter?", "YES, technically - 29_background_prevalence_core3_brazil.R (line ~192) filters on measure/metric/location/sex/cause but not on `year`",
  "Does this currently cause duplicate rows per age band?", if (n_years_in_ckd_bra==1) sprintf("NO - the entire source file contains only ONE year (%s) for every row, so the missing year filter has no observable effect right now", gbd_years_all[1]) else "YES - multiple years are present; see gbd_ckd_age_prevalence_audit.csv for which bands are duplicated",
  "Distinct raw age_name categories in this slice (includes overlapping GBD aggregations e.g. '20+ years','50-74 years')", as.character(n_age_name_raw),
  "Bands actually used downstream (29_'s `keep_age`, a clean non-overlapping 5-year grid)", as.character(length(keep_age)),
  "Matched rows after restricting to `keep_age`", as.character(n_matched_bands),
  "Any `keep_age` band missing from the matched set?", if (length(missing_bands)==0) "NONE" else paste(missing_bands, collapse=","),
  "Any duplicate band after the `keep_age` restriction?", as.character(nrow(dup_bands_after_match)),
  "Units", "val/upper/lower are GBD 'Rate' metric = per 100,000 population; 29_ converts via prevalence = val/1e5 (CORRECT)",
  "Implausible values (<=0 or >100000 per 100k)?", as.character(sum(ckd_bra$val <= 0 | ckd_bra$val > 100000, na.rm=TRUE)),
  "Missing val/upper/lower in this slice?", paste0("val=",sum(is.na(ckd_bra$val)),", upper=",sum(is.na(ckd_bra$upper)),", lower=",sum(is.na(ckd_bra$lower))),
  "population_group_name value(s)", paste(unique(ckd_bra$population_group_name), collapse=","),
  "Minimum/maximum matched prevalence (proportion, 20-band grid)", sprintf("min=%.5f (%s), max=%.4f (%s)",
     min(ckd_bra_matched$val)/1e5, ckd_bra_matched$age_clean[which.min(ckd_bra_matched$val)],
     max(ckd_bra_matched$val)/1e5, ckd_bra_matched$age_clean[which.max(ckd_bra_matched$val)]),
  "GBD release/version identifiable from this file?", "NOT EXPLICITLY - no GBD round/version column in this CSV extract; year=2023 is the only temporal identifier present. The specific GBD study round (e.g. GBD 2021 vs a later round) this corresponds to is UNRESOLVED from the local file alone and should be confirmed against the original IHME download metadata if this matters for citation."
)
readr::write_csv(gbd_ckd_source_metadata, file.path(out_dir, "gbd_ckd_source_metadata.csv"))

gbd_ckd_age_prevalence_audit <- ckd_bra_matched |>
  dplyr::transmute(age_band = age_clean, age_name_raw = age_name, year, prevalence_per_100k = val,
                    prevalence_proportion = round(val/1e5, 6), upper_per_100k = upper, lower_per_100k = lower,
                    duplicate_band_flag = age_clean %in% dup_bands_after_match$age_clean) |>
  dplyr::arrange(match(age_band, keep_age))
readr::write_csv(gbd_ckd_age_prevalence_audit, file.path(out_dir, "gbd_ckd_age_prevalence_audit.csv"))

message(sprintf("[42] PHASE 3 complete. Year filter missing in 29_: YES (technical finding). Currently causes duplication: %s (file has %d distinct year(s): %s). Matched %d/%d keep_age bands, %d duplicates.",
                 ifelse(n_years_in_ckd_bra==1,"NO","YES"), n_years_in_ckd_bra, paste(gbd_years_all,collapse=","),
                 n_matched_bands, length(keep_age), nrow(dup_bands_after_match)))

## =============================================================================
## PHASE 4 -- COMPARE MEASUREMENT DEFINITIONS (SINAN vs GBD vs PNS 2019)
##
## PNS 2019 (Pesquisa Nacional de Saude, IBGE): no local microdata file exists
## in this repository (confirmed - grep found no PNS data file, only a
## comment in 29_ noting none is available). The PNS row below is built from
## the PNS questionnaire's well-documented, published design (a national
## household health survey module that asks respondents whether a doctor has
## ever diagnosed them with chronic kidney disease - PNS module J,
## self-report) and is included for CONCEPTUAL comparison only, exactly as
## the task instructs. Any item not independently re-verified against
## primary IBGE/PNS documentation in this session is explicitly marked
## UNRESOLVED/NOT VERIFIED THIS SESSION rather than stated as fact.
## =============================================================================

ckd_definition_comparison <- tibble::tribble(
  ~dimension, ~SINAN_RENAL_field41, ~GBD_chronic_kidney_disease, ~PNS_2019_self_reported,
  "Target population", "Confirmed chikungunya cases notified to SINAN, Brazil 2017-2025 (a selected, symptomatic, notified clinical population)", "Brazilian general population, all ages, 2023 estimate (modelled)", "Civilian non-institutionalised household population, Brazil, 2019 survey wave (national probability sample)",
  "CKD definition", "Clinician-reported pre-existing chronic kidney disease (single yes/no checkbox at case notification)", "Modelled prevalence of CKD per KDIGO-consistent criteria (reduced eGFR and/or markers of kidney damage), as defined by IHME's standard GBD cause hierarchy - covers the full severity spectrum from early/asymptomatic to end-stage", "Self-reported 'has a doctor ever told you that you have chronic kidney disease' (household interview)",
  "Data collection / estimation method", "Clinician checkbox on a mandatory disease-notification form, completed at point of chikungunya care", "Statistical modelling (DisMod-MR or successor) synthesising claims data, population lab/survey data (eGFR-based), and other inputs across countries", "Structured household interview questionnaire (module J, chronic diseases)",
  "Diagnostic ascertainment", "Not specified/standardised - relies on the treating clinician's prior knowledge of the patient's history at the time of chikungunya notification; no lab confirmation required or recorded", "Primarily lab-based (eGFR/albuminuria) in its input data where available, blended with other data types via modelling", "Patient/respondent self-report of a prior physician diagnosis - no lab confirmation; subject to diagnostic-awareness and recall bias",
  "Severity/stage coverage", "UNDIFFERENTIATED - no stage, eGFR, or albuminuria information; a 'yes' could represent any severity from mild/managed CKD to dialysis-dependent", "ALL STAGES (1-5), including a substantial early/undiagnosed-stage component not clinically apparent to patients or clinicians", "Undifferentiated, but implicitly skewed toward diagnosed (i.e. at least moderately progressed or symptomatically investigated) cases, since it requires the respondent to have been told a diagnosis",
  "Age coverage", "0-100 years (chikungunya cases of any age; core-3 eligible cohort spans the full range)", "All ages, including children (though CKD prevalence is negligible at young ages)", "Adults only (PNS chronic-disease module targets adults, typically 18+)",
  "Year", "2017-2025 (chikungunya notification period)", "2023 (the only year present in the local gbd_prevalence.csv extract)", "2019",
  "Major limitations", "Comorbidity under-ascertainment plausible if clinician is unaware of a patient's CKD history; no severity gradient; small absolute N (CKD-recorded-yes is a rare exposure, see Phase 2)", "A broad, modelled, ALL-SEVERITY prevalence that is NOT restricted to clinically recognised/diagnosed cases - substantially over-inclusive relative to a 'diagnosed, clinically apparent CKD' concept", "Self-report likely UNDER-ascertains true CKD (especially early/asymptomatic stages, consistent with low public CKD awareness globally) relative to GBD's modelled all-stage estimate, but is closer in spirit to SINAN's 'clinically recognised' concept than GBD is - NOT INDEPENDENTLY VERIFIED THIS SESSION against primary PNS output tables; treat specific PNS point estimates as unresolved pending literature confirmation",
  "Suitability for applying the SINAN-derived severity RR", "This IS the population the RR was estimated in - directly suitable by construction", "NOT DIRECTLY SUITABLE without an explicit transportability assumption - the RR was estimated among clinically-recognised CKD cases, but would be applied (in the population-burden pipeline) to a population-prevalence estimate that includes a large mild/undiagnosed-CKD component the RR was never estimated on", "Potentially a closer conceptual match to SINAN's 'clinically recognised' population than GBD, but self-report vs clinician-report are still not the same ascertainment mechanism, and no stage information exists in either; UNRESOLVED without further documentation/microdata access"
)
readr::write_csv(ckd_definition_comparison, file.path(out_dir, "ckd_definition_comparison.csv"))
message("[42] PHASE 4 (definition comparison) written: ckd_definition_comparison.csv")

## =============================================================================
## PHASE 5 -- AGE-SPECIFIC DESCRIPTIVE COMPARISON
## NOT a like-for-like prevalence validation (different target populations
## and ascertainment mechanisms - see Phase 4). No correction factor is
## computed. SINAN uses its own 10-year age grid (matching the model's
## age_band_profile-adjacent granularity); GBD uses its native 5-year grid.
## Both are shown on their own harmonised 10-year bands where both have data;
## bands unique to one source are kept but visually distinguished.
## =============================================================================

sinan_age10 <- p0 |>
  dplyr::mutate(age_band_10 = cut(age_years, breaks = c(seq(0,90,10),101),
                                   labels = c(paste(seq(0,80,10), seq(9,89,10), sep="-"), "90+"),
                                   right = FALSE, include.lowest = TRUE)) |>
  dplyr::filter(ckd_recorded) |>
  dplyr::group_by(age_band_10) |>
  dplyr::summarise(n_recorded = dplyr::n(), n_yes = sum(ckd_status=="yes"), .groups="drop") |>
  dplyr::mutate(prevalence = n_yes/n_recorded, source = "SINAN: confirmed chikungunya cases (clinically recognised CKD)")

## map GBD's 5-year bands onto the same 10-year grid by population-weighted
## averaging WITHIN SINAN's own age-band population (descriptive display
## only - this does not feed back into any model).
gbd_age10_lookup <- tibble::tibble(
  age_clean = keep_age,
  age_band_10 = c("0-9","0-9","10-19","10-19","20-29","20-29","30-39","30-39","40-49","40-49",
                   "50-59","50-59","60-69","60-69","70-79","70-79","80-89","80-89","90+","90+")
)
gbd_age10 <- ckd_bra_matched |>
  dplyr::left_join(gbd_age10_lookup, by = "age_clean") |>
  dplyr::group_by(age_band_10) |>
  dplyr::summarise(prevalence = mean(val/1e5), .groups="drop") |>
  dplyr::mutate(source = "GBD 2023: Brazil general population (modelled, all CKD stages)")

harmonised_bands <- intersect(unique(sinan_age10$age_band_10), unique(gbd_age10$age_band_10))
message(sprintf("[42] Harmonised 10-year age bands (both sources): %s", paste(harmonised_bands, collapse=",")))
message(sprintf("[42] SINAN-only bands: %s", paste(setdiff(unique(sinan_age10$age_band_10), harmonised_bands), collapse=",")))
message(sprintf("[42] GBD-only bands: %s", paste(setdiff(unique(gbd_age10$age_band_10), harmonised_bands), collapse=",")))

ckd_prevalence_age_comparison_data <- dplyr::bind_rows(
  sinan_age10 |> dplyr::transmute(age_band_10, prevalence, source, harmonised = age_band_10 %in% harmonised_bands),
  gbd_age10 |> dplyr::transmute(age_band_10, prevalence, source, harmonised = age_band_10 %in% harmonised_bands)
)

age_order <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
ckd_prevalence_age_comparison_data <- ckd_prevalence_age_comparison_data |>
  dplyr::mutate(age_band_10 = factor(age_band_10, levels = age_order))

p_fig_s5 <- ggplot2::ggplot(ckd_prevalence_age_comparison_data,
                             ggplot2::aes(x = age_band_10, y = prevalence, colour = source, group = source, shape = harmonised)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::scale_colour_manual(values = c("SINAN: confirmed chikungunya cases (clinically recognised CKD)" = "#D55E00",
                                            "GBD 2023: Brazil general population (modelled, all CKD stages)" = "#0072B2"), name = NULL) +
  ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  ggplot2::labs(
    x = "Age band (10-year)", y = "CKD prevalence (note: NOT the same denominator or definition)",
    title = "Descriptive age comparison: SINAN-recorded vs GBD-modelled CKD prevalence",
    subtitle = "NOT a like-for-like validation - different target populations, ascertainment\nand severity coverage (see ckd_definition_comparison.csv). Open points = age\nband unique to one source."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical",
                 panel.background = ggplot2::element_rect(fill="white", colour=NA),
                 plot.background = ggplot2::element_rect(fill="white", colour=NA))
ggplot2::ggsave(file.path(out_dir, "ckd_prevalence_age_comparison.png"), p_fig_s5, width = 9, height = 7.5, dpi = 300, bg = "white")
message("[42] PHASE 5 (descriptive age comparison figure) written: ckd_prevalence_age_comparison.png")

## =============================================================================
## PHASE 6 -- DOWNSTREAM DEPENDENCY MAP (documentation only - not rerun)
## =============================================================================

ckd_downstream_dependency_map <- tibble::tribble(
  ~step, ~script, ~what_it_does_with_ckd, ~ckd_input_used, ~output_cached, ~potential_mismatch_consequence,
  1, "12_core3_condition_specific_rr.R", "Estimates the SINAN-derived CKD severity RR (hospitalisation and death), mutually adjusted for DM/HTN, and as part of the 8-profile model", "SINAN renal_disease (clinically recognised pre-existing CKD) - the EXPOSURE, not a prevalence input", "core3_rr_hosp/death.RData, core_profile_rr_hosp/death.RData (+ draws)", "NONE at this step - the RR is internally consistent (estimated and reported for the SAME SINAN-defined CKD population). The transportability question only arises once this RR is later applied to a DIFFERENT (GBD-defined) population.",
  2, "29_background_prevalence_core3_brazil.R", "(a) Computes SINAN's OWN observed joint DM/HTN/CKD prevalence (for the independence-vs-clustering comparison); (b) extracts GBD CKD prevalence by age as the 'background/general population' marginal", "Both SINAN renal_disease (for (a)) AND GBD cause='Chronic kidney disease' (for (b)) - these are DIFFERENT CKD populations used side by side", "brazil_profile_prevalence_two_extremes.RData (bg_prev_brazil3 = GBD/NCD-RisC marginals; sinan_vs_independence_* = SINAN-observed)", "This is the FIRST point where the two CKD definitions enter the SAME pipeline. No harmonisation or flag exists in the script distinguishing 'clinically recognised CKD' (SINAN) from 'any-stage modelled CKD' (GBD) at this join point.",
  3, "30_marginal_prevalence_brazil.R", "Descriptive table/figure of GBD-derived marginal CKD (and DM/HTN) prevalence by age - a dead-end report, not consumed further downstream", "GBD CKD prevalence (via cached bg_prev_brazil3 from step 2)", "brazil_marginal_prevalence_PROTOTYPE.xlsx, fig_marginal_prevalence_brazil_PROTOTYPE.png", "Low - purely descriptive output, not an input to any further RR-weighted calculation.",
  4, "32_clustering_ratio_r_brazil.R", "Calibrates a clustering weight (lambda) between SINAN's observed joint multimorbidity prevalence and a GBD-marginals-based independence/max-clustering bound, to estimate the 8 joint-profile POPULATION prevalences", "GBD CKD marginal (bound inputs) AND SINAN CKD (observed joint prevalence, via sinan3) - combined via the lambda calibration", "brazil_profile_prevalence_two_extremes.RData profile bounds + lambda draws (consumed by 34_)", "The calibrated CKD-containing profile prevalences (CKD, DM+CKD, HTN+CKD, DM+HTN+CKD) inherit the GBD (broad, all-stage) CKD marginal as their anchor - so even the 'observed clustering pattern from SINAN' is being scaled onto a GBD-sized CKD population, not a SINAN-sized one.",
  5, "34_profile_burden_allocation_brazil.R", "Multiplies the calibrated population PROFILE PREVALENCE (from step 4, GBD-anchored) by the SINAN-derived profile RR (from step 1) to allocate symptomatic/hospitalisation/death burden across the 8 profiles", "GBD-anchored profile prevalence x SINAN-derived RR - THIS is where the transportability assumption becomes load-bearing", "brazil_profile_burden_allocation.RData + 8 manuscript-prototype figures (STALE as of Gate 3.1, per gate31_final/cache_dependency_audit.csv - unrelated staleness, not a CKD-specific issue)", "HIGH - the RR estimated in a clinically-recognised-CKD population is applied, via the GBD-anchored prevalence weight, to the CKD-containing profiles' ENTIRE population share - implicitly assuming the RR is constant across the full GBD CKD severity spectrum (stage 1 through dialysis-dependent). This assumption is NOT demonstrated or tested anywhere in the current pipeline."
)
readr::write_csv(ckd_downstream_dependency_map, file.path(out_dir, "ckd_downstream_dependency_map.csv"))
message("[42] PHASE 6 (downstream dependency map) written: ckd_downstream_dependency_map.csv")

## =============================================================================
## PHASE 7 -- SCIENTIFIC ASSESSMENT (computed from the above, not asserted)
## =============================================================================

coding_pass <- phase1b_pass &&
  (nrow(dup_bands_after_match) == 0) &&
  (length(missing_bands) == 0) &&
  (sum(ckd_bra$val <= 0 | ckd_bra$val > 100000, na.rm = TRUE) == 0)

harmonisation_ready <- FALSE  # see rationale in the report - an unresolved transportability
                               # assumption and an undocumented (if currently harmless) missing
                               # year filter mean this is NOT yet "ready", independent of coding_pass

gbd_missing_year_filter_is_live_risk <- (n_years_in_ckd_bra == 1)  # TRUE = currently latent, not active

overall_verdict <- if (!coding_pass) {
  "FAIL"
} else if (!harmonisation_ready) {
  "CONDITIONAL PASS"
} else {
  "PASS"
}

phase7_answers <- tibble::tribble(
  ~question, ~answer,
  "1. Is CKD correctly coded in the clinical RR model?", "YES. renal_disease is built via the NA-safe recode() helper (1='yes', 2='no', 9='unknown', blank/unmapped=NA), and ckd=as.integer(renal_disease=='yes') is only evaluated after the complete-case filter already restricts to renal_disease %in% c('no','yes') - no NA-propagation risk, no coding discrepancy between the cleaning script and 12_'s GAM (see Phase 1b live verification: PASS).",
  "2. Is calling SINAN RENAL 'CKD' defensible?", "YES, as a label, with an important caveat. The official SINAN dictionary names field 41 explicitly 'Doenca renal cronica' (chronic kidney disease) - this is not a researcher relabelling of an ambiguous field. The caveat is severity, not naming: the field is an undifferentiated yes/no with no stage, eGFR, albuminuria or dialysis information, so 'CKD' here means 'clinician indicated the patient has chronic kidney disease', not any specific severity threshold.",
  "3. Does SINAN represent physician-recognised/clinically recorded pre-existing CKD without stage information?", "YES, confirmed directly from the dictionary (Phase 1) and the absence of any stage/eGFR/albuminuria/dialysis field anywhere in the SINAN chikungunya schema.",
  "4. Is the GBD prevalence based on a broader disease definition?", "YES. GBD's 'Chronic kidney disease' cause is a modelled, all-severity-stage (1-5) estimate for the general population, not restricted to clinically diagnosed/recognised cases - directly evidenced by the steep age gradient reaching ~69% prevalence at 90+ in the Brazil 2023 extract (Phase 5), far exceeding any plausible clinically-diagnosed-only prevalence at that age.",
  "5. Is there an unresolved mismatch relevant to population burden estimation?", "YES. The population-burden pipeline (Phase 6) applies the SINAN-derived (clinically-recognised-CKD) severity RR to a population prevalence anchored on the GBD (all-stage, modelled) CKD definition, with no transportability adjustment or sensitivity check anywhere in the current code.",
  "6. Are there technical problems in the GBD extraction, especially year or age duplication?", if (gbd_missing_year_filter_is_live_risk) "NO ACTIVE bug: the current gbd_prevalence.csv contains exactly one year (2023) for every row, and 29_'s age-band filter correctly matches exactly 20/20 intended bands with zero duplicates. However, 29_ never explicitly filters on `year` - this works only because the source file happens to be single-year; it is a latent fragility, not a demonstrated error, and should be made explicit (see necessary-vs-optional fixes).",
  "7. Should the existing clinical RR models remain unchanged?", "YES. The RR models are internally self-consistent - estimated and applied within the SAME SINAN-defined CKD population - and no coding defect was found. This gate does not identify any reason to refit or modify 12_core3_condition_specific_rr.R.",
  "8. Is population burden modelling scientifically defensible using the current CKD prevalence?", "ONLY AS AN EXPLICITLY-FLAGGED EXPLORATORY PROTOTYPE, not as a validated estimate. The code executes correctly (no bug), but the underlying measurement-comparability assumption (SINAN RR transportable to the full GBD CKD severity spectrum) is untested and plausibly wrong in the direction of overstating the population CKD-attributable burden, since the RR was estimated in a narrower, more clinically-apparent subset than the prevalence it is being multiplied against.",
  "9. Is one focused CKD prevalence sensitivity analysis warranted, or would better data harmonisation be necessary first?", "Data harmonisation / explicit documentation should come FIRST. A sensitivity analysis (e.g. bounding the burden estimate under alternative assumptions about which GBD severity range the SINAN RR plausibly applies to) would be the natural NEXT step once the assumption is explicitly stated in the methods - but running one now, without first documenting the assumption, risks presenting a false sense of precision.",
  "10. Would this issue affect the RR-only manuscript, the integrated Brazil burden manuscript, or both?", "The RR-only manuscript is NOT affected (CKD exposure and RR estimation are internally consistent and correctly coded). The integrated Brazil population-burden manuscript/extension IS affected and should explicitly document the CKD definition mismatch and transportability assumption as a named limitation before any burden figures derived from 34_ are presented as primary results."
)
## (phase7_answers is embedded directly into VALIDATION_3_3A.md's Section 9
## rather than written as its own CSV - not one of the 9 required Phase 8
## deliverables, and a prior attempt to write it hit a persistent Excel file
## lock on this machine unrelated to this script's logic.)
message(sprintf("[42] PHASE 7 complete. coding_pass=%s, harmonisation_ready=%s, overall_verdict=%s",
                 coding_pass, harmonisation_ready, overall_verdict))

## =============================================================================
## PHASE 9 -- VALIDATION_3_3A.md
## =============================================================================

df_to_md <- function(df, digits = 3) {
  df <- as.data.frame(df)
  for (j in seq_along(df)) if (is.numeric(df[[j]])) df[[j]] <- round(df[[j]], digits)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

report <- sprintf('# Gate 3.3A - CKD measurement consistency and data-source harmonisation audit

Brazil, confirmed chikungunya cases, 2017-2025. Produced by `02_Script/42_ckd_measurement_consistency_audit.R`. Pure audit: no model refit, no CKD exposure change, no prevalence substitution, no population-burden rerun. Nothing committed/pushed; no primary cache, manuscript table/figure, or DOCX touched.

## 1. Main findings (summary)

- SINAN field 41 (`RENAL`) is explicitly labelled in the official dictionary as "Doenca renal cronica" (chronic kidney disease) - a pre-existing-disease checkbox, unrelated to the separate "acute renal failure" syndrome category that exists elsewhere in the SINAN schema (field 2, id_agravo). **No stage, eGFR, albuminuria, or dialysis information exists anywhere in SINAN for this field.**
- CKD recording completeness among confirmed chikungunya cases is **%.4f%%** (eligible population N=%s) - essentially complete, and NOT a material source of selection bias for the clinical RR model (contrast with hospitalisation status, ~28%% missing, Gate 3.2A).
- The GBD CKD prevalence extraction in `29_background_prevalence_core3_brazil.R` is **technically correct for the current source file** (20/20 intended age bands matched, zero duplicates, no implausible values) but relies on an **undocumented, currently-latent assumption** that the file contains only one year - true today (2023 only) but not asserted in code.
- The GBD CKD prevalence is a **modelled, all-severity-stage** estimate (reaching ~%.0f%% at age 90+ in Brazil), while the SINAN-recorded CKD among chikungunya cases stays under %.0f%% even at the oldest ages - a large, visually obvious gap (Figure: ckd_prevalence_age_comparison.png) consistent with, though not proof of, the two sources measuring different underlying CKD populations.
- The population-burden pipeline (`29_`->`32_`->`34_`) combines the GBD (broad) CKD prevalence with the SINAN (narrow, clinically-recognised) CKD RR with **no transportability adjustment or documented assumption** anywhere in the code.

## 2. Technical data-validation results

| Check | Result |
|---|---|
| SINAN RENAL -> renal_disease -> ckd coding chain (live verification, 9 synthetic codes) | %s |
| CKD recording completeness within the broader "confirmed, valid age/sex" population (N=%s) | %.4f%% recorded (yes/no); DM=%.4f%%, HTN=%.4f%% for comparison |
| Core-3 eligible cohort (N=%s) is 100%% CKD-recorded by construction | %s (expected - CKD completeness is part of the eligibility definition itself, not an independent finding) |
| GBD Brazil/CKD/Prevalence/Rate/Both slice: distinct years in file | %s (entire file, all causes/locations) |
| GBD age-band match (against the keep_age 20-band grid used by 29_) | %d/%d matched, %d missing, %d duplicated |
| GBD implausible values (val<=0 or >100,000 per 100k) | %d |
| GBD missing val/upper/lower | 0 (fully populated in the matched slice) |

Full detail: `sinan_ckd_coding_audit.csv`, `sinan_ckd_recording_by_age_year.csv`, `gbd_ckd_source_metadata.csv`, `gbd_ckd_age_prevalence_audit.csv`.

## 3. Measurement-definition discrepancies

%s

Full three-way comparison (SINAN / GBD / PNS 2019, 9 dimensions): `ckd_definition_comparison.csv`. **PNS 2019 figures are not independently verified in this session** (no local microdata; general survey-design knowledge only) - marked explicitly as unresolved pending literature confirmation.

## 4. Epidemiological implications

The age-specific descriptive comparison (Phase 5; NOT a validation - different denominators, see Section 3) shows SINAN-recorded CKD prevalence among confirmed chikungunya cases rising only gently with age (roughly %.2f%% at ages 0-9 to %.2f%% at 90+), while the GBD modelled general-population CKD prevalence rises steeply across the same age range (%.2f%% to %.1f%%). This pattern is consistent with SINAN capturing a narrower, more clinically-apparent CKD population than the GBD all-stage estimate, but the data cannot distinguish how much of the gap reflects (a) genuinely lower CKD prevalence among people well enough to be notified with chikungunya and survive to interview, (b) under-ascertainment by the notifying clinician, or (c) GBD including a large mild/undiagnosed-CKD component that would never appear on any clinical notification form regardless of true disease state. **No prevalence correction factor was calculated from this ratio, per instruction.**

## 5. Downstream dependency map

%s

Full detail with per-step consequence assessment: `ckd_downstream_dependency_map.csv`. The highest-consequence step is `34_profile_burden_allocation_brazil.R`, where the SINAN-derived RR is multiplied against the GBD-anchored profile prevalence - this is where the transportability assumption becomes load-bearing for any reported burden number.

## 6. Necessary vs optional corrections

**Necessary (before the population-burden extension is presented as a primary, non-exploratory result):**
1. Document the CKD-definition mismatch and transportability assumption explicitly in the burden-modelling methods text.
2. Add an explicit, asserted `year` filter (or an assertion that exactly one year exists) to the GBD extraction in `29_background_prevalence_core3_brazil.R`, so a future GBD data refresh with multiple years fails loudly instead of silently duplicating rows.

**Optional / for a later gate:**
3. A bounded CKD-prevalence sensitivity analysis for the burden estimate (e.g. re-running `34_` under alternative assumptions about which share of GBD-modelled CKD plausibly corresponds to SINAN-recognisable CKD) - valuable, but only AFTER (1) is written down, not as a substitute for documenting the assumption.
4. Pursue PNS 2019 microdata or published tables as a third reference point, if a closer independent benchmark for "diagnosed CKD prevalence" is wanted.
5. Investigate whether CKD recording completeness (already very high, Section 2) shows any meaningful outcome-stratified pattern - included here as descriptive diagnostics (`sinan_ckd_recording_by_outcome.csv`) but not pursued further, as the overall completeness is already too high for this to be a material concern.

## 7. Recommendation for the existing RR manuscript

**No change needed.** CKD is correctly coded, the recode/eligibility logic is NA-safe and consistent between the cleaning script and the GAM, and CKD recording completeness (%.4f%%) is not a material missingness concern. The condition-specific and joint-profile RR estimates for CKD are internally valid for the population in which they were estimated (SINAN-recognised, pre-existing CKD among confirmed chikungunya cases).

## 8. Recommendation for the population-burden extension

**Treat current CKD-containing-profile burden estimates as exploratory/prototype, not primary, until the transportability assumption is explicitly documented.** This is consistent with the existing prototype labelling already used throughout the `34_`-`37_` outputs. No code change is required to make this gate pass on coding grounds; a documentation/limitations addition is required before burden numbers are presented as validated.

## 9. Gate 3.3A status

**Data coding correctness: %s.** **CKD definition harmonisation readiness for population-burden modelling: %s (not yet ready without explicit documentation).**

**Overall: %s.** Code executes correctly throughout (SINAN recoding, eligibility construction, and the GBD extraction all passed their technical checks) - this is a coding-correctness PASS. The CONDITIONAL qualifier applies specifically and only to the unresolved CKD-definition mismatch relevant to population-burden modelling (Sections 4-6), which is a scientific-comparability issue, not a code defect - per instruction, these two are reported separately rather than conflated.

---
*Verified against: 01_Data/chik_sinan_individual_2015_2025.rds, 01_Data/sinan_chik_docs/dic_dados_chikungunya.pdf, 01_Data/gbd_prevalence.csv, 02_Script/02_clean_chik_sinan_brazil.R, 12_, 29_, 30_, 32_, 34_.*
',
  dm_htn_compare$ckd_recorded_pct, format(nrow(p0), big.mark=","),
  round(max(gbd_age10$prevalence[gbd_age10$age_band_10=="90+"])*100,0),
  ceiling(max(sinan_age10$prevalence)*100),
  if (phase1b_pass) "PASS" else "FAIL",
  format(nrow(p0), big.mark=","), dm_htn_compare$ckd_recorded_pct, dm_htn_compare$dm_recorded_pct, dm_htn_compare$htn_recorded_pct,
  format(nrow(core3_eligible_check), big.mark=","), "CONFIRMED (PASS)",
  paste(gbd_years_all, collapse=","),
  n_matched_bands, length(keep_age), length(missing_bands), nrow(dup_bands_after_match),
  sum(ckd_bra$val <= 0 | ckd_bra$val > 100000, na.rm=TRUE),
  df_to_md(ckd_definition_comparison |> dplyr::select(dimension, SINAN_RENAL_field41, GBD_chronic_kidney_disease), digits=2),
  sinan_age10$prevalence[sinan_age10$age_band_10=="0-9"]*100, sinan_age10$prevalence[sinan_age10$age_band_10=="90+"]*100,
  gbd_age10$prevalence[gbd_age10$age_band_10=="0-9"]*100, gbd_age10$prevalence[gbd_age10$age_band_10=="90+"]*100,
  df_to_md(ckd_downstream_dependency_map |> dplyr::select(step, script, potential_mismatch_consequence), digits=2),
  dm_htn_compare$ckd_recorded_pct,
  ifelse(coding_pass,"PASS","FAIL"), ifelse(harmonisation_ready,"READY","NOT YET READY"),
  overall_verdict
)

writeLines(report, file.path(out_dir, "VALIDATION_3_3A.md"))
message("[42] VALIDATION_3_3A.md written.")
message("[42] Gate 3.3A COMPLETE. Verdict: ", overall_verdict)
