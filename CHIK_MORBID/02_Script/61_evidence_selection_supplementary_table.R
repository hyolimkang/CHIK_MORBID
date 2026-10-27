## -----------------------------------------------------------------------------
## 61_evidence_selection_supplementary_table.R
##
## Supplementary table: evidence-informed prioritisation of the seven
## SINAN-recorded underlying conditions (DM, HTN, CKD = core).
##
## Inputs
##   - Completed evidence extraction workbook (outside the repository):
##       C:/Users/user/OneDrive/CHIK_MORBID/chik_morbid_evidence_extraction_completed.xlsx
##     A dated snapshot plus MD5 is copied to <out_dir>/source_snapshot/.
##   - Historical classifications: 02_Script/16_figure_evidence_matrix.R
##     (copied verbatim below; NOT modified).
##
## Rules
##   - Study-level facts are transcribed from the workbook "Effect measure /
##     estimate" field. Every numeric string is checked verbatim against the
##     workbook (stop() on mismatch). No estimate, CI or screening detail is
##     added.
##   - Hospitalisation and death are kept separate. Severe/composite, ICU-only
##     and fatal-only records are audited but never counted as mortality.
##   - Historical classifications are displayed unchanged. A reviewer re-check
##     against the workbook decision rules is reported alongside, flagged, and
##     never substituted.
##   - The current manuscript's RRs are not used.
##
## Outputs: 03_Output/tables/evidence_selection_final/
## -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(openxlsx)
  library(flextable)
  library(officer)
})

wb_path <- "C:/Users/user/OneDrive/CHIK_MORBID/chik_morbid_evidence_extraction_completed.xlsx"
out_dir <- file.path("03_Output", "tables", "evidence_selection_final")
dir.create(file.path(out_dir, "source_snapshot"), recursive = TRUE, showWarnings = FALSE)
font_family <- "Arial"

## ---- 1. Source workbook and snapshot -----------------------------------------
stopifnot(file.exists(wb_path))
snap_path <- file.path(out_dir, "source_snapshot", basename(wb_path))
if (!file.exists(snap_path)) file.copy(wb_path, snap_path)
md5_src  <- unname(tools::md5sum(wb_path))
md5_snap <- unname(tools::md5sum(snap_path))
if (md5_src != md5_snap) stop("Snapshot differs from source workbook; inspect before overwriting.")
writeLines(c(
  paste0("source_path: ", wb_path),
  paste0("source_mtime: ", format(file.mtime(wb_path), "%Y-%m-%d %H:%M:%S")),
  paste0("md5: ", md5_src),
  paste0("snapshot_taken: ", format(Sys.time(), "%Y-%m-%d"))
), file.path(out_dir, "source_snapshot", "SOURCE_MD5.txt"))

ext <- read.xlsx(wb_path, sheet = "Evidence extraction", startRow = 2)
ext <- ext[!is.na(suppressWarnings(as.integer(ext$No))), ]
ext$No <- as.integer(ext$No)
effect_col <- grep("^Effect", names(ext), value = TRUE)[1]
effect_of <- function(no) ext[[effect_col]][ext$No == no]

## ---- 2. Historical classifications (verbatim from script 16) -----------------
conditions <- c("Diabetes mellitus", "Hypertension", "Chronic kidney disease",
                "Hepatopathy", "Autoimmune disease", "Haematological disease",
                "Peptic ulcer disease")
cond_key <- c("DM", "HTN", "CKD", "LIV", "AUT", "HAE", "PEP")
historical <- data.frame(
  key = cond_key, condition = conditions,
  hist_hosp = c("Supported", "Supported", "Supported", "Insufficient",
                "Limited", "Insufficient", "Insufficient"),
  hist_severe_fatal = c("Supported", "Limited", "Supported", "Limited",
                        "Insufficient", "Insufficient", "Insufficient"),
  hist_role = c("Core", "Core", "Core", "Secondary", "Secondary",
                "Not prioritised", "Not prioritised"),
  stringsAsFactors = FALSE)

## ---- 3. Study registry (from the workbook; families per its overlap sheet) ---
studies <- data.frame(
  no = c(2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23),
  study = c("Añazco 2026", "Furtado 2026", "Cerqueira-Silva 2025", "Micheleto 2025",
            "100 Million Brazilian Cohort (study 7)", "Costa 2023", "Oliveira 2022",
            "de Lima 2021", "Cerbino-Neto 2020", "Pinto 2019", "Silva Junior 2019",
            "Silva Junior 2018", "de Almeida Barreto 2018", "Mercado 2018",
            "Badawi 2018", "Crosby 2016", "Perti 2016", "Economopoulou 2009",
            "Ecological T2D study (study 23)"),
  family = c("IND-PY", "BRA-A", "BRA-A", "SYN", "BRA-D", "SYN", "BRA-C",
             "BRA-E", "SYN", "BRA-B", "BRA-B", "BRA-B", "SYN", "CTX",
             "SYN", "CTX", "IND-PR", "IND-RE", "CTX"),
  setting = c("Paraguay, ICU, n = 34", "Brazil, national SINAN 2017–2023",
              "Brazil, national SINAN 2015–2024", "Meta-analysis (7 studies)",
              "Brazil, linked cohort 2015–2018", "Scoping review (CKD)",
              "Fortaleza 2017, case-control", "Ceará, fatal cases only",
              "Systematic review of fatal cases", "Ceará 2016–2017 surveillance",
              "Ceará 2016–2017 surveillance", "Ceará 2016–2017 surveillance",
              "Narrative review (DM)", "Fatal-case series", "Meta-analysis (severe composite)",
              "ICU case series", "US veterans / Puerto Rico", "Réunion 2005–2006 outbreak",
              "Ecological, Ceará"),
  stringsAsFactors = FALSE)
family_type <- function(f) ifelse(f %in% c("BRA-A", "BRA-B", "BRA-C"), "Overlapping with target SINAN data",
                           ifelse(grepl("^IND", f), "External (non-overlapping)",
                           ifelse(f == "SYN", "Evidence synthesis", "Contextual only")))

## ---- 4. Condition × study × outcome audit -----------------------------------
## type: ADJ = adjusted estimate; ADJ_DIR = adjusted model, direction reported,
## no numeric estimate extracted; CRUDE = crude estimate; CRUDE_NOEST = crude
## comparison without extractable estimate; POOLED = pooled synthesis estimate;
## NARR = synthesis without pooled estimate; DESC = descriptive only;
## NOT_EXTRACTED = condition assessed but no condition-specific result extracted;
## NOT_REPORTED = not reported among the study's associated factors;
## CONTEXT = contextual record, not condition-level prognostic evidence.
## verify = substring that must appear verbatim in the workbook effect field.
a <- function(key, no, outcome, type, direction, estimate, verify = NA_character_, note = "")
  data.frame(key, no, outcome, type, direction, estimate, verify, note, stringsAsFactors = FALSE)

audit <- rbind(
  ## Diabetes
  a("DM", 3,  "Hospitalisation", "ADJ", "Positive", "aOR 1.488 (1.385–1.599)", "DM 1.488 (1.385–1.599)", "Stepwise logistic regression"),
  a("DM", 3,  "Death", "ADJ", "Positive", "aOR 2.396 (1.974–2.908)", "DM 2.396 (1.974–2.908)", "Death analysis restricted to sample with hospitalisation data"),
  a("DM", 4,  "Hospitalisation", "ADJ_DIR", "Positive", "Associated (adjusted logistic model); no numeric estimate extracted", "DM, autoimmune disease, HTN and kidney disease associated with hospitalisation", "Missing comorbidity treated as absent"),
  a("DM", 4,  "Death", "ADJ_DIR", "Positive", "Associated with CHIK-attributed in-hospital death (Fine–Gray); no numeric estimate extracted", "DM, kidney disease and liver disease were associated with chikungunya-attributed death", "Conditional on hospitalisation"),
  a("DM", 13, "Hospitalisation", "ADJ", "Positive", "aOR 1.70 (1.44–1.99)", "DM 1.70 (1.44–1.99)", "Large missingness in hospitalisation status"),
  a("DM", 14, "Death", "ADJ", "Positive", "aOR 3.29 (p<0.0001); CI not recorded in workbook", "DM 3.29", ""),
  a("DM", 9,  "Death", "CRUDE", "Imprecise", "Crude OR 1.8 (0.9–3.1)", "DM crude OR 1.8 (0.9–3.1)", "82 deaths; partial overlap with SINAN"),
  a("DM", 20, "Hospitalisation", "ADJ", "Positive", "Age-adjusted RR 1.39 (1.06–1.84)", "DM 1.39 (1.06–1.84)", "Age adjustment only"),
  a("DM", 2,  "Death", "CRUDE", "Imprecise", "Crude OR 0.70 (0.11–3.72)", "diabetes OR 0.70 (0.11–3.72)", "ICU-selected cohort (n = 34); not risk among all cases"),
  a("DM", 22, "Severe/composite", "ADJ_DIR", "Null", "Not associated with severe disease; no estimate extracted", "Diabetes was not associated with severe disease or death", "Not mortality"),
  a("DM", 22, "Death", "ADJ_DIR", "Null", "Not associated with death; no estimate extracted", "Diabetes was not associated with severe disease or death", "Mortality predictors were age ≥85 and alcohol abuse"),
  a("DM", 6,  "Death", "POOLED", "Positive", "Pooled OR 2.86 (1.75–4.69)", "DM OR 2.86 (95% CI 1.75–4.69)", "Includes overlapping Brazilian primary studies"),
  a("DM", 18, "Severe/composite", "POOLED", "Positive", "Pooled OR 1.20 (1.05–1.48)", "OR 1.20 (95% CI 1.05–1.48)", "Heterogeneous severe-outcome definitions; not mortality"),
  a("DM", 16, "Hospitalisation", "NARR", "Positive", "Narrative: more frequent hospitalisation; no pooled estimate", "more frequently required hospitalisation", ""),
  a("DM", 11, "Descriptive (fatal cases)", "DESC", "Not applicable", "DM in 5.6% of 2,140 decedents", "diabetes (5.6%)", "No comparator"),
  a("DM", 10, "Contextual (fatal cases only)", "CONTEXT", "Not applicable", "aOR 7.7 compares subacute vs acute death among fatalities; not a mortality OR", "adjusted OR 7.7", "No survivor comparator"),
  a("DM", 7,  "Contextual", "CONTEXT", "Not applicable", "Diabetes is a cause-of-death outcome of CHIKV infection, not a baseline exposure", NA, ""),
  a("DM", 23, "Contextual", "CONTEXT", "Not applicable", "Ecological; no individual-level estimate", NA, ""),
  ## Hypertension
  a("HTN", 3,  "Hospitalisation", "ADJ", "Positive", "aOR 1.519 (1.438–1.604)", "HTN 1.519 (1.438–1.604)", "Stepwise logistic regression"),
  a("HTN", 3,  "Death", "ADJ", "Positive", "aOR 2.053 (1.723–2.446)", "HTN 2.053 (1.723–2.446)", ""),
  a("HTN", 4,  "Hospitalisation", "ADJ_DIR", "Positive", "Associated (adjusted logistic model); no numeric estimate extracted", "HTN and kidney disease associated with hospitalisation", ""),
  a("HTN", 4,  "Death", "NOT_REPORTED", "Not reported", "Not listed among factors associated with CHIK-attributed death", NA, "Conditional on hospitalisation"),
  a("HTN", 13, "Hospitalisation", "ADJ", "Positive", "aOR 1.90 (1.69–2.14)", "HTN 1.90 (1.69–2.14)", ""),
  a("HTN", 14, "Death", "ADJ", "Positive", "aOR 3.74 (p<0.0001); CI not recorded in workbook", "HTN 3.74", ""),
  a("HTN", 9,  "Death", "CRUDE", "Positive", "Crude OR 2.2 (1.2–4.1)", "HTN crude OR 2.2 (1.2–4.1)", "Crude only"),
  a("HTN", 20, "Hospitalisation", "ADJ", "Null", "Age-adjusted RR 1.00 (0.63–1.57)", "HTN 1.00 (0.63–1.57)", "Null after age adjustment"),
  a("HTN", 2,  "Death", "CRUDE", "Imprecise", "Crude OR 1.17 (0.28–4.84)", "hypertension OR 1.17 (95% CI 0.28–4.84)", "ICU-selected cohort (n = 34)"),
  a("HTN", 22, "Severe/composite", "ADJ", "Positive", "aRR 1.7 (1.2–2.4) for severe atypical disease", "hypertension 1.7 (95% CI 1.2–2.4)", "Not mortality"),
  a("HTN", 22, "Death", "NOT_REPORTED", "Not reported", "Not among reported mortality predictors (age ≥85, alcohol abuse)", "Mortality predictors were age ≥85 and alcohol abuse", ""),
  a("HTN", 6,  "Death", "POOLED", "Positive", "Pooled OR 3.10 (2.02–4.77)", "HTN OR 3.10 (2.02–4.77)", "Includes overlapping Brazilian primary studies"),
  a("HTN", 18, "Severe/composite", "POOLED", "Null", "Not shown to have a significant pooled OR", "Hypertension/cardiac disease were prevalent but not shown to have significant", "Not mortality"),
  a("HTN", 11, "Descriptive (fatal cases)", "DESC", "Not applicable", "HTN in 6.9% of 2,140 decedents", "hypertension (6.9%)", "No comparator"),
  a("HTN", 10, "Contextual (fatal cases only)", "CONTEXT", "Not applicable", "aOR 4.1 (0.62–26.9) for subacute vs acute death among fatalities", "HTN aOR 4.1 (0.62–26.9)", "No survivor comparator"),
  a("HTN", 17, "Contextual (fatal cases only)", "CONTEXT", "Not applicable", "Underlying disease (mainly HTN) in 4/13 fatal cases", "4/13 had underlying disease, mainly hypertension", "No comparator"),
  ## Chronic kidney disease
  a("CKD", 3,  "Hospitalisation", "ADJ", "Positive", "aOR 1.982 (1.682–2.335)", "CKD 1.982 (1.682–2.335)", ""),
  a("CKD", 3,  "Death", "ADJ", "Positive", "aOR 4.437 (3.171–6.209)", "CKD 4.437 (3.171–6.209)", ""),
  a("CKD", 4,  "Hospitalisation", "ADJ_DIR", "Positive", "Kidney disease associated (adjusted logistic model); no numeric estimate extracted", "kidney disease associated with hospitalisation", "Exposure labelled 'kidney disease'"),
  a("CKD", 4,  "Death", "ADJ_DIR", "Positive", "Kidney disease associated with CHIK-attributed in-hospital death (Fine–Gray); no numeric estimate extracted", "kidney disease and liver disease were associated", "Conditional on hospitalisation"),
  a("CKD", 13, "Hospitalisation", "ADJ", "Positive", "aOR 4.56 (3.36–6.17)", "CKD 4.56 (95% CI 3.36–6.17)", ""),
  a("CKD", 14, "Death", "ADJ", "Positive", "aOR 3.14 (p<0.0001); CI not recorded in workbook", "CKD 3.14", ""),
  a("CKD", 15, "Hospitalisation", "CRUDE_NOEST", "Positive", "Admission more frequent with CKD; no estimate (multivariable model had CKD as outcome)", "hospital admission strongly more frequent in CKD", "OR 5.60 is not a CKD→hospitalisation estimate"),
  a("CKD", 15, "Death", "CRUDE_NOEST", "Positive", "Death 3.0% vs 0.2% (p<0.0001)", "death 3.0% vs 0.2%", "Crude only"),
  a("CKD", 9,  "Death", "ADJ", "Positive", "aOR 12.77 (2.75–59.4)", "CKD adjusted OR 12.77 (95% CI 2.75–59.4)", "Wide CI; 82 deaths"),
  a("CKD", 20, "Hospitalisation", "ADJ", "Positive", "Age-adjusted RR 1.52 (1.19–1.94)", "CKD 1.52 (95% CI 1.19–1.94)", "Age adjustment only"),
  a("CKD", 2,  "Descriptive (ICU-selected)", "DESC", "Not applicable", "CKD in 3/34; no CKD-specific mortality estimate", "CKD reported in 3/34", "No estimate"),
  a("CKD", 6,  "Death", "POOLED", "Positive", "Pooled OR 5.81 (1.30–25.99)", "CKD OR 5.81 (1.30–25.99)", "Wide CI; includes overlapping Brazilian studies"),
  a("CKD", 8,  "Severe/fatal (narrative)", "NARR", "Positive", "Scoping review; no pooled estimate", "No single pooled prognostic estimate", "Pre-existing CKD vs acute kidney injury not always separated"),
  a("CKD", 11, "Descriptive (fatal cases)", "DESC", "Not applicable", "CKD reported among decedents; no comparator", "CKD and liver disease were also reported", ""),
  a("CKD", 17, "Contextual (fatal cases only)", "CONTEXT", "Not applicable", "Acquired renal injury in fatal cases; pre-existing CKD not assessed", NA, ""),
  a("CKD", 19, "Contextual (ICU-selected)", "CONTEXT", "Not applicable", "Chronic renal failure described in ICU series; no estimate", "No condition-specific prognostic RR/OR", ""),
  ## Hepatopathy
  a("LIV", 3,  "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no hospitalisation estimate extracted", NA, ""),
  a("LIV", 3,  "Death", "ADJ", "Positive", "aOR 1.781 (1.062–2.987)", "liver 1.781 (1.062–2.987)", ""),
  a("LIV", 4,  "Hospitalisation", "NOT_REPORTED", "Not reported", "Not listed among factors associated with hospitalisation", NA, ""),
  a("LIV", 4,  "Death", "ADJ_DIR", "Positive", "Associated with CHIK-attributed in-hospital death (Fine–Gray); no numeric estimate extracted", "liver disease were associated with chikungunya-attributed death", "Conditional on hospitalisation"),
  a("LIV", 13, "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("LIV", 15, "Descriptive", "DESC", "Not applicable", "Described as a comorbidity in the CKD cohort only", NA, ""),
  a("LIV", 11, "Descriptive (fatal cases)", "DESC", "Not applicable", "Liver disease reported among decedents; no comparator", "CKD and liver disease were also reported", ""),
  ## Autoimmune disease
  a("AUT", 3,  "Hospitalisation", "ADJ", "Positive", "aOR 1.536 (1.297–1.819)", "autoimmune OR 1.536 (1.297–1.819)", ""),
  a("AUT", 3,  "Death", "ADJ", "Positive", "aOR 2.766 (1.679–4.557)", "autoimmune 2.766 (1.679–4.557)", ""),
  a("AUT", 4,  "Hospitalisation", "ADJ_DIR", "Positive", "Associated (adjusted logistic model); no numeric estimate extracted", "autoimmune disease, HTN and kidney disease associated with hospitalisation", ""),
  a("AUT", 4,  "Death", "NOT_REPORTED", "Not reported", "Not listed among factors associated with CHIK-attributed death", NA, "Conditional on hospitalisation"),
  a("AUT", 13, "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("AUT", 15, "Descriptive", "DESC", "Not applicable", "Described as a comorbidity in the CKD cohort only", NA, ""),
  a("AUT", 19, "Contextual (ICU-selected)", "CONTEXT", "Not applicable", "Described in ICU series; no estimate", "No condition-specific prognostic RR/OR", ""),
  ## Haematological disease
  a("HAE", 3,  "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("HAE", 3,  "Death", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("HAE", 4,  "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; not listed among associated factors", NA, ""),
  a("HAE", 4,  "Death", "NOT_EXTRACTED", "Not reported", "Assessed; not listed among associated factors", NA, ""),
  a("HAE", 13, "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("HAE", 15, "Descriptive", "DESC", "Not applicable", "Described as a comorbidity in the CKD cohort only", NA, ""),
  ## Peptic ulcer disease
  a("PEP", 3,  "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Acid-peptic disease assessed; no estimate extracted", NA, ""),
  a("PEP", 3,  "Death", "NOT_EXTRACTED", "Not reported", "Acid-peptic disease assessed; no estimate extracted", NA, ""),
  a("PEP", 13, "Hospitalisation", "NOT_EXTRACTED", "Not reported", "Assessed; no condition-specific estimate extracted", NA, ""),
  a("PEP", 15, "Descriptive", "DESC", "Not applicable", "Described as a comorbidity in the CKD cohort only", NA, "")
)

## Verbatim check against the workbook
audit$workbook_effect_text <- vapply(audit$no, effect_of, character(1))
audit$verbatim_check <- ifelse(is.na(audit$verify), "Not applicable (no numeric/direction text)",
  ifelse(mapply(function(v, txt) grepl(v, txt, fixed = TRUE), audit$verify, audit$workbook_effect_text),
         "Matched", "MISMATCH"))
if (any(audit$verbatim_check == "MISMATCH"))
  stop("Verbatim mismatch: ", paste(audit$no[audit$verbatim_check == "MISMATCH"], audit$key[audit$verbatim_check == "MISMATCH"], collapse = "; "))

type_label <- c(ADJ = "Adjusted estimate", ADJ_DIR = "Adjusted model; direction reported, no estimate extracted",
                CRUDE = "Crude estimate", CRUDE_NOEST = "Crude comparison; no estimate",
                POOLED = "Pooled synthesis estimate", NARR = "Synthesis; no pooled estimate",
                DESC = "Descriptive only", NOT_EXTRACTED = "Assessed; no result extracted",
                NOT_REPORTED = "Not reported among associated factors", CONTEXT = "Contextual only")
counts_type <- c("ADJ", "ADJ_DIR", "CRUDE", "CRUDE_NOEST")
audit <- merge(audit, studies, by = "no", all.x = TRUE, sort = FALSE)
audit$family_type <- family_type(audit$family)
audit$condition <- conditions[match(audit$key, cond_key)]
audit$evidence_type <- unname(type_label[audit$type])
audit$adjusted <- ifelse(audit$type %in% c("ADJ", "ADJ_DIR"), "Yes",
                  ifelse(audit$type %in% c("CRUDE", "CRUDE_NOEST"), "No", "Not applicable"))
audit$counts_toward_family <- ifelse(audit$type %in% counts_type &
  audit$outcome %in% c("Hospitalisation", "Death") &
  audit$family %in% c("BRA-A", "BRA-B", "BRA-C") | (audit$type %in% counts_type &
  audit$outcome %in% c("Hospitalisation", "Death") & grepl("^IND", audit$family)), "Yes", "No")
audit$counts_as_external <- ifelse(audit$counts_toward_family == "Yes" & grepl("^IND", audit$family), "Yes", "No")
audit <- audit[order(match(audit$key, cond_key),
                     match(audit$outcome, c("Hospitalisation", "Death", "Severe/composite")),
                     audit$no), ]
## Source facts parsed from the extracted estimate string (nothing imputed)
m <- regmatches(audit$estimate, regexec("(OR|RR) ([0-9.]+)(?: \\(([0-9.]+)–([0-9.]+)\\))?", audit$estimate, perl = TRUE))
pick <- function(k) vapply(m, function(x) if (length(x) && nzchar(x[k])) x[k] else NA_character_, character(1))
audit$effect_measure <- ifelse(is.na(pick(2)), ifelse(audit$type %in% c("ADJ_DIR", "NOT_EXTRACTED"), "Not extracted", "Not reported"), pick(2))
audit$point <- suppressWarnings(as.numeric(pick(3)))
audit$lower <- suppressWarnings(as.numeric(pick(4)))
audit$upper <- suppressWarnings(as.numeric(pick(5)))
audit$adjustment_variables <- ifelse(audit$no == 20 & audit$type == "ADJ", "Age",
  ifelse(audit$type %in% c("CRUDE", "CRUDE_NOEST"), "None (crude)",
  ifelse(audit$type %in% c("ADJ", "ADJ_DIR"), "Not extracted (multivariable model; covariates not recorded in workbook)", "Not applicable")))
## Reviewer judgements
audit$judgement_precision <- with(audit, ifelse(is.na(point), "Not assessable (no estimate extracted)",
  ifelse(is.na(lower), "Not assessable (CI not recorded)",
  ifelse(upper / lower > 3, "Imprecise (wide CI)",
  ifelse(lower < 1 & upper > 1, "Compatible with null", "Reasonably precise")))))
audit$judgement_directness <- with(audit, ifelse(outcome %in% c("Hospitalisation", "Death"),
  ifelse(no == 2, "Direct outcome, ICU-selected population",
  ifelse(no == 4 & outcome == "Death", "Direct (in-hospital death, conditional on hospitalisation)", "Direct")),
  ifelse(outcome == "Severe/composite", "Indirect (composite severity; not mortality)", "Indirect / contextual")))
doi_col <- grep("^DOI", names(ext), value = TRUE)[1]
design_col <- grep("^Study.design", names(ext), value = TRUE)[1]
audit$doi <- ext[[doi_col]][match(audit$no, ext$No)]
audit$design <- ext[[design_col]][match(audit$no, ext$No)]
audit_out <- data.frame(
  condition = audit$condition, study_no = audit$no, study = audit$study, doi = audit$doi,
  design = audit$design, setting = audit$setting,
  evidence_family = audit$family, sinan_overlap_status = audit$family_type, outcome = audit$outcome,
  evidence_type = audit$evidence_type, effect_measure = audit$effect_measure,
  estimate = audit$point, ci_lower = audit$lower, ci_upper = audit$upper,
  adjusted = audit$adjusted, adjustment_variables = audit$adjustment_variables,
  direction = audit$direction, estimate_as_extracted = audit$estimate, source_note = audit$note,
  judgement_directness = audit$judgement_directness, judgement_precision = audit$judgement_precision,
  counts_toward_family_n = audit$counts_toward_family, counts_as_external_family = audit$counts_as_external,
  verbatim_check = audit$verbatim_check, workbook_effect_text = audit$workbook_effect_text,
  stringsAsFactors = FALSE)
write.csv(audit_out, file.path(out_dir, "Evidence_Study_Level_Audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## ---- 5. Family counts --------------------------------------------------------
fam <- function(k, o, ext_only = FALSE) {
  x <- audit[audit$key == k & audit$outcome == o & audit$counts_toward_family == "Yes", ]
  if (ext_only) x <- x[grepl("^IND", x$family), ]
  sort(unique(x$family))
}

## ---- 6. Domain judgements (reviewer, qualitative) and reconciliation ---------
## One row per condition × target outcome. Historical category shown unchanged;
## rule_check = reviewer re-check of the workbook decision rules using death-only
## evidence for mortality. Discrepancies are flagged, not substituted.
d <- function(key, outcome, directness, consistency, precision, adjustment, rule_check, flag, flag_note)
  data.frame(key, outcome, directness, consistency, precision, adjustment, rule_check, flag, flag_note, stringsAsFactors = FALSE)
dom <- rbind(
  d("DM", "Hospitalisation", "Direct (all cases)", "Positive in all 3 families", "CIs exclude 1 in all 3",
    "Multivariable (BRA-A, BRA-B); age only (Perti)", "Supported", "No", ""),
  d("DM", "Death", "Direct in BRA-A/B/C; BRA-A partly conditional on hospitalisation; Añazco ICU-selected",
    "Positive in BRA-A, BRA-B and pooled synthesis; external families null (Economopoulou) or imprecise (Añazco)",
    "Adequate in BRA-A; CI not recorded for BRA-B; BRA-C and Añazco imprecise",
    "Multivariable (BRA-A, BRA-B); crude (BRA-C, Añazco)", "Supported", "Note",
    "Meets 'recurrent direction' but signal rests on SINAN-overlapping families; external evidence does not corroborate"),
  d("HTN", "Hospitalisation", "Direct (all cases)", "Positive in BRA-A and BRA-B; null in the only external family (Perti RR 1.00)",
    "CIs exclude 1 in BRA-A/B; Perti CI wide", "Multivariable (BRA-A, BRA-B); age only (Perti)",
    "Supported", "Note", "Recurrent within overlapping Brazilian families; not corroborated externally"),
  d("HTN", "Death", "Direct in BRA-A/B/C; Añazco ICU-selected",
    "Positive in BRA-A, BRA-B, BRA-C (crude) and pooled synthesis; not among death factors in Cerqueira-Silva or Economopoulou",
    "Adequate in BRA-A; CI not recorded for BRA-B; Añazco imprecise",
    "Multivariable (BRA-A, BRA-B); crude only (BRA-C, Añazco)", "Limited (borderline Supported)", "Note",
    "Historical 'severe/fatal' category; death-only evidence is recurrent within Brazil but inconsistent across studies; Réunion aRR 1.7 is severe disease, not death"),
  d("CKD", "Hospitalisation", "Direct (all cases)", "Positive in all 3 families", "CIs exclude 1 in BRA-A, BRA-B, Perti",
    "Multivariable (BRA-A, BRA-B); age only (Perti)", "Supported", "No", ""),
  d("CKD", "Death", "Direct in BRA-A/B/C; BRA-A partly conditional on hospitalisation",
    "Positive in all 3 families and pooled synthesis", "Wide CIs in BRA-C (aOR 12.77) and synthesis (OR 5.81)",
    "Multivariable (BRA-A, BRA-B, BRA-C)", "Supported", "Note",
    "No external (non-SINAN-overlapping) mortality family"),
  d("LIV", "Hospitalisation", "Assessed in BRA-A and BRA-B", "No association reported/extracted", "Not assessable",
    "Not assessable", "Insufficient", "No", ""),
  d("LIV", "Death", "Direct; one study conditional on hospitalisation", "Positive in BRA-A only (two papers)",
    "CI lower bound near 1 (aOR 1.781, 1.062–2.987)", "Multivariable (BRA-A)", "Limited", "No", ""),
  d("AUT", "Hospitalisation", "Direct (all cases)", "Positive in BRA-A only (two papers)", "CI excludes 1",
    "Multivariable (BRA-A)", "Limited", "No", ""),
  d("AUT", "Death", "Direct", "Positive in Furtado 2026 (BRA-A); not reported in Cerqueira-Silva 2025 (BRA-A)",
    "CI excludes 1 (aOR 2.766, 1.679–4.557)", "Multivariable (BRA-A)", "Limited", "Yes",
    "Historical category Insufficient, but the completed workbook records an adjusted death OR from BRA-A; prioritisation (Secondary) unchanged under either category"),
  d("HAE", "Hospitalisation", "Assessed in BRA-A and BRA-B", "No condition-specific result extracted", "Not assessable",
    "Not assessable", "Insufficient", "No", ""),
  d("HAE", "Death", "Assessed in BRA-A", "No condition-specific result extracted", "Not assessable",
    "Not assessable", "Insufficient", "No", ""),
  d("PEP", "Hospitalisation", "Assessed in BRA-A and BRA-B", "No condition-specific result extracted", "Not assessable",
    "Not assessable", "Insufficient", "Note",
    "Earlier summary (chik_comorbid_evidence.xlsx) cites a crude OR 2.18 from one small study; not traceable in the completed workbook"),
  d("PEP", "Death", "Assessed in BRA-A", "No condition-specific result extracted", "Not assessable",
    "Not assessable", "Insufficient", "No", "")
)
dom$condition <- conditions[match(dom$key, cond_key)]
dom$families <- mapply(function(k, o) paste(fam(k, o), collapse = ", "), dom$key, dom$outcome)
dom$n_families <- mapply(function(k, o) length(fam(k, o)), dom$key, dom$outcome)
dom$n_external <- mapply(function(k, o) length(fam(k, o, TRUE)), dom$key, dom$outcome)
dom$independence <- ifelse(dom$n_families == 0, "No contributing family",
  ifelse(dom$n_external == 0, paste0(dom$n_families, " famil", ifelse(dom$n_families == 1, "y", "ies"), ", all overlapping with target SINAN data"),
         paste0(dom$n_families, " families (", dom$n_external, " external)")))
dom$historical <- ifelse(dom$outcome == "Hospitalisation",
  historical$hist_hosp[match(dom$key, historical$key)],
  historical$hist_severe_fatal[match(dom$key, historical$key)])
dom$historical_source <- ifelse(dom$outcome == "Hospitalisation", "Script 16: hospitalisation",
                                "Script 16: severe/fatal (applied to mortality)")
dom$historical_prioritisation <- historical$hist_role[match(dom$key, historical$key)]
rule_role <- function(h, m) {
  h <- sub(" .*", "", h); m <- sub(" .*", "", m)
  if ((h == "Supported" && m != "Insufficient") || (m == "Supported" && h != "Insufficient")) "Core"
  else if (h == "Insufficient" && m == "Insufficient") "Not prioritised" else "Secondary"
}
hist_check <- sapply(cond_key, function(k) rule_role(dom$historical[dom$key == k & dom$outcome == "Hospitalisation"],
                                                    dom$historical[dom$key == k & dom$outcome == "Death"]))
rc_check <- sapply(cond_key, function(k) rule_role(dom$rule_check[dom$key == k & dom$outcome == "Hospitalisation"],
                                                  dom$rule_check[dom$key == k & dom$outcome == "Death"]))
stopifnot(all(hist_check == historical$hist_role))   # rule reproduces historical roles
dom$prioritisation_rule_check <- rc_check[dom$key]
dom <- dom[order(match(dom$key, cond_key), match(dom$outcome, c("Hospitalisation", "Death"))), ]

summary_out <- data.frame(
  condition = dom$condition, outcome = ifelse(dom$outcome == "Death", "Mortality", dom$outcome),
  outcome_directness = dom$directness, consistency = dom$consistency, precision = dom$precision,
  confounding_adjustment = dom$adjustment, independence = dom$independence,
  contributing_families = dom$families, n_families = dom$n_families, n_external_families = dom$n_external,
  historical_classification = dom$historical, historical_source = dom$historical_source,
  reviewer_rule_check = dom$rule_check, discrepancy_flag = dom$flag, discrepancy_note = dom$flag_note,
  historical_prioritisation = dom$historical_prioritisation,
  prioritisation_under_rule_check = dom$prioritisation_rule_check, stringsAsFactors = FALSE)
write.csv(summary_out, file.path(out_dir, "Evidence_Prioritisation_Summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## ---- 7. Display table --------------------------------------------------------
hosp_text <- c(
  "Adjusted positive associations in national (BRA-A) and Ceará (BRA-B) SINAN data; age-adjusted RR 1.39 (1.06–1.84) in an external cohort",
  "Adjusted positive associations in BRA-A and BRA-B; null in the external cohort (age-adjusted RR 1.00, 0.63–1.57)",
  "Adjusted positive associations in BRA-A and BRA-B (Ceará aOR 4.56); age-adjusted RR 1.52 (1.19–1.94) in an external cohort",
  "Assessed in BRA-A and BRA-B; no condition-specific association extracted",
  "Adjusted positive association in BRA-A only (aOR 1.54, 1.30–1.82)",
  "Assessed in BRA-A and BRA-B; no condition-specific association extracted",
  "Assessed in BRA-A and BRA-B; no condition-specific association extracted")
mort_text <- c(
  "Adjusted positive associations in BRA-A and BRA-B; crude, imprecise in BRA-C; pooled OR 2.86; no association in Réunion and imprecise in an ICU cohort",
  "Adjusted positive associations in BRA-A and BRA-B, crude in BRA-C; pooled OR 3.10; not among death factors in two studies; imprecise in an ICU cohort",
  "Adjusted positive associations in BRA-A, BRA-B and BRA-C (aOR 12.77, 2.75–59.4); pooled OR 5.81 (1.30–25.99); no external mortality estimate",
  "Adjusted positive association in BRA-A only (aOR 1.78, 1.06–2.99; in-hospital death)",
  "Adjusted aOR 2.77 (1.68–4.56) in one BRA-A study; not reported in the other†",
  "Assessed in BRA-A; no condition-specific association extracted",
  "Assessed in BRA-A; no condition-specific association extracted")
limits <- c(
  "Positive mortality evidence rests on SINAN-overlapping data; external studies null or imprecise",
  "Single external hospitalisation study null; mortality evidence inconsistent; Réunion signal is severe disease, not death",
  "Mortality evidence entirely from Brazilian SINAN-overlapping families; wide CIs; 'kidney disease' labels vary",
  "Single overlapping family; outcome-specific; broad SINAN category",
  "Single overlapping family; heterogeneous category",
  "No extractable estimates; heterogeneous category",
  "No extractable estimates")
n_syn <- function(k, o) length(unique(audit$no[audit$key == k & audit$outcome == o &
                                                 audit$type %in% c("POOLED", "NARR")]))
fam_text <- sapply(cond_key, function(k) {
  cell <- function(o, lab) {
    r <- dom[dom$key == k & dom$outcome == o, ]
    sprintf("%s: %d / %d / %d", lab, r$n_families - r$n_external, r$n_external, n_syn(k, o))
  }
  paste(cell("Hospitalisation", "H"), cell("Death", "M"), sep = "\n")
})
tab <- data.frame(
  condition = conditions,
  hosp = paste0(historical$hist_hosp, ". ", hosp_text),
  mort = paste0(historical$hist_severe_fatal, ifelse(cond_key %in% c("HTN"), "‡", ""), ". ", mort_text),
  fams = unname(fam_text), limits = limits, role = historical$hist_role, stringsAsFactors = FALSE)
write.csv(tab, file.path(out_dir, "Supplementary_Table_Evidence_Prioritisation_display.csv"), row.names = FALSE, fileEncoding = "UTF-8")

ft <- flextable(tab) |>
  set_header_labels(condition = "Underlying condition", hosp = "Hospitalisation evidence*",
                    mort = "Mortality evidence*", fams = "Evidence families, n
BRA / IND / SYN§",
                    limits = "Main limitations", role = "Prioritisation")
## Bold the category word at the start of the two evidence cells
for (col in c("hosp", "mort")) {
  for (i in seq_len(nrow(tab))) {
    txt <- tab[[col]][i]; cat_end <- regexpr("\\. ", txt)
    ft <- compose(ft, i = i, j = col, value = as_paragraph(
      as_chunk(substr(txt, 1, cat_end - 1), props = fp_text(font.family = font_family, bold = TRUE, font.size = 8.25, color = "#263238")),
      as_chunk(substr(txt, cat_end, nchar(txt)), props = fp_text(font.family = font_family, font.size = 8.25, color = "#263238"))))
  }
}
thin_rule <- fp_border(color = "#C9D1D5", width = 0.65)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
ft <- border_remove(ft)
ft <- hline_top(ft, part = "header", border = header_rule)
ft <- hline_bottom(ft, part = "header", border = header_rule)
ft <- hline(ft, i = seq_len(nrow(tab)), part = "body", border = thin_rule)
ft <- hline(ft, i = 3, part = "body", border = header_rule)
ft <- hline_bottom(ft, part = "body", border = header_rule)
ft <- bg(ft, part = "header", bg = "#EEF1F3")
ft <- bg(ft, part = "body", bg = "#FFFFFF")
ft <- color(ft, part = "all", color = "#263238")
ft <- font(ft, part = "all", fontname = font_family)
ft <- fontsize(ft, part = "header", size = 8.5)
ft <- fontsize(ft, part = "body", size = 8.25)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, j = c("condition", "role"), part = "body", bold = TRUE)
ft <- align(ft, align = "left", part = "all")
ft <- align(ft, j = c("fams", "role"), align = "center", part = "all")
ft <- valign(ft, part = "all", valign = "top")
ft <- padding(ft, part = "all", padding.top = 3, padding.bottom = 3, padding.left = 5, padding.right = 5)
widths <- c(1.15, 2.55, 2.75, 0.95, 2.05, 0.95)
for (j in seq_along(widths)) ft <- width(ft, j = j, width = widths[j])
ft <- set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))

title_bold <- "Supplementary Table Sx. "
title_rest <- "Evidence-informed prioritisation of SINAN-recorded underlying conditions for chikungunya severity analyses"
notes <- c(
  "Note. Classifications summarise a focused, qualitative evidence assessment that informed variable selection. They are not a GRADE, ROBINS-I or numerical scoring assessment, and \"Insufficient\" does not mean evidence of no association. Estimates are as extracted (OR or RR, 95% CI); this study's own estimates were not used.",
  "* Supported: direct condition-specific evidence for the outcome, with a recurrent direction of association across analytically informative evidence and/or corroboration from an independent source. Limited: a plausible signal restricted by a single or overlapping population, crude-only analysis, sparse events or wide CIs, or limited replication. Insufficient: no informative direct comparative evidence. Mortality evidence is death only; severe or composite outcomes, ICU-selected cohorts and fatal-case-only series were not counted as mortality evidence. Categories are reproduced unchanged from the project's earlier evidence matrix, where the second outcome was labelled severe/fatal.",
  "§ Distinct evidence families contributing condition-specific comparative evidence (adjusted or crude) for hospitalisation (H) or mortality (M), shown as overlapping Brazilian SINAN families (BRA) / independent primary-study families (IND) / evidence syntheses reporting that outcome (SYN). Publications from the same population form one family: BRA-A, national SINAN (Furtado 2026; Cerqueira-Silva 2025); BRA-B, Ceará 2016–2017 surveillance (Pinto 2019; Silva Junior 2018, 2019); BRA-C, Fortaleza 2017 case-control (Oliveira 2022). All BRA families overlap with the SINAN data analysed here and are not independent replication. IND = independent families not overlapping with the target data (US veterans/Puerto Rico, Perti 2016; Réunion, Economopoulou 2009; Paraguay ICU, Añazco 2026). Evidence syntheses (Micheleto 2025; Badawi 2018; Costa 2023; Cerbino-Neto 2020; de Almeida Barreto 2018) corroborate primary evidence they already include and are not counted as replication.",
  "Prioritisation: Core, Supported for at least one outcome and at least Limited for the other (included in the primary condition-specific and joint-profile models); Secondary, signal confined to one outcome and/or Limited overall; Not prioritised, Insufficient for both outcomes.",
  "† A re-check against the decision rules would classify autoimmune mortality evidence as Limited rather than Insufficient; prioritisation is unchanged. ‡ Borderline between Limited and Supported on death-only evidence. Both are documented in the reconciliation report."
)

doc <- read_docx()
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)))
doc <- body_add_fpar(doc, fpar(
  ftext(title_bold, prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
  ftext(title_rest, prop = fp_text(font.family = font_family, font.size = 10))))
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft, align = "center")
doc <- body_add_par(doc, "", style = "Normal")
for (k in seq_along(notes)) {
  if (k == 1) {
    doc <- body_add_fpar(doc, fpar(
      ftext("Note. ", prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)),
      ftext(sub("^Note\\. ", "", notes[k]), prop = fp_text(font.family = font_family, font.size = 8))))
  } else {
    doc <- body_add_fpar(doc, fpar(ftext(notes[k], prop = fp_text(font.family = font_family, font.size = 8))))
  }
}
docx_path <- file.path(out_dir, "Supplementary_Table_Evidence_Prioritisation_FINAL.docx")
if (file.exists(docx_path)) stop("Refusing to overwrite ", docx_path)
print(doc, target = docx_path)
flextable::save_as_image(ft, path = file.path(out_dir, "Supplementary_Table_Evidence_Prioritisation_FINAL.png"), res = 200)

## ---- 8. Optional workbook ----------------------------------------------------
wb <- createWorkbook()
for (s in list(list("Summary", summary_out), list("Study_level_audit", audit_out), list("Display_table", tab))) {
  addWorksheet(wb, s[[1]]); writeData(wb, s[[1]], s[[2]], headerStyle = createStyle(textDecoration = "bold", fgFill = "#EEF1F3"))
  freezePane(wb, s[[1]], firstRow = TRUE)
}
saveWorkbook(wb, file.path(out_dir, "Evidence_Prioritisation_Workbook.xlsx"), overwrite = FALSE)

cat("Audit rows:", nrow(audit_out), " verbatim matched:", sum(audit_out$verbatim_check == "Matched"), "\n")
print(summary_out[, c("condition", "outcome", "contributing_families", "n_families", "n_external_families",
                      "historical_classification", "reviewer_rule_check", "discrepancy_flag")], right = FALSE)
cat("Prioritisation (historical vs rule check):\n"); print(rbind(historical = historical$hist_role, rule_check = rc_check))
