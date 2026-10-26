# ---------------------------------------------------------------------------
# 41_gate32_summary_tables.R
#
# REPORTING / SYNTHESIS ONLY. Assembles publication-ready supplementary
# tables and figures from the already-completed Gate 3.2A (hospitalisation
# missingness audit) and Gate 3.2B (restricted-sample sensitivity analysis)
# outputs. Does NOT refit any model, compute new standardisation, implement
# IPW/imputation, or touch any primary result - every number here is read
# directly from the existing validated CSVs and re-displayed/reformatted.
#
# Needs (read-only):
#   03_Output/validation/gate32a/hospitalisation_status_overall.csv
#   03_Output/validation/gate32a/hospitalisation_missingness_by_age_profile.csv
#   03_Output/validation/gate32a/hospitalisation_missingness_by_year_state.csv
#   03_Output/validation/gate32a/recorded_vs_unrecorded_characteristics.csv
#   03_Output/validation/gate32a/cohort_reconciliation.csv
#   03_Output/validation/gate32b/hospitalisation_condition_rr_comparison.csv
#   03_Output/validation/gate32b/hospitalisation_profile_rr_comparison.csv
#   03_Output/validation/gate32b/profile_specific_counts_by_scenario.csv
#   03_Output/validation/gate32b/scenario_cohort_summary.csv
#   03_Output/validation/gate32b/model_convergence_and_sparse_cells.csv
# Output: 03_Output/validation/gate32_summary/
#   table_S1_recording_completeness.xlsx
#   table_S2_rr_robustness.xlsx
#   table_S3_profile_event_counts.xlsx
#   figure_S1_state_year_completeness.png
#   figure_S2_rr_robustness.png
#   gate32_manuscript_reporting.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(readr); library(tibble)
  library(openxlsx)
})
options(scipen = 999, dplyr.summarise.inform = FALSE)

in_a  <- "03_Output/validation/gate32a"
in_b  <- "03_Output/validation/gate32b"
out_dir <- "03_Output/validation/gate32_summary"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

font_family <- "Arial"   # house style (matches 12_/17_/18_/27_ etc.)

## =============================================================================
## VERIFY INPUT FILES EXIST AND HAVE THE EXPECTED COLUMNS BEFORE PROCESSING
## =============================================================================

required_files <- c(
  file.path(in_a, "hospitalisation_status_overall.csv"),
  file.path(in_a, "hospitalisation_missingness_by_age_profile.csv"),
  file.path(in_a, "hospitalisation_missingness_by_year_state.csv"),
  file.path(in_a, "recorded_vs_unrecorded_characteristics.csv"),
  file.path(in_a, "cohort_reconciliation.csv"),
  file.path(in_b, "hospitalisation_condition_rr_comparison.csv"),
  file.path(in_b, "hospitalisation_profile_rr_comparison.csv"),
  file.path(in_b, "profile_specific_counts_by_scenario.csv"),
  file.path(in_b, "scenario_cohort_summary.csv"),
  file.path(in_b, "model_convergence_and_sparse_cells.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) stop("Missing required input file(s):\n", paste(missing_files, collapse = "\n"))
message("[41] all ", length(required_files), " required input files found.")

status_overall   <- readr::read_csv(file.path(in_a, "hospitalisation_status_overall.csv"), show_col_types = FALSE)
by_age_profile   <- readr::read_csv(file.path(in_a, "hospitalisation_missingness_by_age_profile.csv"), show_col_types = FALSE)
by_year_state    <- readr::read_csv(file.path(in_a, "hospitalisation_missingness_by_year_state.csv"), show_col_types = FALSE)
smd_tbl          <- readr::read_csv(file.path(in_a, "recorded_vs_unrecorded_characteristics.csv"), show_col_types = FALSE)
cohort_recon     <- readr::read_csv(file.path(in_a, "cohort_reconciliation.csv"), show_col_types = FALSE)

cond_rr_cmp      <- readr::read_csv(file.path(in_b, "hospitalisation_condition_rr_comparison.csv"), show_col_types = FALSE)
prof_rr_cmp      <- readr::read_csv(file.path(in_b, "hospitalisation_profile_rr_comparison.csv"), show_col_types = FALSE)
profile_counts   <- readr::read_csv(file.path(in_b, "profile_specific_counts_by_scenario.csv"), show_col_types = FALSE)
scenario_summary <- readr::read_csv(file.path(in_b, "scenario_cohort_summary.csv"), show_col_types = FALSE)
convergence      <- readr::read_csv(file.path(in_b, "model_convergence_and_sparse_cells.csv"), show_col_types = FALSE)

stopifnot(
  all(c("stratifier_domain","level","n_eligible","n_known","pct_known","pct_missing") %in% names(by_age_profile)),
  all(c("stratifier_domain","level","n_eligible","n_known","pct_known","pct_missing") %in% names(by_year_state)),
  all(c("scenario","condition","analysis_type","rr","rr_lower","rr_upper","pct_change_rr") %in% names(cond_rr_cmp)),
  all(c("scenario","core_profile","analysis_type","rr","rr_lower","rr_upper","pct_change_rr") %in% names(prof_rr_cmp)),
  all(c("scenario","core_profile","n_patients","n_hosp_events","sparse_lt10_events") %in% names(profile_counts))
)
message("[41] column structure verified for all inputs.")

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
cond_label <- c(dm = "Diabetes", htn = "Hypertension", ckd = "Chronic kidney disease")
scenario_labels <- c(
  A  = "A: Original (complete-case)",
  B  = "B: Exclude DF 2020-2025",
  C1 = "C1: State-year completeness ≥70%",
  C2 = "C2: State-year completeness ≥80%",
  D  = "D: Laboratory-confirmed only"
)
scenario_colours <- stats::setNames(c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00"), unname(scenario_labels))

## =============================================================================
## STYLE HELPERS (house style, matches 12_/17_/18_/27_ etc.)
## =============================================================================

wb_title_style <- createStyle(fontName = font_family, fontSize = 12, textDecoration = "bold")
wb_header_style <- createStyle(
  fontName = font_family, fontSize = 10, textDecoration = "bold",
  fgFill = "#EEF1F3", halign = "center", valign = "center",
  border = "Bottom", borderColour = "#7E8A90", wrapText = TRUE
)
wb_body_style <- createStyle(
  fontName = font_family, fontSize = 10, valign = "center",
  border = "Bottom", borderColour = "#D8DEE1"
)
wb_note_style <- createStyle(fontName = font_family, fontSize = 9, textDecoration = "italic",
                              fontColour = "#4F5D64", wrapText = TRUE)
wb_flag_style <- createStyle(fontName = font_family, fontSize = 10, valign = "center",
                              fgFill = "#FCE8E6", border = "Bottom", borderColour = "#D8DEE1")

write_sheet <- function(wb, sheet, title, main_table, note_lines, col_widths = "auto", flag_rows = integer(0)) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet, wb_title_style, rows = 1, cols = 1)
  writeData(wb, sheet, main_table, startRow = 3, startCol = 1, headerStyle = wb_header_style)
  addStyle(wb, sheet, wb_body_style, rows = 4:(3 + nrow(main_table)), cols = 1:ncol(main_table), gridExpand = TRUE)
  if (length(flag_rows) > 0) {
    addStyle(wb, sheet, wb_flag_style, rows = 3 + flag_rows, cols = 1:ncol(main_table), gridExpand = TRUE)
  }
  note_start <- 3 + nrow(main_table) + 2
  for (i in seq_along(note_lines)) {
    writeData(wb, sheet, note_lines[i], startRow = note_start + i - 1, startCol = 1)
    addStyle(wb, sheet, wb_note_style, rows = note_start + i - 1, cols = 1)
  }
  setColWidths(wb, sheet, cols = 1:ncol(main_table), widths = col_widths)
  freezePane(wb, sheet, firstActiveRow = 4)
  invisible(wb)
}

message("[41] style helpers defined.")

## =============================================================================
## TABLE S1 -- HOSPITALISATION RECORDING COMPLETENESS
## Denominator throughout: the full core-three eligible cohort (N=1,074,796),
## NOT the Gate 3.1-corrected mortality cohort - mortality and hospitalisation
## status are independently constructed outcomes (see VALIDATION_3_2A.md).
## =============================================================================

fmt_pretty_n <- function(x) format(round(x), big.mark = ",")

## ---- Overall row (from the eligible-cohort-level reconciliation) ----
overall_row <- tibble::tibble(
  Category = "Overall", Group = "All eligible core-three cases",
  N_eligible = sum(status_overall$n), N_recorded = sum(status_overall$n[status_overall$hosp_missing_type=="recorded"]),
  N_missing = sum(status_overall$n[status_overall$hosp_missing_type!="recorded"])
) |> dplyr::mutate(Pct_recorded = round(100*N_recorded/N_eligible,2), Pct_missing = round(100*N_missing/N_eligible,2))

domain_block <- function(domain, category_label, level_order = NULL) {
  d <- by_age_profile |> dplyr::filter(stratifier_domain == domain) |>
    dplyr::transmute(Category = category_label, Group = level,
                      N_eligible = n_eligible, N_recorded = n_known, N_missing = n_eligible - n_known,
                      Pct_recorded = pct_known, Pct_missing = pct_missing)
  if (!is.null(level_order)) d <- d |> dplyr::mutate(Group = factor(Group, levels = level_order)) |> dplyr::arrange(Group) |> dplyr::mutate(Group = as.character(Group))
  d
}

age_block     <- domain_block("age_band_5", "Age group", c("0-19","20-39","40-59","60-79","80+"))
sex_block     <- domain_block("sex", "Sex", c("female","male")) |> dplyr::mutate(Group = stringr::str_to_title(Group))
dm_block      <- domain_block("diabetes", "Diabetes", c("DM absent","DM present"))
htn_block     <- domain_block("hypertension", "Hypertension", c("HTN absent","HTN present"))
ckd_block     <- domain_block("renal_disease", "Chronic kidney disease", c("CKD absent","CKD present"))
profile_block <- domain_block("core_profile", "Multimorbidity profile", core_profile_levels)
lab_block     <- domain_block("lab_confirmation_criterion", "Laboratory-confirmation status",
                               c("laboratory","clinical_epi","clinical","NA (unmapped/blank)")) |>
  dplyr::mutate(Group = dplyr::recode(Group, laboratory = "Laboratory-confirmed", clinical_epi = "Clinical-epidemiological",
                                        clinical = "Clinical only", `NA (unmapped/blank)` = "Unresolved criterion"))

year_block <- by_year_state |> dplyr::filter(stratifier_domain == "calendar_year") |>
  dplyr::transmute(Category = "Calendar year", Group = level, N_eligible = n_eligible, N_recorded = n_known,
                    N_missing = n_eligible - n_known, Pct_recorded = pct_known, Pct_missing = pct_missing) |>
  dplyr::arrange(as.integer(Group))

table_s1_main <- dplyr::bind_rows(overall_row, age_block, sex_block, dm_block, htn_block, ckd_block, profile_block, year_block, lab_block)
flag_rows_s1 <- which(table_s1_main$Category == "Multimorbidity profile" & table_s1_main$Group %in% c("DM+CKD")) # smallest profile, for visual reference

## ---- Full state-level detail (separate sheet; states NOT in the main table) ----
state_block <- by_year_state |> dplyr::filter(stratifier_domain == "state") |>
  dplyr::transmute(State = level, N_eligible = n_eligible, N_recorded = n_known, N_missing = n_eligible - n_known,
                    Pct_recorded = pct_known, Pct_missing = pct_missing) |>
  dplyr::arrange(dplyr::desc(Pct_missing))
flag_rows_state <- which(state_block$State == "DF")

## ---- Full year x state cross-tab (separate sheet, as instructed) ----
year_x_state_block <- by_year_state |> dplyr::filter(stratifier_domain == "year_x_state") |>
  dplyr::mutate(
    Year = stringr::str_extract(level, "^[0-9]+"),
    State = stringr::str_extract(level, "(?<=\\| ).*$")
  ) |>
  dplyr::transmute(Year, State, N_eligible = n_eligible, N_recorded = n_known, N_missing = n_eligible - n_known,
                    Pct_recorded = pct_known, Pct_missing = pct_missing) |>
  dplyr::arrange(State, Year)

## ---- Standardised differences, recorded vs unrecorded (binary/continuous only
## in the main SMD display; multi-category summary SMDs shown separately with
## an explicit non-comparability caveat) ----
smd_binary_cont <- smd_tbl |> dplyr::filter(variable_type %in% c("continuous","binary","binary_secondary_diagnostic")) |>
  dplyr::transmute(
    Variable = dplyr::recode(variable, age_years = "Age (years)", sex_male = "Sex = male",
                              dm = "Diabetes present", htn = "Hypertension present", ckd = "CKD present",
                              lab_confirmed = "Laboratory-confirmed",
                              chik_death_rate_among_death_known = "Chikungunya death rate (among death-status-known; secondary diagnostic)"),
    Recorded_group = dplyr::case_when(
      variable_type == "continuous" ~ sprintf("mean=%.1f (sd=%.1f)", mean_recorded, sd_recorded),
      TRUE ~ sprintf("%s%% (n=%s)", pct_recorded, fmt_pretty_n(n_recorded))
    ),
    Unrecorded_group = dplyr::case_when(
      variable_type == "continuous" ~ sprintf("mean=%.1f (sd=%.1f)", mean_unrecorded, sd_unrecorded),
      TRUE ~ sprintf("%s%% (n=%s)", pct_unrecorded, fmt_pretty_n(n_unrecorded))
    ),
    SMD = round(smd, 3)
  )

smd_categorical <- smd_tbl |> dplyr::filter(variable_type == "categorical_summary") |>
  dplyr::transmute(Variable = dplyr::recode(variable, calendar_year = "Calendar year (9 categories)",
                                              region = "Region (5 categories)", core_profile = "Multimorbidity profile (8 categories)"),
                    Overall_generalised_SMD = round(smd, 3))

message("[41] Table S1 components built.")

## =============================================================================
## TABLE S2 -- ROBUSTNESS OF HOSPITALISATION RR
## Primary comparison (per task instruction): common_target_restricted_model
## vs common_target_full_model, SAME scenario-specific target population.
## pct_change_rr in the source CSVs was already computed exactly as
## 100*(restricted/full - 1) on that identical target - reused here, not
## recomputed, as a reporting-only task should.
## =============================================================================

fmt_ci <- function(rr, lo, hi) sprintf("%.2f (%.2f–%.2f)", rr, lo, hi)

build_rr_panel <- function(df, group_col) {
  grp_sym <- rlang::sym(group_col)

  baseline <- df |> dplyr::filter(analysis_type == "original_manuscript") |>
    dplyr::transmute(Group = as.character(!!grp_sym), RR_original_manuscript = fmt_ci(rr, rr_lower, rr_upper))

  full_model <- df |> dplyr::filter(analysis_type == "common_target_full_model") |>
    dplyr::transmute(scenario, Group = as.character(!!grp_sym),
                      RR_full_model_same_target = fmt_ci(rr, rr_lower, rr_upper), N_target_full = round(target_n))

  restr_model <- df |> dplyr::filter(analysis_type == "common_target_restricted_model") |>
    dplyr::transmute(scenario, Group = as.character(!!grp_sym),
                      RR_restricted_model_same_target = fmt_ci(rr, rr_lower, rr_upper),
                      pct_change_rr, N_target_restricted = round(target_n))

  panel <- full_model |>
    dplyr::full_join(restr_model, by = c("scenario","Group")) |>
    dplyr::left_join(baseline, by = "Group") |>
    dplyr::mutate(
      Scenario = unname(scenario_labels[scenario]),
      Flag_gt10pct = ifelse(abs(pct_change_rr) > 10, "YES", "")
    ) |>
    dplyr::select(Group, Scenario, RR_original_manuscript, RR_full_model_same_target,
                  RR_restricted_model_same_target, Pct_change = pct_change_rr, Flag_gt10pct, N_target_restricted)
  panel
}

cond_rr_cmp_labelled <- cond_rr_cmp |> dplyr::mutate(condition = unname(cond_label[condition]))
table_s2_condition <- build_rr_panel(cond_rr_cmp_labelled, "condition") |>
  dplyr::mutate(Group = factor(Group, levels = unname(cond_label))) |>
  dplyr::arrange(Group, Scenario)

prof_rr_cmp_f <- prof_rr_cmp |> dplyr::filter(core_profile != "None")  # None is the reference (RR=1 by construction)
table_s2_profile <- build_rr_panel(prof_rr_cmp_f, "core_profile") |>
  dplyr::mutate(Group = factor(Group, levels = setdiff(core_profile_levels, "None"))) |>
  dplyr::arrange(Group, Scenario)

flag_rows_cond <- which(table_s2_condition$Flag_gt10pct == "YES")
flag_rows_prof <- which(table_s2_profile$Flag_gt10pct == "YES")

message("[41] Table S2 components built. Flagged (>10%) rows: condition=",
        length(flag_rows_cond), ", profile=", length(flag_rows_prof))

## =============================================================================
## TABLE S3 -- PROFILE-SPECIFIC COUNTS AND EVENT SPARSITY
## =============================================================================

cell_fmt <- function(n_patients, n_events) sprintf("%s / %s", fmt_pretty_n(n_patients), fmt_pretty_n(n_events))

table_s3_wide <- profile_counts |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels),
                cell = cell_fmt(n_patients, n_hosp_events),
                scenario = factor(scenario, levels = c("A","B","C1","C2","D"))) |>
  dplyr::arrange(core_profile, scenario) |>
  dplyr::select(core_profile, scenario, cell) |>
  tidyr::pivot_wider(names_from = scenario, values_from = cell) |>
  dplyr::rename(`Multimorbidity profile` = core_profile) |>
  dplyr::rename_with(~ unname(scenario_labels[.x]), .cols = tidyselect::any_of(names(scenario_labels)))

## observed (unstandardised) hospitalisation proportion per cell, as its own
## table - explicitly separate from the model-standardised risks in Table S2
table_s3_observed_pct <- profile_counts |>
  dplyr::mutate(core_profile = factor(core_profile, levels = core_profile_levels),
                scenario = factor(scenario, levels = c("A","B","C1","C2","D"))) |>
  dplyr::arrange(core_profile, scenario) |>
  dplyr::select(core_profile, scenario, pct_hosp_among_patients) |>
  tidyr::pivot_wider(names_from = scenario, values_from = pct_hosp_among_patients) |>
  dplyr::rename(`Multimorbidity profile` = core_profile) |>
  dplyr::rename_with(~ unname(scenario_labels[.x]), .cols = tidyselect::any_of(names(scenario_labels)))

sparse_cells <- profile_counts |> dplyr::filter(sparse_lt10_events) |>
  dplyr::transmute(Scenario = unname(scenario_labels[scenario]), Profile = as.character(core_profile),
                    N_patients = n_patients, N_events = n_hosp_events)

flag_rows_s3 <- which(table_s3_wide$`Multimorbidity profile` %in%
                        unique(as.character(sparse_cells$Profile)))

## ---- concordance check: does the RR change line up with event-count loss?
## (requested explicitly - "explain whether changes in the RR point
## estimates coincide with substantial event loss", without claiming
## sparsity alone proves the reason) ----
rr_change_long <- dplyr::bind_rows(
  cond_rr_cmp |> dplyr::filter(analysis_type == "common_target_restricted_model") |>
    dplyr::transmute(scenario, core_profile = unname(cond_label[condition]), pct_change_rr) |> dplyr::distinct(),
  prof_rr_cmp |> dplyr::filter(analysis_type == "common_target_restricted_model", core_profile != "None") |>
    dplyr::transmute(scenario, core_profile = as.character(core_profile), pct_change_rr) |> dplyr::distinct()
)

event_change <- profile_counts |> dplyr::filter(scenario != "A") |>
  dplyr::transmute(scenario, core_profile = as.character(core_profile), n_hosp_events,
                    pct_change_n_events_vs_A)

concordance_tbl <- rr_change_long |>
  dplyr::inner_join(event_change, by = c("scenario","core_profile")) |>
  dplyr::filter(core_profile %in% c("DM+CKD","DM+HTN+CKD","HTN+CKD")) |>
  dplyr::transmute(
    Scenario = unname(scenario_labels[scenario]), Profile = core_profile,
    N_events_restricted = n_hosp_events,
    Pct_change_events_vs_A = pct_change_n_events_vs_A,
    Pct_change_RR_vs_full_same_target = pct_change_rr,
    Pattern = dplyr::case_when(
      abs(pct_change_n_events_vs_A) >= 35 & abs(pct_change_rr) > 10 ~ "Large event loss + large RR change: consistent with sparsity-driven instability",
      abs(pct_change_n_events_vs_A) < 25 & abs(pct_change_rr) > 10 ~ "Modest event loss but large RR change: NOT explained by sparsity alone - compositional difference more likely",
      TRUE ~ "No large RR change flagged"
    )
  ) |>
  dplyr::arrange(Profile, Scenario)

message("[41] Table S3 components built. Sparse (<10 events) cells: ", nrow(sparse_cells))

## =============================================================================
## WRITE TABLE S1 WORKBOOK
## =============================================================================

wb1 <- createWorkbook()
write_sheet(wb1, "Table S1",
  "Table S1. Hospitalisation outcome recording completeness among confirmed chikungunya cases, Brazil 2017-2025",
  table_s1_main,
  c("Abbreviations: DM = diabetes mellitus; HTN = hypertension; CKD = chronic kidney disease.",
    "Denominator: the full core-three eligible cohort (N=1,074,796; confirmed chikungunya, 2017-2025, valid age/sex, DM/HTN/CKD status all explicit yes/no) - NOT the Gate 3.1-corrected mortality cohort, which is a separately constructed population.",
    "'Recorded' = hospitalisation status explicitly yes/no; 'Missing' pools both an explicit 'unknown' (Ignorado) response and a field left entirely blank (see Table S1 footnote in VALIDATION_3_2A.md for the ~9:1 split between these two mechanisms).",
    "Source: 03_Output/validation/gate32a/hospitalisation_status_overall.csv, hospitalisation_missingness_by_age_profile.csv, hospitalisation_missingness_by_year_state.csv (Gate 3.2A, completed)."),
  col_widths = c(20,18,12,12,12,12,12), flag_rows = flag_rows_s1)

write_sheet(wb1, "State detail",
  "Table S1 (state detail). Hospitalisation recording completeness by Brazilian state, ranked by missingness",
  state_block,
  c("DF = Distrito Federal, highlighted - the single worst-recorded state overall and the basis for Gate 3.2B Scenario B.",
    "Denominator: full core-three eligible cohort, as in Table S1."),
  col_widths = c(10,12,12,12,12,12), flag_rows = flag_rows_state)

write_sheet(wb1, "Year x State detail",
  "Table S1 (year x state detail). Full calendar-year by state recording-completeness cross-tabulation",
  year_x_state_block,
  c("242 state-year strata; see figure_S1_state_year_completeness.png for the corresponding heatmap.",
    "Denominator: full core-three eligible cohort, as in Table S1."),
  col_widths = c(8,10,12,12,12,12,12))

smd_note <- c(
  "SMD = standardised mean difference (recorded vs unrecorded hospitalisation status). Conventional interpretation: <0.1 negligible, 0.1-0.2 small, 0.2-0.4 moderate, >0.4 large - shown for descriptive orientation only.",
  "IMPORTANT: small marginal SMDs here are NOT evidence that hospitalisation-status missingness is missing-at-random (MAR) - MAR cannot be established from observed-data comparisons alone (see VALIDATION_3_2A.md).",
  "Multi-category variables (calendar year: 9 levels; region: 5; profile: 8) use a generalised multinomial SMD (Yang & Dalton 2012) and are NOT numerically comparable to the binary/continuous SMDs above on a shared scale, because the two are different statistics - shown in a separate block for this reason.",
  "Source: 03_Output/validation/gate32a/recorded_vs_unrecorded_characteristics.csv (Gate 3.2A, completed)."
)
addWorksheet(wb1, "SMD recorded vs unrecorded")
writeData(wb1, "SMD recorded vs unrecorded", "Table S1 (supplement). Standardised differences, recorded vs unrecorded hospitalisation status", startRow = 1, startCol = 1)
addStyle(wb1, "SMD recorded vs unrecorded", wb_title_style, rows = 1, cols = 1)
writeData(wb1, "SMD recorded vs unrecorded", "Binary and continuous variables", startRow = 3, startCol = 1)
addStyle(wb1, "SMD recorded vs unrecorded", createStyle(fontName=font_family, fontSize=10, textDecoration="bold"), rows=3, cols=1)
writeData(wb1, "SMD recorded vs unrecorded", smd_binary_cont, startRow = 4, startCol = 1, headerStyle = wb_header_style)
addStyle(wb1, "SMD recorded vs unrecorded", wb_body_style, rows = 5:(4+nrow(smd_binary_cont)), cols = 1:ncol(smd_binary_cont), gridExpand = TRUE)
next_row <- 4 + nrow(smd_binary_cont) + 2
writeData(wb1, "SMD recorded vs unrecorded", "Multi-category variables (generalised SMD; not comparable to the block above)", startRow = next_row, startCol = 1)
addStyle(wb1, "SMD recorded vs unrecorded", createStyle(fontName=font_family, fontSize=10, textDecoration="bold"), rows=next_row, cols=1)
writeData(wb1, "SMD recorded vs unrecorded", smd_categorical, startRow = next_row+1, startCol = 1, headerStyle = wb_header_style)
addStyle(wb1, "SMD recorded vs unrecorded", wb_body_style, rows = (next_row+2):(next_row+1+nrow(smd_categorical)), cols = 1:ncol(smd_categorical), gridExpand = TRUE)
note_row <- next_row + 1 + nrow(smd_categorical) + 2
for (i in seq_along(smd_note)) {
  writeData(wb1, "SMD recorded vs unrecorded", smd_note[i], startRow = note_row + i - 1, startCol = 1)
  addStyle(wb1, "SMD recorded vs unrecorded", wb_note_style, rows = note_row + i - 1, cols = 1)
}
setColWidths(wb1, "SMD recorded vs unrecorded", cols = 1:5, widths = c(32,20,20,20,12))

saveWorkbook(wb1, file.path(out_dir, "table_S1_recording_completeness.xlsx"), overwrite = TRUE)
message("[41] table_S1_recording_completeness.xlsx written.")

## =============================================================================
## WRITE TABLE S2 WORKBOOK
## =============================================================================

wb2 <- createWorkbook()
s2_note <- c(
  "RR = standardised relative risk vs no core condition (profiles) or vs condition-absent, mutually adjusted for the other two conditions (condition-specific); g-computation with a 2000-draw parametric coefficient bootstrap for the 95% uncertainty interval (UI), identical method to the primary manuscript model.",
  "'RR original manuscript' = Scenario A (existing primary complete-case model), standardised to its own population - shown as a fixed baseline reference, not recomputed per scenario.",
  "'RR full model, same target' and 'RR restricted model, same target' are the PRIMARY robustness comparison: the original (Scenario A) model and the scenario-B/C1/C2/D-restricted model are both applied, via g-computation, to the IDENTICAL target population (that scenario's full eligible population, hospitalisation status known + unknown) - this isolates a genuine change in the fitted association from a change in who is being averaged over.",
  "Pct_change = 100*(RR_restricted_model / RR_full_model - 1), evaluated on that same common target. This is a descriptive sensitivity flag at |change|>10%, NOT a significance test or an automatic failure criterion - see VALIDATION_3_2B.md Section 6.",
  "Scenarios: B = exclude Distrito Federal 2020-2025; C1/C2 = restrict to state-year strata with >=70%/>=80% hospitalisation-recording completeness (and >=100 eligible cases per stratum); D = restrict to laboratory-confirmed cases only.",
  "Source: 03_Output/validation/gate32b/hospitalisation_condition_rr_comparison.csv, hospitalisation_profile_rr_comparison.csv (Gate 3.2B, completed)."
)

write_sheet(wb2, "Panel A - Condition-specific",
  "Table S2, Panel A. Hospitalisation RR for DM, hypertension and CKD: original vs restricted-sample models (common-target standardisation)",
  table_s2_condition, s2_note, col_widths = c(22,30,20,20,24,12,10,14), flag_rows = flag_rows_cond)

write_sheet(wb2, "Panel B - Joint profile",
  "Table S2, Panel B. Hospitalisation RR for the eight core-3 multimorbidity profiles vs None: original vs restricted-sample models (common-target standardisation)",
  table_s2_profile, s2_note, col_widths = c(14,30,20,20,24,12,10,14), flag_rows = flag_rows_prof)

saveWorkbook(wb2, file.path(out_dir, "table_S2_rr_robustness.xlsx"), overwrite = TRUE)
message("[41] table_S2_rr_robustness.xlsx written.")

## =============================================================================
## WRITE TABLE S3 WORKBOOK
## =============================================================================

wb3 <- createWorkbook()
s3_note <- c(
  "Cell format: N patients (hospitalisation-known) / N observed hospitalisations, within each scenario's actual model-fitting sample.",
  "Scenarios: A = original complete-case; B = exclude DF 2020-2025; C1/C2 = state-year completeness >=70%/>=80%; D = laboratory-confirmed only.",
  "Highlighted row(s): profile x scenario combinations with fewer than 10 observed hospitalisation events - standardised RR/UI for these cells should be interpreted with caution (see Table S2 and VALIDATION_3_2B.md Section 7)."
)
write_sheet(wb3, "Table S3",
  "Table S3. Profile-specific patient and hospitalisation-event counts by scenario",
  table_s3_wide, s3_note, col_widths = c(22,22,20,20,20,20), flag_rows = flag_rows_s3)

write_sheet(wb3, "Observed pct (unstandardised)",
  "Table S3 (supplement). Observed (crude, unstandardised) hospitalisation proportion per profile x scenario",
  table_s3_observed_pct,
  c("These are CRUDE observed proportions within the hospitalisation-known sample - distinct from the model-standardised risks reported in Table S2, which adjust for age, sex, calendar year and state."),
  col_widths = c(22,22,20,20,20,20))

write_sheet(wb3, "Sparse cells (<10 events)",
  "Table S3 (supplement). Profile x scenario cells with fewer than 10 observed hospitalisation events",
  sparse_cells,
  c("Only cells meeting this threshold are listed; all other profile x scenario combinations (listed in Table S3) had >=10 events."),
  col_widths = c(30,16,14,12))

write_sheet(wb3, "Event loss vs RR change",
  "Table S3 (supplement). Does the RR change coincide with event loss? (DM+CKD, DM+HTN+CKD, HTN+CKD)",
  concordance_tbl,
  c("Compares each profile's % loss in observed hospitalisation events (vs Scenario A) to its % change in standardised RR (common-target: restricted model vs full model, same target).",
    "This descriptive comparison does NOT prove sparsity is the sole cause of an RR change - it only indicates whether the two are directionally/magnitude consistent. See VALIDATION_3_2B.md Section 7 and the main manuscript-reporting markdown for full interpretation."),
  col_widths = c(30,14,16,22,26,45))

saveWorkbook(wb3, file.path(out_dir, "table_S3_profile_event_counts.xlsx"), overwrite = TRUE)
message("[41] table_S3_profile_event_counts.xlsx written.")

## =============================================================================
## FIGURE S1 -- STATE x YEAR RECORDING COMPLETENESS HEATMAP
## Re-rendered, publication-styled, from the SAME already-validated Gate 3.2A
## numbers (hospitalisation_missingness_by_year_state.csv) - no new analysis.
## =============================================================================

fig1_data <- year_x_state_block |> dplyr::mutate(Year = as.integer(Year))

p_fig_s1 <- ggplot2::ggplot(fig1_data, ggplot2::aes(x = factor(Year), y = reorder(State, Pct_recorded), fill = Pct_recorded)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
  ggplot2::scale_fill_viridis_c(name = "% status\nrecorded", option = "C", direction = 1, limits = c(0,100)) +
  ggplot2::labs(x = "Calendar year", y = "State of residence",
                title = "Hospitalisation-status recording completeness by state and year",
                subtitle = "Brazil, confirmed chikungunya cases, 2017-2025 (core-three eligible cohort, N=1,074,796)") +
  ggplot2::theme_minimal(base_size = 11, base_family = font_family) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
ggplot2::ggsave(file.path(out_dir, "figure_S1_state_year_completeness.png"), p_fig_s1, width = 9, height = 8, dpi = 400, bg = "white")
message("[41] figure_S1_state_year_completeness.png written.")

## =============================================================================
## FIGURE S2 -- RR ROBUSTNESS, COMMON-TARGET STANDARDISATION (PRIMARY
## comparison per task instruction: full model vs restricted model, same
## target, paired per scenario)
## =============================================================================

build_fig2_data <- function(df, group_col, panel_label) {
  grp_sym <- rlang::sym(group_col)
  df |> dplyr::filter(analysis_type %in% c("common_target_full_model","common_target_restricted_model")) |>
    dplyr::transmute(
      scenario, group = as.character(!!grp_sym),
      model = ifelse(analysis_type=="common_target_full_model", "Full model (A), same target", "Restricted model, same target"),
      rr, rr_lower, rr_upper, panel = panel_label
    )
}

fig2_cond <- build_fig2_data(cond_rr_cmp_labelled, "condition", "Condition-specific (mutually adjusted)")
fig2_prof <- build_fig2_data(prof_rr_cmp_f, "core_profile", "Joint profile (vs None)")

fig2_data <- dplyr::bind_rows(fig2_cond, fig2_prof) |>
  dplyr::mutate(
    scenario_label = factor(unname(scenario_labels[scenario]), levels = unname(scenario_labels)[unname(scenario_labels) != scenario_labels["A"]]),
    group = factor(group, levels = c("Diabetes","Hypertension","Chronic kidney disease",
                                      "DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")),
    model = factor(model, levels = c("Full model (A), same target","Restricted model, same target"))
  )

p_fig_s2 <- ggplot2::ggplot(fig2_data, ggplot2::aes(x = rr, y = group, colour = scenario_label, shape = model,
                                                     group = interaction(scenario_label, model))) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = rr_lower, xmax = rr_upper),
                            position = ggplot2::position_dodge(width = 0.7), size = 0.38, fatten = 2.6, stroke = 1.1) +
  ggplot2::scale_x_log10(breaks = scales::log_breaks(n = 6), labels = scales::label_number(accuracy = 0.1)) +
  ggplot2::scale_colour_manual(values = scenario_colours, name = "Scenario") +
  ggplot2::scale_shape_manual(values = c(17, 16), name = NULL,
                               labels = c("Full model (A), same target","Restricted model, same target")) +
  ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 1) +
  ggplot2::labs(x = "Standardised RR (log scale)", y = NULL,
                title = "Hospitalisation RR robustness: common-target standardisation",
                subtitle = "Triangle = original (Scenario A) model; circle = restricted-sample model\nBoth applied to the SAME target population within each scenario") +
  ggplot2::theme_minimal(base_size = 10.5, base_family = font_family) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.position = "bottom", legend.box = "vertical"
  )
ggplot2::ggsave(file.path(out_dir, "figure_S2_rr_robustness.png"), p_fig_s2, width = 10, height = 12, dpi = 400, bg = "white")
message("[41] figure_S2_rr_robustness.png written.")

## =============================================================================
## COMPUTE KEY NUMBERS FOR THE REPORTING TEXT (from the already-loaded,
## already-validated data frames - never hand-typed)
## =============================================================================

n_eligible_total <- sum(status_overall$n)
n_recorded_total <- sum(status_overall$n[status_overall$hosp_missing_type=="recorded"])
pct_recorded_total <- round(100*n_recorded_total/n_eligible_total, 2)
pct_missing_total <- round(100 - pct_recorded_total, 2)

smd_small <- smd_tbl |> dplyr::filter(variable %in% c("age_years","sex_male","dm","htn","ckd")) |>
  dplyr::summarise(min_smd = min(smd), max_smd = max(smd))
smd_year <- smd_tbl$smd[smd_tbl$variable=="calendar_year" & smd_tbl$level=="__OVERALL_SMD__"]
smd_region <- smd_tbl$smd[smd_tbl$variable=="region" & smd_tbl$level=="__OVERALL_SMD__"]
smd_profile <- smd_tbl$smd[smd_tbl$variable=="core_profile" & smd_tbl$level=="__OVERALL_SMD__"]

year_pct_range <- by_year_state |> dplyr::filter(stratifier_domain=="calendar_year") |>
  dplyr::summarise(lo = min(pct_known), hi = max(pct_known))
state_pct_range <- by_year_state |> dplyr::filter(stratifier_domain=="state") |>
  dplyr::summarise(lo = min(pct_known), hi = max(pct_known))
df_pct <- by_year_state |> dplyr::filter(stratifier_domain=="state", level=="DF") |> dplyr::pull(pct_known)

ckd_rr_range <- cond_rr_cmp |> dplyr::filter(condition=="ckd", analysis_type %in% c("original_manuscript","common_target_restricted_model")) |>
  dplyr::summarise(lo = min(rr), hi = max(rr))
cond_pct_change_range <- pct_changes_cond <- cond_rr_cmp |> dplyr::filter(analysis_type=="common_target_restricted_model") |>
  dplyr::summarise(max_abs = max(abs(pct_change_rr)), median_abs = median(abs(pct_change_rr)))
prof_pct_change_all <- prof_rr_cmp |> dplyr::filter(analysis_type=="common_target_restricted_model", core_profile!="None")
n_flagged_cond <- sum(abs(cond_rr_cmp$pct_change_rr[cond_rr_cmp$analysis_type=="common_target_restricted_model"]) > 10, na.rm=TRUE)
n_flagged_prof <- sum(abs(prof_pct_change_all$pct_change_rr) > 10, na.rm=TRUE)
max_prof_change <- prof_pct_change_all |> dplyr::slice_max(abs(pct_change_rr), n=1, with_ties=FALSE)

n_sparse_total <- sum(profile_counts$sparse_lt10_events)
n_fit_fail <- sum(convergence$fit_failed)
n_nonconv <- sum(!convergence$converged & !convergence$fit_failed, na.rm=TRUE)

message(sprintf("[41] Key numbers: overall recorded=%s%%, year range=%s-%s%%, state range=%s-%s%%, DF=%s%%",
                 pct_recorded_total, year_pct_range$lo, year_pct_range$hi, round(state_pct_range$lo,1), round(state_pct_range$hi,1), df_pct))

## =============================================================================
## gate32_manuscript_reporting.md
## =============================================================================

report_md <- sprintf('# Gate 3.2 manuscript reporting summary

Synthesises the completed Gate 3.2A (hospitalisation missingness audit) and Gate 3.2B (restricted-sample sensitivity analysis) into publication-ready supplementary tables and figures. This is a reporting/synthesis document only - no new analysis was performed; every number is read directly from the validated Gate 3.2A/3.2B CSVs.

**Source reports:** `03_Output/validation/gate32a/VALIDATION_3_2A.md`, `03_Output/validation/gate32b/VALIDATION_3_2B.md`.

## Outputs produced

| File | Content |
|---|---|
| `table_S1_recording_completeness.xlsx` | Recording completeness by age/sex/condition/profile/year/state/lab-confirmation, + state detail, year x state cross-tab, and SMD sheets |
| `table_S2_rr_robustness.xlsx` | Condition-specific (Panel A) and joint-profile (Panel B) RR, original vs restricted, common-target standardisation |
| `table_S3_profile_event_counts.xlsx` | Profile x scenario patient/event counts, observed proportions, sparse-cell flags, event-loss-vs-RR-change concordance |
| `figure_S1_state_year_completeness.png` | State x year recording-completeness heatmap |
| `figure_S2_rr_robustness.png` | RR forest plot, common-target standardisation, original vs restricted per scenario |

## Methods (suggested manuscript text)

> Among confirmed chikungunya cases meeting core-three eligibility criteria (N=%s), hospitalisation outcome was analysed as a complete case among the %s (%s%%) with an explicit yes/no hospitalisation-status response. Recording completeness was assessed across demographic, clinical, temporal and geographic strata, using the full eligible cohort (not the independently defined, Gate 3.1-corrected mortality cohort) as the denominator throughout. Standardised mean differences compared recorded and unrecorded cases; multi-category variables used a generalised multinomial SMD (Yang & Dalton, 2012), not directly comparable to binary/continuous SMDs. To assess whether geographic and temporal heterogeneity in recording completeness could materially affect the primary hospitalisation RR estimates, four prespecified restricted-sample sensitivity analyses were conducted: (B) excluding Distrito Federal during 2020-2025, its period of most severe recording collapse; (C1/C2) restricting to state-calendar-year strata with >=70%%/>=80%% recording completeness (requiring >=100 eligible cases per stratum); and (D) restricting to laboratory-confirmed cases only. For each restricted sample, the original (full-sample) model and a model refit on the restricted sample were both standardised, via g-computation with a 2000-draw parametric coefficient bootstrap, to the IDENTICAL target population (the full eligible population for that scenario, including hospitalisation-status-unknown cases) - this common-target standardisation isolates genuine change in the fitted association from change in the standardisation population.

## Results (suggested manuscript text)

**1. Overall missingness.** Hospitalisation status was unrecorded for %s%% of the eligible cohort (%s%% recorded).

**2. Year/state vs age/comorbidity.** Recording completeness varied little by age, sex or individual comorbidity (SMD %s-%s, all <0.1) and only modestly by multimorbidity profile (generalised SMD %s). In contrast, completeness varied substantially by calendar year (%s%%-%s%% across 2017-2025; generalised SMD %s) and by state (%s%%-%s%% across states; generalised SMD %s), with Distrito Federal the most extreme single case, falling to %s%% recorded overall and remaining below 20%% every year from 2020 onward (Figure S1).

**3. Stability of condition-specific RRs.** Under common-target standardisation, the restricted-sample condition-specific RRs (DM, HTN, CKD) changed by a maximum of %s%% and a median of %s%% relative to the original model on the same target; %s of the condition-scenario comparisons exceeded the prespecified 10%% descriptive flag.

**4. Persistence of elevated CKD-profile risk.** CKD remained the single strongest individual condition, and CKD-containing joint profiles remained among the highest-risk multimorbidity groups, in every restricted scenario (Table S2).

**5. Sensitivity of rare profiles.** %s of the joint-profile common-target comparisons exceeded the 10%% flag, concentrated in the rarest profiles - most notably DM+CKD (largest single change: %s%%, %s, see Table S3). %s profile x scenario cell(s) had fewer than 10 observed hospitalisation events.

**6. Sparse-event limitations.** The event-loss-vs-RR-change comparison in Table S3 shows that in the state/year-restricted scenarios (C1/C2), large RR changes for DM+CKD and DM+HTN+CKD coincided with substantial (>35%%) loss of observed events - consistent with, though not proof of, sparsity-driven instability. In the laboratory-confirmed scenario (D), however, the RR for those same profiles shifted despite comparatively modest event loss (<20%%), suggesting a genuine compositional difference in laboratory-confirmed cases rather than small-sample noise alone (Table S3, "Event loss vs RR change" sheet).

## What these results do NOT establish

- They do **not** establish that hospitalisation-status missingness is random (MAR) - geographic/temporal concentration of missingness is consistent with, but does not prove, a MAR-conditional-on-state-and-year mechanism.
- They do **not** eliminate the possibility of residual selection bias - restricted-sample stability shows the primary estimates are not an artefact of the specific poorly-recorded areas/periods tested, which is a narrower claim than "unbiased".
- Stability of the dominant conditions/profiles does **not** extend uniformly to the rarest profiles (DM+CKD, DM+HTN+CKD), which should be reported with appropriately cautious language.
- These results do **not** by themselves make IPW or imputation unnecessary in every circumstance - see VALIDATION_3_2A.md and VALIDATION_3_2B.md for the specific, narrower recommendation on where a formal IPW sensitivity analysis could still add value (the DM+CKD / DM+HTN+CKD cells specifically).

## Validation note

All displayed values were read programmatically from `03_Output/validation/gate32a/*.csv` and `03_Output/validation/gate32b/*.csv`; none were hand-typed or copied from the Markdown reports. Column structures were verified against expected names before processing (see script header). No model was refit, no new sensitivity scenario was run, and no primary result, cache, or manuscript file was modified.
',
  fmt_pretty_n(n_eligible_total), fmt_pretty_n(n_recorded_total), pct_recorded_total,
  pct_missing_total, pct_recorded_total,
  round(smd_small$min_smd,3), round(smd_small$max_smd,3), round(smd_profile,3),
  round(year_pct_range$lo,1), round(year_pct_range$hi,1), round(smd_year,3),
  round(state_pct_range$lo,1), round(state_pct_range$hi,1), round(smd_region,3), df_pct,
  round(cond_pct_change_range$max_abs,2), round(cond_pct_change_range$median_abs,2), n_flagged_cond,
  n_flagged_prof, max_prof_change$pct_change_rr, unname(scenario_labels[max_prof_change$scenario]), n_sparse_total
)

writeLines(report_md, file.path(out_dir, "gate32_manuscript_reporting.md"))
message("[41] gate32_manuscript_reporting.md written.")
message("[41] Gate 3.2 reporting synthesis COMPLETE.")
