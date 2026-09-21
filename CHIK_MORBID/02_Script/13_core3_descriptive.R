# ---------------------------------------------------------------------------
# 13_core3_descriptive.R
#
# Basic descriptive statistics for the core-3 (DM/HTN/CKD) cohort - meant to
# be the paper's first descriptive section, run and read BEFORE any of the
# RR modelling in 12_core3_condition_specific_rr.R. Answers three questions:
#
#   1. ATTRITION - how many records survive each eligibility step, from the
#      raw SINAN notification down to the final hosp_cohort/death_cohort
#      used for modelling?
#   2. MISSINGNESS - how complete is each of the 3 core conditions (and, for
#      context, the 4 non-core ones) - overall, and broken down by age, so
#      you can judge whether complete-case selection on DM/HTN/CKD is likely
#      to introduce age-related bias?
#   3. COMPOSITION - for the two final analysis cohorts, how many people
#      have each condition MARGINALLY (core3-style in 12_'s script - dm,
#      htn, ckd each counted on their own, regardless of the other two) vs
#      as a MUTUALLY EXCLUSIVE joint profile (core_profile-style in 12_'s
#      script - "CKD" means CKD and neither of the other two) - both
#      overall and by age band. This is what explains why, e.g., core3's
#      pooled CKD estimate and core_profile's "CKD-alone" estimate answer
#      different questions from overlapping but not identical group
#      definitions.
#
# Independent of 12_core3_condition_specific_rr.R: reads
# 01_Data/chik_sinan_individual_2015_2024.rds directly (needs
# 02_clean_chik_sinan_brazil.R + 00_setup.R already run), and does not read
# or write anything 12_ produces. core_cols/core_profile are rebuilt here
# from scratch with the SAME definitions as 12_'s add_core3(), specifically
# so the N's below reconcile exactly with 12_'s hosp_cohort/death_cohort -
# if you change one, change the other.
#
# Meant to be run interactively, section by section, and inspected as you
# go (every table below is print()ed). Everything is also written to one
# workbook: 03_Output/tables/core3_descriptive_tables.xlsx, with sheets
# Attrition, Missingness_overall, Missingness_by_age, Table1_hosp,
# Table1_death, Composition_marginal_hosp, Composition_profile_hosp,
# Composition_marginal_death, Composition_profile_death.
# ---------------------------------------------------------------------------

table_dir <- "03_Output/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

ind <- readRDS("01_Data/chik_sinan_individual_2015_2024.rds")

core_cols    <- c("diabetes", "hypertension", "renal_disease")
noncore_cols <- c("hepatopathy", "hematologic", "peptic_ulcer", "autoimmune")
all_comorb_cols <- c(core_cols, noncore_cols)

core_profile_levels <- c(
  "None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD"
)

age_band_10yr <- function(age_years) {
  breaks <- c(seq(0, 90, by = 10), 101)
  labels <- c(paste0(seq(0, 80, by = 10), "–", seq(9, 89, by = 10)), "90+")
  cut(age_years, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)
}

## ============================================================
## 1. Attrition / eligibility flow
## ============================================================
## Same filter sequence as 12_core3_condition_specific_rr.R's add_core3(),
## broken into one row per step so you can see exactly how much each
## criterion costs.

add_step <- function(flow, label, n) {
  dplyr::bind_rows(flow, tibble::tibble(step = label, n = n))
}

flow <- tibble::tibble(step = character(), n = integer())
flow <- add_step(flow, "1. All SINAN chikungunya records, 2015-2024 (raw)", nrow(ind))

step1 <- ind |> dplyr::filter(is_confirmed_chik)
flow <- add_step(flow, "2. Confirmed chikungunya (CLASSI_FIN = 13)", nrow(step1))

step2 <- step1 |> dplyr::filter(lubridate::year(event_date) >= 2017)
flow <- add_step(flow, "3. + notified/onset 2017 or later", nrow(step2))

step3 <- step2 |>
  dplyr::filter(
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female")
  )
flow <- add_step(flow, "4. + valid age (0-100) and sex known", nrow(step3))

step4_core3_known <- step3 |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes")))
flow <- add_step(
  flow, "5. + DM, HTN, and CKD all known (core-3 complete case)",
  nrow(step4_core3_known)
)

step4_all7_known <- step3 |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(all_comorb_cols), ~ .x %in% c("no", "yes")))
flow <- add_step(
  flow, "5b. [comparison only, not used downstream] all 7 comorbidities known",
  nrow(step4_all7_known)
)

hosp_eligible <- step4_core3_known |> dplyr::filter(hospitalised %in% c("no", "yes"))
flow <- add_step(
  flow, "6a. -> hosp_cohort: + hospitalisation status known", nrow(hosp_eligible)
)

death_eligible <- step4_core3_known |> dplyr::filter(!is.na(died_from_chik))
flow <- add_step(
  flow, "6b. -> death_cohort: + death status known", nrow(death_eligible)
)

## % relative to each step's PARENT - not simply the row above, because
## step 5b (all-7-known) and step 6b (death_cohort) both branch off an
## earlier step (4 and 5 respectively) rather than off the row immediately
## above them (5 and 6a). Using row-above blindly would divide death_cohort
## (which does not require hospitalised to be known, so it is LARGER) by
## hosp_cohort and print a nonsensical >100%.
parent_step <- c(
  NA,
  "1. All SINAN chikungunya records, 2015-2024 (raw)",
  "2. Confirmed chikungunya (CLASSI_FIN = 13)",
  "3. + notified/onset 2017 or later",
  "4. + valid age (0-100) and sex known",
  "4. + valid age (0-100) and sex known",
  "5. + DM, HTN, and CKD all known (core-3 complete case)",
  "5. + DM, HTN, and CKD all known (core-3 complete case)"
)
stopifnot(length(parent_step) == nrow(flow))

n_raw <- flow$n
names(n_raw) <- flow$step
flow$pct_of_parent_step <- round(100 * n_raw / n_raw[parent_step], 1)

flow$n <- format(flow$n, big.mark = ",")

print(flow, n = Inf)

## ============================================================
## 2. Missingness of each comorbidity field
## ============================================================
## Computed within step3 (confirmed + 2017+ + valid age/sex) - i.e. BEFORE
## any comorbidity-completeness filtering - so this shows exactly what
## complete-case selection on core_cols (or on all 7) discards, and why.

missingness_overall <- purrr::map_dfr(all_comorb_cols, function(col) {
  known <- step3[[col]] %in% c("no", "yes")
  tibble::tibble(
    condition   = col,
    is_core     = col %in% core_cols,
    n_total     = length(known),
    n_known     = sum(known),
    n_missing   = sum(!known),
    pct_missing = round(100 * mean(!known), 2)
  )
})

print(missingness_overall, n = Inf)

joint_missingness_overall <- tibble::tibble(
  requirement  = c("All 3 core (DM/HTN/CKD) known", "All 7 conditions known"),
  n_eligible   = c(nrow(step4_core3_known), nrow(step4_all7_known)),
  n_base       = nrow(step3),
  pct_eligible = round(100 * n_eligible / n_base, 1)
)

print(joint_missingness_overall)

## ---- by age ----

step3 <- step3 |> dplyr::mutate(age_band10 = age_band_10yr(age_years))

missingness_by_age <- purrr::map_dfr(core_cols, function(col) {
  step3 |>
    dplyr::mutate(known = .data[[col]] %in% c("no", "yes")) |>
    dplyr::group_by(age_band10) |>
    dplyr::summarise(
      n_total     = dplyr::n(),
      n_known     = sum(known),
      pct_missing = round(100 * mean(!known), 2),
      .groups     = "drop"
    ) |>
    dplyr::mutate(condition = col)
})

print(missingness_by_age, n = Inf)

joint_missingness_by_age <- step3 |>
  dplyr::mutate(
    core3_known = dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::group_by(age_band10) |>
  dplyr::summarise(
    n_total           = dplyr::n(),
    n_core3_known     = sum(core3_known),
    pct_core3_missing = round(100 * mean(!core3_known), 2),
    .groups           = "drop"
  )

print(joint_missingness_by_age, n = Inf)

## ============================================================
## 3. Build the same two analysis cohorts as 12_core3_condition_specific_rr.R
## ============================================================
## Deliberately duplicated (not sourced from 12_) so this script has no
## dependency on it - see header. Keep in sync with 12_'s add_core3() if
## either changes.

add_core3_flags <- function(df) {
  df |>
    dplyr::mutate(
      dm  = as.integer(diabetes == "yes"),
      htn = as.integer(hypertension == "yes"),
      ckd = as.integer(renal_disease == "yes"),
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
      age_band = age_band_10yr(age_years)
    )
}

hosp_cohort <- hosp_eligible |>
  add_core3_flags() |>
  dplyr::mutate(hosp_only = hospitalised == "yes")

death_cohort <- death_eligible |>
  add_core3_flags() |>
  dplyr::mutate(death_only = died_from_chik)

message(sprintf(
  "[13] hosp_cohort: %s (should match 12_'s hosp_cohort N exactly)",
  format(nrow(hosp_cohort), big.mark = ",")
))
message(sprintf(
  "[13] death_cohort: %s (should match 12_'s death_cohort N exactly)",
  format(nrow(death_cohort), big.mark = ",")
))

## ============================================================
## 4. Table 1 - baseline characteristics
## ============================================================
## Shows the marginal (core3-style) and mutually-exclusive (core_profile-
## style) condition counts side by side, in the SAME table, so the
## difference between them - the subject of the discussion this script
## follows from - is visible directly in the numbers rather than something
## you have to take on faith.

make_table1 <- function(data, outcome_var, outcome_label) {

  n_total <- nrow(data)
  fmt_n_pct <- function(n, denom = n_total) {
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / denom)
  }

  bind_rows_list <- list()

  bind_rows_list$n <- tibble::tibble(
    Characteristic = "N", Level = "", Value = format(n_total, big.mark = ",")
  )

  bind_rows_list$age <- tibble::tibble(
    Characteristic = "Age, years",
    Level = "median (IQR)",
    Value = sprintf(
      "%.0f (%.0f–%.0f)",
      median(data$age_years),
      stats::quantile(data$age_years, 0.25, names = FALSE),
      stats::quantile(data$age_years, 0.75, names = FALSE)
    )
  )

  bind_rows_list$sex <- data |>
    dplyr::count(sex) |>
    dplyr::transmute(
      Characteristic = "Sex", Level = as.character(sex), Value = fmt_n_pct(n)
    )

  bind_rows_list$outcome <- tibble::tibble(
    Characteristic = outcome_label,
    Level = "Yes",
    Value = fmt_n_pct(sum(data[[outcome_var]]))
  )

  bind_rows_list$marginal <- tibble::tibble(
    Characteristic = "Core condition present (marginal - regardless of the other two)",
    Level = c("Diabetes", "Hypertension", "Chronic kidney disease"),
    Value = c(fmt_n_pct(sum(data$dm)), fmt_n_pct(sum(data$htn)), fmt_n_pct(sum(data$ckd)))
  )

  bind_rows_list$profile <- data |>
    dplyr::count(core_profile) |>
    dplyr::transmute(
      Characteristic = "Joint profile (mutually exclusive)",
      Level = as.character(core_profile),
      Value = fmt_n_pct(n)
    )

  dplyr::bind_rows(bind_rows_list)
}

table1_hosp  <- make_table1(hosp_cohort, "hosp_only", "Hospitalised")
table1_death <- make_table1(death_cohort, "death_only", "Died from chikungunya")

print(table1_hosp, n = Inf)
print(table1_death, n = Inf)

## ============================================================
## 5. Age-specific comorbidity composition
## ============================================================
## For each cohort: (a) marginal prevalence of DM/HTN/CKD by age band
## (core3-style), and (b) the distribution of the 8 mutually-exclusive
## profiles by age band (core_profile-style) - the same two framings as
## Table 1 above, but split out by age instead of pooled.

composition_marginal_by_age <- function(data) {
  purrr::map_dfr(c("dm", "htn", "ckd"), function(v) {
    data |>
      dplyr::group_by(age_band) |>
      dplyr::summarise(
        n_total = dplyr::n(),
        n_present = sum(.data[[v]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        condition = v,
        pct_present = round(100 * n_present / n_total, 2)
      )
  })
}

composition_profile_by_age <- function(data) {
  data |>
    dplyr::count(age_band, core_profile, name = "n") |>
    dplyr::group_by(age_band) |>
    dplyr::mutate(
      age_band_n = sum(n),
      pct = round(100 * n / age_band_n, 2)
    ) |>
    dplyr::ungroup()
}

composition_marginal_hosp  <- composition_marginal_by_age(hosp_cohort)
composition_profile_hosp   <- composition_profile_by_age(hosp_cohort)
composition_marginal_death <- composition_marginal_by_age(death_cohort)
composition_profile_death  <- composition_profile_by_age(death_cohort)

print(composition_marginal_hosp, n = Inf)
print(composition_profile_hosp, n = Inf)
print(composition_marginal_death, n = Inf)
print(composition_profile_death, n = Inf)

## ============================================================
## 6. Save everything to one workbook
## ============================================================

wb <- openxlsx::createWorkbook()

add_sheet <- function(wb, name, df) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, df)
}

add_sheet(wb, "Attrition", flow)
add_sheet(wb, "Missingness_overall", missingness_overall)
add_sheet(wb, "Missingness_joint_overall", joint_missingness_overall)
add_sheet(wb, "Missingness_by_age", missingness_by_age)
add_sheet(wb, "Missingness_joint_by_age", joint_missingness_by_age)
add_sheet(wb, "Table1_hosp", table1_hosp)
add_sheet(wb, "Table1_death", table1_death)
add_sheet(wb, "Composition_marginal_hosp", composition_marginal_hosp)
add_sheet(wb, "Composition_profile_hosp", composition_profile_hosp)
add_sheet(wb, "Composition_marginal_death", composition_marginal_death)
add_sheet(wb, "Composition_profile_death", composition_profile_death)

openxlsx::saveWorkbook(
  wb,
  file.path(table_dir, "core3_descriptive_tables.xlsx"),
  overwrite = TRUE
)

message(sprintf(
  "[13] saved %s",
  file.path(table_dir, "core3_descriptive_tables.xlsx")
))
