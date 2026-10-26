# ---------------------------------------------------------------------------
# 38_mortality_outcome_validation.R   -- Gate 3.1
#
# AUDIT + CORRECTION of chikungunya-attributed death classification
# (died_from_chik) in 02_clean_chik_sinan_brazil.R, and its downstream
# impact on the mutually-adjusted DM/HTN/CKD and 8-profile mortality RR
# models in 12_core3_condition_specific_rr.R.
#
# Official definition (verified against 01_Data/sinan_chik_docs/
# dic_dados_chikungunya.pdf, field 74 "Evolução Do Caso" / EVOLUCAO):
#   1 = cura                        (cured)
#   2 = obito pelo agravo           (death FROM chikungunya)
#   3 = obito por outras causas     (death from another cause)
#   4 = obito em investigacao       (death under investigation)
#   9 = ignorado                    (unknown)
# This matches map_evolucao in 02_clean_chik_sinan_brazil.R exactly - the
# LABEL mapping was already correct. The bug is in the boolean derived from
# it: died_from_chik = (EVOLUCAO == "2"). In R, `x == "2"` is FALSE (not NA)
# for every non-NA, non-"2" string - including "4", "9", "0" (an unmapped
# stray code found in the raw files, not in the official dictionary) and,
# critically, blank/"" EVOLUCAO (which data.table::fread reads as the empty
# string "", not R's NA, so `"" == "2"` is also FALSE, not NA). The result:
# died_from_chik currently has ZERO NA values in the cleaned data (verified
# empirically below) - so the existing `!is.na(died_from_chik)` filter in
# every downstream script is a no-op, and the mortality "control" group
# silently includes everyone with an unresolved/unrecorded outcome as a
# confirmed non-death.
#
# This script does NOT modify 02_clean_chik_sinan_brazil.R or
# 12_core3_condition_specific_rr.R. It re-derives the corrected outcome
# independently from the already-cleaned RDS's `evolution` label column
# (which is already unmapped-safe - see Part A), refits the SAME model
# structures used in 12_, and writes every output to 03_Output/validation/
# gate31/ so the original manuscript caches (01_Data/core3_rr_death.RData,
# core_profile_rr_death.RData) are never touched.
#
# Needs: 01_Data/chik_sinan_individual_2015_2025.rds
#        01_Data/sinan_chik_csv/CHIKBR{17..25}.csv.zip (Part A.1 raw audit)
#        01_Data/core3_rr_death.RData, core_profile_rr_death.RData (baseline
#          comparison only - read, never written)
# Output: 03_Output/validation/gate31/mortality_status_audit.csv
#         03_Output/validation/gate31/mortality_status_by_year_profile.csv
#         03_Output/validation/gate31/mortality_rr_before_after.csv
#         03_Output/validation/gate31/mortality_rr_comparison.png
#         03_Output/validation/gate31/VALIDATION_3_1.md
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(scales); library(mgcv); library(MASS)
  library(lubridate)
})
options(scipen = 999)

val_dir <- "03_Output/validation/gate31"
if (!dir.exists(val_dir)) dir.create(val_dir, recursive = TRUE)

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")
profile_age_levels <- c("0-19", "20-39", "40-59", "60-79", "80+")

## =============================================================================
## PHASE A -- AUDIT BEFORE MODIFYING ANYTHING
## =============================================================================

## ---- A.1: raw EVOLUCAO tabulation, unmapped-safe, directly from source CSVs ----
## (the cleaned RDS's `evolution` label column already silently collapses any
## unmapped raw code to NA via recode() - this step checks what those unmapped
## codes actually are BEFORE trusting that collapse.)

RAW_DIR <- "01_Data/sinan_chik_csv"
AUDIT_YEAR_START <- 2017
AUDIT_YEAR_END <- 2025
RAW_KEEP_COLS <- c("NU_ANO", "CLASSI_FIN", "EVOLUCAO", "DT_OBITO")

list_raw_files <- function() {
  files <- list.files(RAW_DIR, pattern = "^CHIKBR\\d{2}\\.csv(\\.zip)?$", full.names = TRUE)
  tibble::tibble(
    path = files,
    yy = as.integer(stringr::str_match(basename(files), "CHIKBR(\\d{2})")[, 2]),
    is_zip = grepl("\\.zip$", files)
  ) |>
    dplyr::mutate(year = ifelse(yy >= 50, 1900 + yy, 2000 + yy)) |>
    dplyr::filter(year >= AUDIT_YEAR_START, year <= AUDIT_YEAR_END) |>
    dplyr::arrange(year, dplyr::desc(is_zip)) |>
    dplyr::distinct(year, .keep_all = TRUE)
}

read_one_raw <- function(path, year) {
  message(sprintf("[Part A.1] reading %s", basename(path)))
  if (grepl("\\.zip$", path)) {
    inside <- sub("\\.zip$", "", basename(path))
    cmd <- sprintf("unzip -p %s %s", shQuote(path), shQuote(inside))
    dt <- data.table::fread(cmd = cmd, sep = ",", encoding = "Latin-1",
                             select = RAW_KEEP_COLS, colClasses = "character",
                             showProgress = FALSE, fill = TRUE)
  } else {
    dt <- data.table::fread(path, sep = ",", encoding = "Latin-1",
                             select = RAW_KEEP_COLS, colClasses = "character",
                             showProgress = FALSE, fill = TRUE)
  }
  dt$SOURCE_YEAR <- year
  dt
}

raw_index <- list_raw_files()
if (nrow(raw_index) == 0) {
  stop("[Part A.1] BLOCKER: no raw SINAN CSVs found in ", RAW_DIR,
       " - cannot audit raw EVOLUCAO. Run 01_fetch_chik_sinan_brazil.R first.")
}

raw_list <- mapply(read_one_raw, path = raw_index$path, year = raw_index$year, SIMPLIFY = FALSE)
raw_all <- data.table::rbindlist(raw_list, use.names = TRUE, fill = TRUE)
raw_confirmed <- raw_all[CLASSI_FIN == "13"]

message(sprintf("[Part A.1] %s confirmed chikungunya rows (raw), %d-%d",
                format(nrow(raw_confirmed), big.mark = ","), AUDIT_YEAR_START, AUDIT_YEAR_END))

official_codes <- c("1", "2", "3", "4", "9")
raw_evolucao_tab <- raw_confirmed[, .N, by = EVOLUCAO][order(-N)]
raw_evolucao_tab[, pct := round(100 * N / sum(N), 4)]
raw_evolucao_tab[, in_official_dictionary := EVOLUCAO %in% official_codes]
raw_evolucao_tab[, category := dplyr::case_when(
  EVOLUCAO == "1" ~ "cured",
  EVOLUCAO == "2" ~ "death_chik",
  EVOLUCAO == "3" ~ "death_other",
  EVOLUCAO == "4" ~ "death_under_investigation",
  EVOLUCAO == "9" ~ "unknown",
  EVOLUCAO == "" ~ "blank",
  TRUE ~ "UNMAPPED (not in official dictionary)"
)]

cat("\n[Part A.1] Raw EVOLUCAO tabulation among confirmed chikungunya cases, ",
    AUDIT_YEAR_START, "-", AUDIT_YEAR_END, " (includes blank + unmapped):\n", sep = "")
print(as.data.frame(raw_evolucao_tab))

n_unmapped_nonblank <- sum(raw_evolucao_tab$N[!raw_evolucao_tab$in_official_dictionary & raw_evolucao_tab$EVOLUCAO != ""])
cat(sprintf("\n[Part A.1] Non-blank EVOLUCAO values outside the official {1,2,3,4,9} set: %d records\n", n_unmapped_nonblank))

## DT_OBITO presence cross-check (dictionary: DT_OBITO mandatory for 2/3/4)
raw_confirmed[, has_dt_obito := !is.na(DT_OBITO) & DT_OBITO != ""]
dt_obito_check <- raw_confirmed[EVOLUCAO %in% official_codes, .N, by = .(EVOLUCAO, has_dt_obito)][order(EVOLUCAO, has_dt_obito)]
cat("\n[Part A.1] DT_OBITO presence by EVOLUCAO (sanity check vs dictionary's mandatory-field rule for 2/3/4):\n")
print(as.data.frame(dt_obito_check))

## ---- A.2 / A.6: trace through the cleaned RDS and reproduce the ORIGINAL
## (buggy) mortality cohort N / death count exactly as the current pipeline
## computes it - BASELINE, saved before any correction is applied. ----

ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
core_cols <- c("diabetes", "hypertension", "renal_disease")

add_core3_fields <- function(df) {
  df |>
    dplyr::mutate(
      year = factor(lubridate::year(event_date)),
      sex = factor(sex, levels = c("female", "male")),
      uf_residence = factor(uf_residence),
      dm = as.integer(diabetes == "yes"),
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
      age_band_profile = cut(age_years, breaks = c(0, 20, 40, 60, 80, 101),
                              labels = profile_age_levels, right = FALSE, include.lowest = TRUE)
    )
}

analytic_base <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(core_cols), ~ .x %in% c("no", "yes"))
  ) |>
  add_core3_fields()

cat(sprintf("\n[Part A.2] Full analytic population (confirmed chik, 2017-2025, valid age/sex, known DM/HTN/CKD): %s\n",
            format(nrow(analytic_base), big.mark = ",")))

## BASELINE reproduction - exactly 12_'s current death_cohort construction.
baseline_death_cohort <- analytic_base |> dplyr::filter(!is.na(died_from_chik))
baseline_n <- nrow(baseline_death_cohort)
baseline_deaths <- sum(baseline_death_cohort$died_from_chik, na.rm = TRUE)
cat(sprintf(
  "[Part A.6][BASELINE - current/original pipeline] death_cohort N = %s, deaths (died_from_chik==TRUE) = %d\n",
  format(baseline_n, big.mark = ","), baseline_deaths
))
cat(sprintf(
  "[Part A.2] died_from_chik NA count in this population: %d (confirms the !is.na() filter is currently a no-op)\n",
  sum(is.na(analytic_base$died_from_chik))
))

saveRDS(
  list(baseline_death_cohort_n = baseline_n, baseline_deaths = baseline_deaths,
       timestamp = Sys.time()),
  file.path(val_dir, "baseline_mortality_cohort_PRESERVED.rds")
)

## ---- A.3: how many records are misclassified as confirmed non-deaths? ----

evolution_vs_current <- analytic_base |>
  dplyr::mutate(evolution_cat = dplyr::coalesce(evolution, "blank_or_unmapped")) |>
  dplyr::count(evolution_cat, died_from_chik, name = "n")

cat("\n[Part A.3] Cross-tab: evolution label x current (buggy) died_from_chik:\n")
print(as.data.frame(evolution_vs_current), row.names = FALSE)

## Per Phase B's definition: these categories SHOULD be NA (unresolved), but
## are currently coded FALSE (confirmed non-death) - this is the bug.
misclassified_categories <- c("unknown", "death_under_investigation")
n_misclassified_labelled <- analytic_base |>
  dplyr::filter(evolution %in% misclassified_categories) |>
  nrow()
n_misclassified_blank <- analytic_base |>
  dplyr::filter(is.na(evolution)) |>
  nrow()
n_misclassified_total <- n_misclassified_labelled + n_misclassified_blank

cat(sprintf(
  "\n[Part A.3] Records currently classified as died_from_chik==FALSE (confirmed non-death) that should be NA (unresolved status):\n"
))
cat(sprintf("  evolution == 'unknown' or 'death_under_investigation': %s\n", format(n_misclassified_labelled, big.mark = ",")))
cat(sprintf("  evolution == NA (blank EVOLUCAO or an unmapped raw code):  %s\n", format(n_misclassified_blank, big.mark = ",")))
cat(sprintf("  TOTAL misclassified: %s / %s (%.2f%% of the mortality analytic cohort)\n",
            format(n_misclassified_total, big.mark = ","), format(nrow(analytic_base), big.mark = ","),
            100 * n_misclassified_total / nrow(analytic_base)))

## ---- A.4 / A.3 stratified: by year, age group, sex, state, profile ----

age_group_of <- function(x) cut(x, breaks = c(0, 20, 40, 60, 80, 101),
                                 labels = profile_age_levels, right = FALSE, include.lowest = TRUE)

status_by_strata <- function(df, strata_var) {
  df |>
    dplyr::mutate(
      status_group = dplyr::case_when(
        evolution %in% c("cured") ~ "resolved_non_event",
        evolution %in% c("death_chik") ~ "chik_death",
        evolution %in% c("death_other") ~ "resolved_non_event_other_death",
        evolution %in% c("unknown", "death_under_investigation") ~ "unresolved_labelled",
        is.na(evolution) ~ "unresolved_blank_or_unmapped"
      )
    ) |>
    dplyr::count(.data[[strata_var]], status_group, name = "n") |>
    dplyr::group_by(.data[[strata_var]]) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 3)) |>
    dplyr::ungroup() |>
    dplyr::rename(stratum_value = 1) |>
    dplyr::mutate(stratum_value = as.character(stratum_value), stratifier = strata_var, .before = 1)
}

by_year <- analytic_base |>
  dplyr::mutate(calendar_year = lubridate::year(event_date)) |>
  status_by_strata("calendar_year")

by_age <- analytic_base |>
  dplyr::mutate(age_group = age_group_of(age_years)) |>
  status_by_strata("age_group")

by_sex <- status_by_strata(analytic_base, "sex")
by_state <- status_by_strata(analytic_base, "uf_residence")
by_profile <- status_by_strata(analytic_base, "core_profile")

mortality_status_by_year_profile <- dplyr::bind_rows(by_year, by_age, by_sex, by_state, by_profile)
readr::write_csv(mortality_status_by_year_profile, file.path(val_dir, "mortality_status_by_year_profile.csv"))
cat(sprintf("\n[Part A.3/A.4] Saved stratified status summary: %s (%d rows)\n",
            file.path(val_dir, "mortality_status_by_year_profile.csv"), nrow(mortality_status_by_year_profile)))

## ---- A.5: is unresolved status associated with age/year/state/profile? ----
## Lightweight chi-square association test per stratifier (descriptive audit,
## not a causal claim) - unresolved = evolution %in% unknown/death_under_investigation/NA.

analytic_base <- analytic_base |>
  dplyr::mutate(
    unresolved = evolution %in% c("unknown", "death_under_investigation") | is.na(evolution),
    calendar_year = lubridate::year(event_date),
    age_group = age_group_of(age_years)
  )

chisq_assoc <- function(var) {
  tab <- table(analytic_base[[var]], analytic_base$unresolved)
  test <- tryCatch(suppressWarnings(chisq.test(tab)), error = function(e) NULL)
  tibble::tibble(
    stratifier = var,
    chisq_statistic = if (!is.null(test)) unname(test$statistic) else NA_real_,
    df = if (!is.null(test)) unname(test$parameter) else NA_real_,
    p_value = if (!is.null(test)) test$p.value else NA_real_
  )
}

association_report <- dplyr::bind_rows(
  chisq_assoc("calendar_year"), chisq_assoc("age_group"),
  chisq_assoc("sex"), chisq_assoc("uf_residence"), chisq_assoc("core_profile")
)
cat("\n[Part A.5] Association between UNRESOLVED mortality status and each stratifier (chi-square):\n")
print(as.data.frame(association_report), row.names = FALSE, digits = 4)

unresolved_rate_by_profile <- analytic_base |>
  dplyr::group_by(core_profile) |>
  dplyr::summarise(n = dplyr::n(), n_unresolved = sum(unresolved), pct_unresolved = round(100 * mean(unresolved), 3), .groups = "drop")
cat("\n[Part A.5] % unresolved by core_profile (is the bug's impact spread evenly across profiles?):\n")
print(as.data.frame(unresolved_rate_by_profile), row.names = FALSE)

## ---- A master audit CSV (item 2 + item 4 combined, overall) ----

mortality_status_audit <- analytic_base |>
  dplyr::mutate(evolution_cat = dplyr::coalesce(as.character(evolution), "NA_blank_or_unmapped")) |>
  dplyr::count(evolution_cat, died_from_chik_current = died_from_chik, name = "n") |>
  dplyr::mutate(
    pct_of_cohort = round(100 * n / nrow(analytic_base), 4),
    corrected_classification = dplyr::case_when(
      evolution_cat == "cured" ~ "FALSE (non-event)",
      evolution_cat == "death_chik" ~ "TRUE (chik death)",
      evolution_cat == "death_other" ~ "FALSE (non-event; competing risk)",
      evolution_cat %in% c("unknown", "death_under_investigation") ~ "NA (excluded)",
      evolution_cat == "NA_blank_or_unmapped" ~ "NA (excluded)"
    ),
    currently_misclassified = (died_from_chik_current == FALSE) &
      (corrected_classification == "NA (excluded)")
  )
readr::write_csv(mortality_status_audit, file.path(val_dir, "mortality_status_audit.csv"))
cat(sprintf("\n[Part A] Saved: %s\n", file.path(val_dir, "mortality_status_audit.csv")))

## =============================================================================
## PHASE B -- CORRECT OUTCOME CLASSIFICATION
## =============================================================================
## Operates on the ALREADY-CORRECT `evolution` label column (recode() in
## 02_clean_chik_sinan_brazil.R already maps raw EVOLUCAO to these 5 labels
## and already sends any blank/unmapped raw code to NA - verified in Part A.1
## above: the only non-dictionary code found, "0" (130 records), has no
## entry in map_evolucao and so already becomes NA in `evolution`). The bug
## was only ever in how the boolean was derived FROM this correct label.

classify_died_from_chik <- function(evolution_label) {
  dplyr::case_when(
    evolution_label == "cured"                      ~ FALSE,
    evolution_label == "death_chik"                  ~ TRUE,
    evolution_label == "death_other"                 ~ FALSE,
    evolution_label == "death_under_investigation"    ~ NA,
    evolution_label == "unknown"                      ~ NA,
    is.na(evolution_label)                            ~ NA,
    TRUE                                               ~ NA  # defensive: any future unmapped label
  )
}

## ---- Unit tests: every status value, including NA and an unmapped label ----

test_cases <- tibble::tibble(
  evolution_label = c("cured", "death_chik", "death_other",
                       "death_under_investigation", "unknown", NA_character_,
                       "some_future_unmapped_label"),
  expected = c(FALSE, TRUE, FALSE, NA, NA, NA, NA)
)
test_cases$actual <- classify_died_from_chik(test_cases$evolution_label)
test_cases$pass <- (is.na(test_cases$expected) & is.na(test_cases$actual)) |
  (!is.na(test_cases$expected) & !is.na(test_cases$actual) & test_cases$expected == test_cases$actual)

cat("\n[Part B] Unit tests - classify_died_from_chik():\n")
print(as.data.frame(test_cases), row.names = FALSE)
if (!all(test_cases$pass)) stop("[Part B] UNIT TEST FAILURE in classify_died_from_chik() - see table above.")
cat("[Part B] All", nrow(test_cases), "unit tests PASSED.\n")

## ---- End-to-end unit test: raw EVOLUCAO code -> map_evolucao label ->
## classify_died_from_chik(), replicating 02_'s exact recode() logic,
## including a synthetic unmapped raw code and a blank raw code. ----

map_evolucao <- c(`1` = "cured", `2` = "death_chik", `3` = "death_other",
                   `4` = "death_under_investigation", `9` = "unknown")
recode_like_02 <- function(x, map) {
  out <- unname(map[as.character(x)])
  out[x == "" | is.na(x)] <- NA_character_
  out
}

e2e_cases <- tibble::tibble(
  raw_evolucao = c("1", "2", "3", "4", "9", "", NA_character_, "0", "7"),
  expected_died_from_chik = c(FALSE, TRUE, FALSE, NA, NA, NA, NA, NA, NA)
)
e2e_cases$label <- recode_like_02(e2e_cases$raw_evolucao, map_evolucao)
e2e_cases$actual_died_from_chik <- classify_died_from_chik(e2e_cases$label)
e2e_cases$pass <- (is.na(e2e_cases$expected_died_from_chik) & is.na(e2e_cases$actual_died_from_chik)) |
  (!is.na(e2e_cases$expected_died_from_chik) & !is.na(e2e_cases$actual_died_from_chik) &
     e2e_cases$expected_died_from_chik == e2e_cases$actual_died_from_chik)

cat("\n[Part B] End-to-end unit tests - raw EVOLUCAO -> label -> classify_died_from_chik():\n")
print(as.data.frame(e2e_cases), row.names = FALSE)
if (!all(e2e_cases$pass)) stop("[Part B] END-TO-END UNIT TEST FAILURE - see table above.")
cat("[Part B] All", nrow(e2e_cases), "end-to-end unit tests PASSED (raw codes '0' and '7' - neither in the\n",
    "official dictionary - both correctly resolve to NA, not a confirmed non-event).\n")

## ---- Apply the corrected classification to the analytic population ----
## Hospitalisation cohort/outcome is UNTOUCHED by this script - only
## died_from_chik_corrected is added, nothing about `hospitalised`/
## `hosp_only` is read or modified anywhere below.

analytic_base <- analytic_base |>
  dplyr::mutate(died_from_chik_corrected = classify_died_from_chik(evolution))

corrected_death_cohort <- analytic_base |> dplyr::filter(!is.na(died_from_chik_corrected))
corrected_n <- nrow(corrected_death_cohort)
corrected_deaths <- sum(corrected_death_cohort$died_from_chik_corrected)

cat(sprintf(
  "\n[Part B] CORRECTED death_cohort N = %s (vs original %s, difference = %s excluded records)\n",
  format(corrected_n, big.mark = ","), format(baseline_n, big.mark = ","),
  format(baseline_n - corrected_n, big.mark = ",")
))
cat(sprintf("[Part B] CORRECTED deaths = %d (vs original %d - unchanged, as expected: no confirmed chik death is reclassified)\n",
            corrected_deaths, baseline_deaths))
stopifnot(corrected_deaths == baseline_deaths)  # sanity: correction only removes ambiguous non-events, never a confirmed death

## =============================================================================
## PHASE C -- RE-ESTIMATE MORTALITY RISKS
## (self-contained copies of 12_'s exact model structure/standardisation
## code, parameterised so the SAME functions run every scenario in Phase D.
## 12_core3_condition_specific_rr.R itself is never sourced or modified -
## this avoids re-triggering its own costly caches/side effects, and
## guarantees this validation cannot silently reuse a stale bootstrap.)
## =============================================================================

fit_core3_death_model <- function(data) {
  mgcv::bam(
    death_only ~ s(age_years, k = 10, bs = "ts") + sex + year + s(uf_residence, bs = "re") + dm + htn + ckd,
    data = data, family = binomial(link = "logit"), method = "fREML", discrete = TRUE, na.action = na.exclude
  )
}

fit_profile_death_model <- function(data) {
  mgcv::gam(
    death_only ~ core_profile + sex + year + s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts"),
    data = data, family = binomial(link = "logit"), method = "REML", na.action = na.exclude
  )
}

get_adjusted_or_local <- function(fit, exposure_var) {
  est <- coef(fit)[[exposure_var]]
  se <- sqrt(diag(vcov(fit, unconditional = TRUE)))[[exposure_var]]
  tibble::tibble(condition = exposure_var, or = exp(est), or_lower = exp(est - 1.96 * se), or_upper = exp(est + 1.96 * se))
}

standardised_rr_local <- function(fit, data, exposure_var, B = 2000, seed = 1, draw_block_size = 100) {
  combo <- data |> dplyr::count(age_years, sex, year, uf_residence, dm, htn, ckd, name = "n")
  combo_exposed <- combo; combo_unexposed <- combo
  combo_exposed[[exposure_var]] <- 1L; combo_unexposed[[exposure_var]] <- 0L

  X1 <- predict(fit, newdata = combo_exposed, type = "lpmatrix")
  X0 <- predict(fit, newdata = combo_unexposed, type = "lpmatrix")
  w <- combo$n
  beta_hat <- coef(fit)
  p1_point <- plogis(as.numeric(X1 %*% beta_hat)); p0_point <- plogis(as.numeric(X0 %*% beta_hat))
  risk_exposed_point <- sum(p1_point * w) / sum(w); risk_unexposed_point <- sum(p0_point * w) / sum(w)
  rr_point <- risk_exposed_point / risk_unexposed_point

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov(fit, unconditional = TRUE))
  risk_exposed_draws <- numeric(B); risk_unexposed_draws <- numeric(B); weight_sum <- sum(w)
  for (first_draw in seq.int(1, B, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1, B)
    beta_block <- t(beta_draws[draw_index, , drop = FALSE])
    p1_block <- plogis(X1 %*% beta_block)
    risk_exposed_draws[draw_index] <- as.numeric(crossprod(w, p1_block)) / weight_sum
    p0_block <- plogis(X0 %*% beta_block)
    risk_unexposed_draws[draw_index] <- as.numeric(crossprod(w, p0_block)) / weight_sum
  }
  rr_draws <- risk_exposed_draws / risk_unexposed_draws

  tibble::tibble(
    condition = exposure_var, rr = rr_point,
    rr_lower = stats::quantile(rr_draws, 0.025, names = FALSE),
    rr_upper = stats::quantile(rr_draws, 0.975, names = FALSE),
    standardised_risk_exposed = risk_exposed_point,
    standardised_risk_unexposed = risk_unexposed_point
  )
}

get_crude_counts_local <- function(data, exposure_var, outcome_var) {
  exposed <- data[[exposure_var]] == 1; outcome <- data[[outcome_var]]
  tibble::tibble(condition = exposure_var, n_exposed = sum(exposed), n_events_exposed = sum(outcome[exposed], na.rm = TRUE),
                 n_unexposed = sum(!exposed), n_events_unexposed = sum(outcome[!exposed], na.rm = TRUE))
}

build_core3_summary_local <- function(fit, data, outcome_var, B = 2000) {
  purrr::map_dfr(c("dm", "htn", "ckd"), function(ev) {
    get_adjusted_or_local(fit, ev) |>
      dplyr::left_join(standardised_rr_local(fit, data, ev, B = B), by = "condition") |>
      dplyr::left_join(get_crude_counts_local(data, ev, outcome_var), by = "condition")
  }) |>
    dplyr::mutate(condition = dplyr::recode(condition, dm = "Diabetes", htn = "Hypertension", ckd = "Chronic kidney disease"))
}

weighted_risk_draws_local <- function(X, beta_draws, w, draw_block_size = 500) {
  draw_count <- nrow(beta_draws); output <- numeric(draw_count)
  for (first_draw in seq.int(1, draw_count, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1, draw_count)
    p_draws_block <- plogis(X %*% t(beta_draws[draw_index, , drop = FALSE]))
    output[draw_index] <- colSums(p_draws_block * w) / sum(w)
  }
  output
}

standardise_profile_rr_local <- function(fit, data, B = 2000, seed = 1) {
  profile_levels <- levels(data$core_profile)
  combo <- data |> dplyr::count(age_years, sex, year, uf_residence, name = "n")
  w <- combo$n
  beta_hat <- coef(fit)
  set.seed(seed)
  beta_draws <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov(fit, unconditional = TRUE))
  risk_point <- setNames(numeric(length(profile_levels)), profile_levels)
  risk_draws <- matrix(NA_real_, nrow = B, ncol = length(profile_levels), dimnames = list(NULL, profile_levels))
  for (p in profile_levels) {
    combo_p <- combo; combo_p$core_profile <- factor(p, levels = profile_levels)
    X_p <- predict(fit, newdata = combo_p, type = "lpmatrix")
    risk_point[p] <- sum(plogis(as.numeric(X_p %*% beta_hat)) * w) / sum(w)
    risk_draws[, p] <- weighted_risk_draws_local(X_p, beta_draws, w)
  }
  rr_point <- risk_point / risk_point[["None"]]
  rr_draws <- sweep(risk_draws, 1, risk_draws[, "None"], FUN = "/")
  tibble::tibble(
    core_profile = factor(profile_levels, levels = profile_levels),
    standardised_risk = as.numeric(risk_point), rr = as.numeric(rr_point),
    rr_lower = apply(rr_draws, 2, stats::quantile, probs = 0.025, names = FALSE),
    rr_upper = apply(rr_draws, 2, stats::quantile, probs = 0.975, names = FALSE)
  )
}

run_mortality_scenario <- function(scenario_name, cohort_data, death_col, B_core3, B_profile, weight_col = NULL) {
  cat(sprintf("\n[Part C/D] ---- Scenario %s: fitting core3 + profile death models (N=%s, B_core3=%d, B_profile=%d) ----\n",
              scenario_name, format(nrow(cohort_data), big.mark = ","), B_core3, B_profile))
  cohort_data$death_only <- cohort_data[[death_col]]

  fit_core3 <- if (is.null(weight_col)) {
    fit_core3_death_model(cohort_data)
  } else {
    mgcv::bam(death_only ~ s(age_years, k = 10, bs = "ts") + sex + year + s(uf_residence, bs = "re") + dm + htn + ckd,
              data = cohort_data, family = binomial(link = "logit"), method = "fREML", discrete = TRUE,
              na.action = na.exclude, weights = cohort_data[[weight_col]])
  }
  fit_profile <- if (is.null(weight_col)) {
    fit_profile_death_model(cohort_data)
  } else {
    mgcv::gam(death_only ~ core_profile + sex + year + s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts"),
              data = cohort_data, family = binomial(link = "logit"), method = "REML", na.action = na.exclude,
              weights = cohort_data[[weight_col]])
  }

  core3_summary <- build_core3_summary_local(fit_core3, cohort_data, "death_only", B = B_core3)
  profile_summary <- standardise_profile_rr_local(fit_profile, cohort_data, B = B_profile)

  n_events <- if (is.null(weight_col)) sum(cohort_data$death_only, na.rm = TRUE) else sum(cohort_data$death_only * cohort_data[[weight_col]], na.rm = TRUE)
  list(scenario = scenario_name, n = nrow(cohort_data), n_events = n_events,
       core3 = core3_summary |> dplyr::mutate(scenario = scenario_name, .before = 1),
       profile = profile_summary |> dplyr::mutate(scenario = scenario_name, .before = 1))
}

## ---- Scenario A: corrected primary definition (full bootstrap, B=2000 to
## match the original's precision for the key comparison) ----

scenario_A <- run_mortality_scenario("A_corrected", corrected_death_cohort, "died_from_chik_corrected", B_core3 = 2000, B_profile = 2000)

## ---- Scenario B: legacy definition - REUSE the existing cached results,
## no refit (identical to what the manuscript currently reports) ----

load("01_Data/core3_rr_death.RData")      # core3_rr_death
load("01_Data/core_profile_rr_death.RData") # core_profile_rr_death

scenario_B <- list(
  scenario = "B_legacy", n = baseline_n, n_events = baseline_deaths,
  core3 = core3_rr_death |> dplyr::mutate(scenario = "B_legacy", .before = 1),
  profile = core_profile_rr_death |> dplyr::rename(core_profile = core_profile) |>
    dplyr::mutate(scenario = "B_legacy", .before = 1)
)
cat(sprintf("\n[Part C/D] Scenario B_legacy: reusing cached 01_Data/core3_rr_death.RData + core_profile_rr_death.RData (N=%s, deaths=%d) - no refit.\n",
            format(baseline_n, big.mark = ","), baseline_deaths))

## =============================================================================
## PHASE D -- SENSITIVITY ANALYSES
## C: a plausible, EXPLICITLY JUSTIFIED imputation - but ONLY for the n=142
## "death under investigation" (EVOLUCAO=4) records. Unlike "unknown"/blank
## (who are not even confirmed to have died), every EVOLUCAO=4 record has a
## recorded death date (verified in Part A.1's DT_OBITO cross-check) - these
## people definitely died, only the ATTRIBUTION to chikungunya vs another
## cause is pending. The plausible assumption: investigation-pending deaths
## resolve to chikungunya-attribution in the same proportion as deaths that
## HAVE already been resolved (evolution %in% c("death_chik","death_other")).
## Implemented as a soft-labelled prior weight (mgcv binomial weights),
## NOT as a hard imputed 0/1 label, so the 142 records contribute partial
## evidence in both directions rather than a fabricated certain outcome.
## "unknown"/blank remain excluded (NA), same as scenario A, because there is
## no analogous resolved-outcome reference population to justify imputing a
## chikungunya-death probability for people not even confirmed to have died.
## =============================================================================

n_death_other <- sum(analytic_base$evolution == "death_other", na.rm = TRUE)
p_chik_given_death <- baseline_deaths / (baseline_deaths + n_death_other)
cat(sprintf(
  "\n[Part D] Scenario C justification: P(chik-attributed | resolved death) = %d / (%d+%d) = %.4f, applied to the %d investigation-pending deaths as a soft prior weight.\n",
  baseline_deaths, baseline_deaths, n_death_other, p_chik_given_death, sum(analytic_base$evolution == "death_under_investigation", na.rm = TRUE)
))

investigation_pending <- analytic_base |> dplyr::filter(evolution == "death_under_investigation")

build_soft_labelled_cohort <- function(p_true) {
  pos <- investigation_pending |> dplyr::mutate(death_only_soft = TRUE, soft_weight = p_true)
  neg <- investigation_pending |> dplyr::mutate(death_only_soft = FALSE, soft_weight = 1 - p_true)
  base_soft <- corrected_death_cohort |> dplyr::mutate(death_only_soft = died_from_chik_corrected, soft_weight = 1)
  dplyr::bind_rows(base_soft, pos, neg) |> dplyr::filter(soft_weight > 0)
}

scenario_C_data <- build_soft_labelled_cohort(p_chik_given_death)
scenario_C <- run_mortality_scenario("C_plausible_weighted", scenario_C_data, "death_only_soft",
                                      B_core3 = 500, B_profile = 500, weight_col = "soft_weight")

## D: extreme-case bound - ALL 142 investigation-pending deaths treated as
## chikungunya-attributed. This is NOT a plausible epidemiological estimate
## (it assumes the medical investigation would resolve unanimously in favour
## of chikungunya, for which there is no evidential basis) - included only
## as a mathematical stress test of how far the RR estimates COULD move if
## every single pending case were attributed to chikungunya.

scenario_D_data <- build_soft_labelled_cohort(1.0) |> dplyr::filter(soft_weight > 0)
scenario_D <- run_mortality_scenario("D_STRESS_TEST_all_investigation_as_chik_death", scenario_D_data, "death_only_soft",
                                      B_core3 = 500, B_profile = 500, weight_col = "soft_weight")

cat("\n[Part D] NOTE: Scenario D is a mathematical stress test, not a plausible epidemiological assumption (see VALIDATION_3_1.md).\n")

## =============================================================================
## PHASE E -- DELIVERABLES: before/after comparison table + figure + report
## =============================================================================

all_scenarios <- list(scenario_B, scenario_A, scenario_C, scenario_D)  # B first = "before"

core3_all <- dplyr::bind_rows(lapply(all_scenarios, `[[`, "core3"))
profile_all <- dplyr::bind_rows(lapply(all_scenarios, `[[`, "profile"))

## ---- before/after absolute + percentage change, vs the legacy baseline ----

core3_before_after <- core3_all |>
  dplyr::filter(scenario %in% c("B_legacy", "A_corrected")) |>
  dplyr::select(scenario, condition, rr, rr_lower, rr_upper) |>
  tidyr::pivot_wider(names_from = scenario, values_from = c(rr, rr_lower, rr_upper)) |>
  dplyr::mutate(
    level = "condition_specific",
    rr_abs_change = rr_A_corrected - rr_B_legacy,
    rr_pct_change = round(100 * (rr_A_corrected - rr_B_legacy) / rr_B_legacy, 2)
  )

profile_before_after <- profile_all |>
  dplyr::filter(scenario %in% c("B_legacy", "A_corrected")) |>
  dplyr::select(scenario, core_profile, rr, rr_lower, rr_upper) |>
  tidyr::pivot_wider(names_from = scenario, values_from = c(rr, rr_lower, rr_upper)) |>
  dplyr::mutate(
    level = "joint_profile",
    rr_abs_change = rr_A_corrected - rr_B_legacy,
    rr_pct_change = round(100 * (rr_A_corrected - rr_B_legacy) / rr_B_legacy, 2)
  )

## Profile ranking check (by RR, descending) - before vs after.
rank_before <- profile_all |> dplyr::filter(scenario == "B_legacy", core_profile != "None") |>
  dplyr::arrange(dplyr::desc(rr)) |> dplyr::mutate(rank_before = dplyr::row_number()) |>
  dplyr::select(core_profile, rank_before)
rank_after <- profile_all |> dplyr::filter(scenario == "A_corrected", core_profile != "None") |>
  dplyr::arrange(dplyr::desc(rr)) |> dplyr::mutate(rank_after = dplyr::row_number()) |>
  dplyr::select(core_profile, rank_after)
rank_comparison <- rank_before |> dplyr::left_join(rank_after, by = "core_profile") |>
  dplyr::mutate(rank_changed = rank_before != rank_after)

cat("\n[Part C] Profile RR ranking, legacy (before) vs corrected (after):\n")
print(as.data.frame(rank_comparison), row.names = FALSE)

mortality_rr_before_after <- dplyr::bind_rows(
  core3_before_after |> dplyr::select(level, label = condition, dplyr::everything()),
  profile_before_after |> dplyr::mutate(core_profile = as.character(core_profile)) |>
    dplyr::select(level, label = core_profile, dplyr::everything())
)
readr::write_csv(mortality_rr_before_after, file.path(val_dir, "mortality_rr_before_after.csv"))
cat(sprintf("\n[Part E] Saved: %s\n", file.path(val_dir, "mortality_rr_before_after.csv")))

## Full 4-scenario table too (sensitivity results), saved alongside for completeness.
readr::write_csv(core3_all, file.path(val_dir, "mortality_rr_core3_all_scenarios.csv"))
readr::write_csv(profile_all, file.path(val_dir, "mortality_rr_profile_all_scenarios.csv"))

## ---- Comparison figure: RR + 95% CI, all 4 scenarios, core3 + profile ----

plot_data_core3 <- core3_all |>
  dplyr::mutate(label = condition, panel = "Condition-specific (mutually adjusted)")
plot_data_profile <- profile_all |>
  dplyr::filter(core_profile != "None") |>
  dplyr::mutate(label = as.character(core_profile), panel = "Joint profile (vs None)")

plot_data <- dplyr::bind_rows(
  plot_data_core3 |> dplyr::select(scenario, label, rr, rr_lower, rr_upper, panel),
  plot_data_profile |> dplyr::select(scenario, label, rr, rr_lower, rr_upper, panel)
) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = c("B_legacy", "A_corrected", "C_plausible_weighted", "D_STRESS_TEST_all_investigation_as_chik_death"),
                       labels = c("B: legacy (original)", "A: corrected (primary)", "C: plausible (investigation-weighted)", "D: stress test (all investigation = chik death)")),
    panel = factor(panel, levels = c("Condition-specific (mutually adjusted)", "Joint profile (vs None)"))
  )

scenario_colours <- c(
  "B: legacy (original)" = "#999999", "A: corrected (primary)" = "#C44E52",
  "C: plausible (investigation-weighted)" = "#4C72B0", "D: stress test (all investigation = chik death)" = "#DD8452"
)

fig_mortality_comparison <- ggplot2::ggplot(plot_data, ggplot2::aes(x = rr, y = label, colour = scenario)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#999999", linewidth = 0.3) +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = rr_lower, xmax = rr_upper), position = ggplot2::position_dodge(width = 0.6), size = 0.35, fatten = 2) +
  ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 1) +
  ggplot2::scale_colour_manual(values = scenario_colours) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Mortality RR before vs after chikungunya-death outcome correction",
    subtitle = "Gate 3.1: EVOLUCAO=4/9/blank excluded as unresolved (A), vs legacy treatment as non-events (B)",
    x = "Standardised RR vs reference (log scale)", y = NULL, colour = NULL
  ) +
  ggplot2::theme_classic(base_size = 10, base_family = "Arial") +
  ggplot2::theme(
    legend.position = "bottom", legend.direction = "vertical",
    strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold", size = 11),
    axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333")
  )

ggplot2::ggsave(file.path(val_dir, "mortality_rr_comparison.png"), fig_mortality_comparison,
                 width = 210, height = 190, units = "mm", dpi = 300, bg = "white")
cat(sprintf("[Part E] Saved: %s\n", file.path(val_dir, "mortality_rr_comparison.png")))

## ---- console summary used to write VALIDATION_3_1.md ----

cat("\n\n================ SUMMARY FOR VALIDATION_3_1.md ================\n")
cat(sprintf("Baseline (legacy) death_cohort N=%s, deaths=%d\n", format(baseline_n, big.mark=","), baseline_deaths))
cat(sprintf("Corrected death_cohort N=%s, deaths=%d (excluded %s records)\n",
            format(corrected_n, big.mark=","), corrected_deaths, format(baseline_n - corrected_n, big.mark=",")))
cat("\ncore3_before_after:\n"); print(as.data.frame(core3_before_after))
cat("\nprofile_before_after:\n"); print(as.data.frame(profile_before_after))
cat("\nrank_comparison:\n"); print(as.data.frame(rank_comparison))

saveRDS(list(
  baseline_n = baseline_n, baseline_deaths = baseline_deaths,
  corrected_n = corrected_n, corrected_deaths = corrected_deaths,
  n_misclassified_total = n_misclassified_total,
  core3_before_after = core3_before_after, profile_before_after = profile_before_after,
  rank_comparison = rank_comparison, association_report = association_report,
  unresolved_rate_by_profile = unresolved_rate_by_profile,
  raw_evolucao_tab = raw_evolucao_tab, n_unmapped_nonblank = n_unmapped_nonblank
), file.path(val_dir, "gate31_results_for_report.rds"))

cat("\n[38] Gate 3.1 computations DONE. Next: write VALIDATION_3_1.md from gate31_results_for_report.rds.\n")
