# ---------------------------------------------------------------------------
# 61_supplementary_age_model_selection.R
#
# Supplementary Tables Sx / Sx+1: comparison of alternative age
# specifications for hospitalisation and chikungunya-attributed death, and
# observed death counts by profile and age group.
#
# No model is refitted. Every value is read from the validated outputs of
#   52_hospital_age_validation.R      (03_Output/figure_revision/figure3_final/)
#   46_figure5_age_profile_validation.R (03_Output/validation/figure5_age_validation/)
# and cross-checked against the harmonised primary-model pipeline
#   62_primary_models_fit_standardise.R (03_Output/primary_models_H2_H0_final/).
# The script stops if any verification fails.
#
# Adult age-interaction test (identical in scripts 46 and 52): joint
# penalisation-aware Wald test, using the Bayesian covariance Vp, that each
# profile's log-odds ratio vs None is equal at ages 20, 30, ..., 90 and at
# reference age 60 (7 profiles jointly; eigen-truncated covariance).
#
# Output: 03_Output/supplementary_age_model_selection/
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(flextable)
  library(officer)
})

out_dir <- "03_Output/supplementary_age_model_selection"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
h_dir <- "03_Output/figure_revision/figure3_final"
m_dir <- "03_Output/validation/figure5_age_validation"
p_dir <- "03_Output/primary_models_H2_H0_final"
src <- list(
  h_cmp = file.path(h_dir, "hospital_age_model_comparison.csv"),
  h_diag = file.path(h_dir, "hospital_age_interaction_diagnostics.csv"),
  h_wald = file.path(h_dir, "hospital_age_wald_tests_by_profile.csv"),
  h_recon = file.path(h_dir, "cohort_reconciliation.csv"),
  m_diag = file.path(m_dir, "age_interaction_diagnostics.csv"),
  m_wald = file.path(m_dir, "age_interaction_wald_tests_by_profile.csv"),
  m_sup = file.path(m_dir, "age_profile_support.csv"),
  m_cohort = file.path(m_dir, "mortality_cohort_check.csv"),
  p_verif = file.path(p_dir, "validation", "primary_model_verification.csv"),
  p_sup = file.path(p_dir, "supplementary", "age_profile_support.csv")
)
stopifnot(all(file.exists(unlist(src))))
rd <- function(k) readr::read_csv(src[[k]], show_col_types = FALSE)
checks <- list()
chk <- function(check, pass, detail = "") {
  checks[[length(checks) + 1]] <<- tibble::tibble(check = check, pass = isTRUE(pass), detail = as.character(detail))
}

## ============================================================
## 1. Hospitalisation (script 52)
## ============================================================
h_cmp <- rd("h_cmp"); h_diag <- rd("h_diag"); h_wald <- rd("h_wald")
h_adult <- h_wald |> filter(test == "adult_20_90", core_profile == "All 7 profiles jointly") |>
  transmute(model, adult_wald_chisq = wald_chisq, adult_wald_df = wald_df, adult_wald_p = wald_p)
h_full <- h_wald |> filter(test == "full_1_95", core_profile == "All 7 profiles jointly") |>
  transmute(model, full_1_95_wald_p = wald_p)
h_coef <- h_wald |> filter(test == "interaction coefficients = 0") |>
  transmute(model, coef_test_chisq = wald_chisq, coef_test_df = wald_df, coef_test_p = wald_p)
hosp <- h_diag |>
  transmute(outcome = "Hospitalisation", source_model_id = model, source_label = label, formula, smoothing,
            n, events, aic, total_edf, converged, n_warnings, warnings) |>
  left_join(select(h_cmp, model, delta_aic_source = delta_aic_vs_H0, lrt_deviance, lrt_df, lrt_p),
            by = c(source_model_id = "model")) |>
  left_join(h_adult, by = c(source_model_id = "model")) |>
  left_join(h_full, by = c(source_model_id = "model")) |>
  left_join(h_coef, by = c(source_model_id = "model")) |>
  mutate(aic_h0 = aic[source_model_id == "H0"], source_files = paste(basename(src$h_diag), basename(src$h_cmp),
                                                                    basename(src$h_wald), sep = "; "))

## ============================================================
## 2. Mortality (script 46)
## ============================================================
m_diag <- rd("m_diag"); m_wald <- rd("m_wald")
m_adult <- m_wald |> filter(age_range == "20-90", core_profile == "All 7 profiles jointly") |>
  transmute(model, adult_wald_chisq = wald_chisq, adult_wald_df = wald_df, adult_wald_p = wald_p)
m_full <- m_wald |> filter(age_range == "1-95", core_profile == "All 7 profiles jointly") |>
  transmute(model, full_1_95_wald_p = wald_p)
m_coef <- m_wald |> filter(age_range == "linear slope") |>
  transmute(model, coef_test_chisq = wald_chisq, coef_test_df = wald_df, coef_test_p = wald_p)
mort <- m_diag |>
  transmute(outcome = "Chikungunya-attributed death", source_model_id = model, source_label = model_label, formula,
            smoothing = ifelse(grepl("sp_x", model), "fixed", "estimated (fREML)"),
            n, events = deaths, aic, total_edf, converged, n_warnings, warnings,
            delta_aic_source = delta_aic_vs_no_interaction, lrt_deviance, lrt_df, lrt_p) |>
  left_join(m_adult, by = c(source_model_id = "model")) |>
  left_join(m_full, by = c(source_model_id = "model")) |>
  left_join(m_coef, by = c(source_model_id = "model")) |>
  mutate(aic_h0 = aic[source_model_id == "no_interaction"],
         source_files = paste(basename(src$m_diag), basename(src$m_wald), sep = "; "))

## ============================================================
## 3. Harmonised labels, selection status, main-table membership
## ============================================================
## Mortality factor-smooth models share the hospitalisation H1/H2/H3 structures.
lab <- tibble::tribble(
  ~outcome, ~source_model_id, ~table_label, ~age_spec, ~status, ~order,
  "Hospitalisation", "H0", "H0", "Common age smooth; no profile-by-age interaction", "Reference", 1,
  "Hospitalisation", "H0_k20", "H0 (k = 20)", "More flexible common age smooth; no interaction", "Sensitivity", 2,
  "Hospitalisation", "H1", "H1", "Separate age smooth for each profile", "Alternative", 3,
  "Hospitalisation", "H2", "H2", "Common age smooth + penalised profile-specific deviations", "Selected", 6,
  "Hospitalisation", "H2_k15", "H2 (k = 15)", "As H2, larger age basis", "Sensitivity", 7,
  "Hospitalisation", "H3", "H3", "Common age smooth + age-group × profile random effects", "Alternative", 8,
  "Hospitalisation", "linear", "Linear", "Common age smooth + profile-specific linear age slopes", "Alternative", 9,
  "Chikungunya-attributed death", "no_interaction", "H0", "Common age smooth; no profile-by-age interaction", "Selected", 1,
  "Chikungunya-attributed death", "no_interaction_k20", "H0 (k = 20)", "More flexible common age smooth; no interaction", "Sensitivity", 2,
  "Chikungunya-attributed death", "fs_original", "H1", "Separate age smooth for each profile", "Alternative", 3,
  "Chikungunya-attributed death", "fs_k5", "H1 (k = 5)", "As H1, smaller age basis", "Sensitivity", 4,
  "Chikungunya-attributed death", "fs_k15", "H1 (k = 15)", "As H1, larger age basis", "Sensitivity", 5,
  "Chikungunya-attributed death", "fs_global", "H2", "Common age smooth + penalised profile-specific deviations", "Alternative", 6,
  "Chikungunya-attributed death", "band_re", "H3", "Common age smooth + age-group × profile random effects", "Alternative", 8,
  "Chikungunya-attributed death", "linear_interaction", "Linear", "Common age smooth + profile-specific linear age slopes", "Alternative", 9
)
audit <- bind_rows(hosp, mort) |>
  mutate(delta_aic = aic - aic_h0) |>
  left_join(lab, by = c("outcome", "source_model_id")) |>
  mutate(in_main_table = !is.na(table_label),
         status = ifelse(is.na(status), "Sensitivity (audit only)", status),
         adult_test_note = dplyr::case_when(
           !is.na(adult_wald_p) ~ "Joint Wald, ages 20-90 vs age 60, Vp",
           !is.na(coef_test_p) ~ "No age-grid test; coefficient test in coef_test_* (different hypothesis)",
           grepl("^H0$|^H0_k20$|^no_interaction", source_model_id) ~ "Not applicable (no interaction term)",
           TRUE ~ "Not available in source outputs"),
         cohort_note = ifelse(source_model_id == "fs_age15plus", "Different cohort (age >= 15); not comparable by AIC", ""),
         outcome = factor(outcome, levels = c("Hospitalisation", "Chikungunya-attributed death"))) |>
  arrange(outcome, !in_main_table, order, source_model_id)

## ============================================================
## 4. Mortality event support (Table Sx+1)
## ============================================================
profiles <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD")
ages <- c("0-14", "15-39", "40-59", "60-74", "75+")
m_sup <- rd("m_sup") |> filter(age_scheme == "primary")
support <- m_sup |>
  mutate(core_profile = factor(core_profile, levels = profiles), age_group = factor(age_group, levels = ages)) |>
  arrange(core_profile, age_group) |>
  group_by(core_profile) |>
  summarise(total_cases = sum(n_cases), total_deaths = sum(n_deaths),
            deaths_by_age = paste(n_deaths, collapse = " / "),
            age_groups_ge10 = sum(n_deaths >= 10), age_groups_1_9 = sum(n_deaths >= 1 & n_deaths < 10),
            age_groups_0 = sum(n_deaths == 0), .groups = "drop") |>
  mutate(core_profile = as.character(core_profile))
nonref <- m_sup |> filter(core_profile != "None")
strata <- c(cells = nrow(nonref), ge10 = sum(nonref$n_deaths >= 10),
            s1_9 = sum(nonref$n_deaths >= 1 & nonref$n_deaths < 10), zero = sum(nonref$n_deaths == 0))

## ============================================================
## 5. Verification (stop on failure)
## ============================================================
main <- filter(audit, in_main_table)
chk("All 15 main-table models found in sources", nrow(main) == 15 && !anyNA(main$aic), nrow(main))
chk("Hospitalisation candidates share cohort N = 776,072 / 25,363",
    all(main$n[main$outcome == "Hospitalisation"] == 776072 & main$events[main$outcome == "Hospitalisation"] == 25363))
chk("Mortality candidates share cohort N = 983,947 / 1,050",
    all(main$n[main$outcome != "Hospitalisation"] == 983947 & main$events[main$outcome != "Hospitalisation"] == 1050))
chk("All main-table models converged", all(main$converged %in% TRUE))
chk("Same adjustment (sex, year, state RE) in every formula",
    all(grepl("sex", main$formula) & grepl("year", main$formula) & grepl("uf_residence", main$formula)))
chk("Recomputed ΔAIC equals stored ΔAIC", max(abs(audit$delta_aic - audit$delta_aic_source), na.rm = TRUE) < 1e-8)
hr <- rd("h_recon"); mc <- rd("m_cohort")
chk("Hospitalisation cohort reconciliation file passes (script 52)", all(hr$match, na.rm = TRUE))
chk("Mortality cohort check file: 983,947 / 1,050",
    mc$value[grepl("cohort N", mc$metric)] == 983947 && mc$value[grepl("deaths", mc$metric)] == 1050)
g <- function(o, id, col) audit[[col]][audit$outcome == o & audit$source_model_id == id]
H <- "Hospitalisation"; M <- "Chikungunya-attributed death"
chk("H2 hospitalisation ΔAIC ≈ −241", abs(g(H, "H2", "delta_aic") + 241.08) < 0.01, sprintf("%.2f", g(H, "H2", "delta_aic")))
chk("H2 hospitalisation adult Wald p < 0.001", g(H, "H2", "adult_wald_p") < 0.001, signif(g(H, "H2", "adult_wald_p"), 3))
chk("Hospitalisation adult interaction p < 0.001 in H1, H2 and all H1/H2 basis and smoothing variants",
    all(audit$adult_wald_p[audit$outcome == H & !is.na(audit$adult_wald_p)] < 0.001),
    paste(audit$source_model_id[audit$outcome == H & !is.na(audit$adult_wald_p)], collapse = ","))
chk("H0 (k = 20) improves AIC far less than interaction models (hosp)",
    g(H, "H0_k20", "delta_aic") > -50 && g(H, "H2", "delta_aic") < -200)
chk("Mortality fs_original and fs_global have lower AIC than H0",
    g(M, "fs_original", "delta_aic") < 0 && g(M, "fs_global", "delta_aic") < 0)
chk("Mortality fs_original and fs_global adult Wald p > 0.05",
    g(M, "fs_original", "adult_wald_p") > 0.05 && g(M, "fs_global", "adult_wald_p") > 0.05)
chk("Mortality fs_k5 retained in main table (worse AIC, adult p ≈ 0.001)",
    g(M, "fs_k5", "in_main_table") && g(M, "fs_k5", "delta_aic") > 0 && g(M, "fs_k5", "adult_wald_p") < 0.01)
chk("Wald p in diagnostics file = Wald p in test file (mortality)",
    max(abs(m_diag$joint_age_shape_wald_p - m_adult$adult_wald_p[match(m_diag$model, m_adult$model)]), na.rm = TRUE) < 1e-12)
chk("Wald p in comparison file = Wald p in test file (hospitalisation)",
    max(abs(h_cmp$joint_wald_p_adult_20_90 - h_adult$adult_wald_p[match(h_cmp$model, h_adult$model)]), na.rm = TRUE) < 1e-12)
anchors <- tibble::tribble(
  ~outcome, ~id, ~anchor_aic,
  H, "H0", 204543.15, H, "H0_k20", 204510.28, H, "H1", 204331.86, H, "H2", 204302.08, H, "H2_k15", 204284.26,
  H, "H3", 204365.28, H, "linear", 204466.89,
  M, "no_interaction", 13738.63, M, "no_interaction_k20", 13735.42, M, "fs_original", 13723.81,
  M, "fs_global", 13705.89, M, "fs_k5", 13758.51, M, "fs_k15", 13722.53, M, "linear_interaction", 13737.62,
  M, "band_re", 13735.34) |>
  mutate(source_aic = mapply(function(o, i) g(o, i, "aic"), outcome, id), diff = source_aic - anchor_aic)
chk("Task anchors match source AIC (|diff| < 0.01)", all(abs(anchors$diff) < 0.01),
    paste(anchors$id[abs(anchors$diff) >= 0.01], collapse = ","))
pv <- readr::read_csv(src$p_verif, show_col_types = FALSE)
chk("Primary H2 refit (script 62) reproduces script 52 H2 AIC",
    abs(pv$aic[pv$model_id == "hospitalisation_H2"] - g(H, "H2", "aic")) < 1e-3)
chk("Primary H0 refit (script 62) reproduces script 46 H0 AIC",
    abs(pv$aic[pv$model_id == "mortality_H0"] - g(M, "no_interaction", "aic")) < 1e-3)
chk("Total deaths in support file = 1,050; cases = 983,947",
    sum(support$total_deaths) == 1050 && sum(support$total_cases) == 983947)
chk("DM+CKD 4 deaths; CKD 12 deaths",
    support$total_deaths[support$core_profile == "DM+CKD"] == 4 && support$total_deaths[support$core_profile == "CKD"] == 12)
chk("35 non-reference strata: 12 with 1-9 deaths, 12 with 0 deaths",
    strata[["cells"]] == 35 && strata[["s1_9"]] == 12 && strata[["zero"]] == 12, paste(names(strata), strata, collapse = "; "))
ps <- readr::read_csv(src$p_sup, show_col_types = FALSE) |> filter(outcome == "death")
cmp_sup <- inner_join(ps, m_sup, by = c("age_group", "core_profile"))
chk("Death counts by age × profile identical to cohort rebuilt in script 62",
    nrow(cmp_sup) == 40 && all(cmp_sup$n_events == cmp_sup$n_deaths) && all(cmp_sup$n_cases.x == cmp_sup$n_cases.y))
checks <- bind_rows(checks)
readr::write_csv(checks, file.path(out_dir, "verification_checks.csv"))
readr::write_csv(anchors, file.path(out_dir, "anchor_reconciliation.csv"))
print(as.data.frame(checks), right = FALSE)
if (!all(checks$pass)) stop("Verification failed - see verification_checks.csv")

## ============================================================
## 6. Machine-readable outputs
## ============================================================
readr::write_csv(audit |> mutate(outcome = as.character(outcome)) |>
                   select(outcome, source_model_id, table_label, status, in_main_table, age_spec, source_label, formula,
                          smoothing, n, events, aic, delta_aic, total_edf, adult_wald_chisq, adult_wald_df, adult_wald_p,
                          adult_test_note, full_1_95_wald_p, coef_test_chisq, coef_test_df, coef_test_p,
                          lrt_deviance, lrt_df, lrt_p, converged, n_warnings, warnings, cohort_note, source_files),
                 file.path(out_dir, "Full_Model_Comparison_Audit.csv"), na = "")
fmt_p <- function(p) vapply(p, function(x) {
  if (is.na(x)) "—" else if (x < 0.001) "<0.001" else formatC(x, format = "f", digits = if (x < 0.01) 3 else 2)
}, character(1))
fmt_num <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
fmt_delta <- function(x) ifelse(abs(x) < 0.05, "0 (reference)", sub("^-", "−", ifelse(x > 0, paste0("+", fmt_num(x)), fmt_num(x))))
tab <- main |>
  transmute(outcome = as.character(outcome), model = table_label, age_spec, aic_text = fmt_num(aic),
            delta_text = fmt_delta(delta_aic), p_text = ifelse(is.na(adult_wald_p), "—", fmt_p(adult_wald_p)),
            status, aic, delta_aic, adult_wald_chisq, adult_wald_df, adult_wald_p, source_model_id)
readr::write_csv(tab, file.path(out_dir, "Supplementary_Table_Age_Model_Selection.csv"))
readr::write_csv(support |> rename(deaths_0_14_15_39_40_59_60_74_75plus = deaths_by_age),
                 file.path(out_dir, "Supplementary_Table_Mortality_Event_Support.csv"))

## ============================================================
## 7. Word tables
## ============================================================
font_family <- "Arial"
thin_rule <- fp_border(color = "#C9D1D5", width = 0.65)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
base_style <- function(ft) {
  ft |> bg(part = "header", bg = "#EEF1F3") |> bg(part = "body", bg = "#FFFFFF") |>
    color(part = "all", color = "#263238") |> font(part = "all", fontname = font_family) |>
    fontsize(part = "header", size = 8.5) |> fontsize(part = "body", size = 8.5) |>
    bold(part = "header", bold = TRUE) |> valign(part = "all", valign = "center") |>
    padding(part = "all", padding.top = 2.5, padding.bottom = 2.5, padding.left = 4, padding.right = 4)
}

## Table Sx: panel heading rows interleaved with model rows.
panel_rows <- function(o, heading) {
  d <- tab[tab$outcome == o, ]
  rbind(data.frame(model = heading, age_spec = "", aic_text = "", delta_text = "", p_text = "", status = "", sel = FALSE,
                   head = TRUE),
        data.frame(model = d$model, age_spec = d$age_spec, aic_text = d$aic_text, delta_text = d$delta_text,
                   p_text = d$p_text, status = d$status, sel = d$status == "Selected", head = FALSE))
}
t1 <- rbind(panel_rows("Hospitalisation", "A. Hospitalisation (N = 776,072; 25,363 hospitalisations)"),
            panel_rows("Chikungunya-attributed death", "B. Chikungunya-attributed death (N = 983,947; 1,050 deaths)"))
heads <- which(t1$head); sels <- which(t1$sel)
ft1 <- flextable(t1[, c("model", "age_spec", "aic_text", "delta_text", "p_text", "status")]) |>
  set_header_labels(model = "Model", age_spec = "Age specification", aic_text = "AIC", delta_text = "ΔAIC vs H0",
                    p_text = "Adult age-interaction test, p", status = "Status") |>
  base_style() |> border_remove() |>
  hline_top(part = "header", border = header_rule) |> hline_bottom(part = "header", border = header_rule) |>
  hline(i = setdiff(seq_len(nrow(t1)), c(heads, heads[2] - 1)), part = "body", border = thin_rule) |>
  hline(i = heads[2] - 1, part = "body", border = header_rule) |> hline_bottom(part = "body", border = header_rule)
for (i in heads) ft1 <- merge_at(ft1, i = i, j = 1:6, part = "body")
ft1 <- ft1 |> bold(i = heads, part = "body") |> bg(i = heads, part = "body", bg = "#F6F7F8") |>
  bold(i = sels, part = "body") |>
  align(j = 1:2, align = "left", part = "all") |> align(j = 3:6, align = "right", part = "all") |>
  align(j = 6, align = "left", part = "all") |> align(i = heads, align = "left", part = "body")
widths1 <- c(0.85, 2.75, 0.85, 0.95, 0.95, 0.8)
for (j in 1:6) ft1 <- width(ft1, j = j, width = widths1[j])
ft1 <- set_table_properties(ft1, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))

h0k20 <- g(H, "H0_k20", "delta_aic"); m_lin <- audit[audit$outcome == M & audit$source_model_id == "linear_interaction", ]
h_lin <- audit[audit$outcome == H & audit$source_model_id == "linear", ]
notes1 <- c(
  paste0("All models are penalised logistic generalised additive models of the outcome on the eight mutually exclusive ",
         "profiles of diabetes mellitus (DM), hypertension (HTN) and chronic kidney disease (CKD), adjusted for sex, ",
         "notification year and state of residence (random effect), fitted to the same outcome-specific cohort. Every ",
         "model includes a non-linear effect of age; models differ only in whether and how profile associations may vary ",
         "with age. Age bases use k = 10 unless stated."),
  paste0("H0, no profile-by-age interaction. H1, a separate penalised age smooth for each profile without a common age ",
         "smooth. H2, a common age smooth plus penalised profile-specific deviations from it. H3, a common age smooth plus ",
         "random effects for each age group (0–14, 15–39, 40–59, 60–74, ≥75 years) × profile. Linear, a common age smooth ",
         "plus a linear age slope for each profile (log-odds scale)."),
  paste0("ΔAIC = AIC(model) − AIC(H0) within each outcome; negative values indicate better fit after penalising for ",
         "complexity. AIC values are not comparable between outcomes."),
  paste0("Adult age-interaction test: joint Wald test (Bayesian covariance matrix of the penalised fit) of whether the ",
         "log-odds ratios of the seven profiles relative to None are equal at ages 20, 30, …, 90 years and at the ",
         "reference age of 60 years. It tests variation in conditional odds ratios, not equality of age-specific risk ",
         "ratios. — denotes not applicable (H0 models, which contain no interaction) or not directly comparable: H3 has no ",
         "age-grid test, and for the linear models only a test of the seven slope coefficients is available (p ",
         fmt_p(h_lin$coef_test_p), " for hospitalisation; p = ", fmt_p(m_lin$coef_test_p), " for death)."),
  paste0("AIC and the adult test address different questions: a lower AIC indicates better overall fit, which may arise ",
         "from childhood or other age ranges, and does not by itself establish that profile associations vary across ",
         "adult ages."),
  paste0("Selected models: H2 for hospitalisation; H0 for death. For death, H1 and H2 had lower AIC than H0, but the adult ",
         "test provided little evidence that profile associations varied across adult ages (p = ",
         fmt_p(g(M, "fs_original", "adult_wald_p")), " and ", fmt_p(g(M, "fs_global", "adult_wald_p")),
         "); it was significant only with the restrictive k = 5 basis, which fitted worse than H0 (an inadequate age ",
         "basis can transfer age misfit into the profile contrasts). Several profiles had few deaths (Supplementary Table Sx+1). ",
         "H2 (k = 15) had a marginally lower AIC than H2 (k = 10), with similar estimates in adequately supported age groups.")
)
notes1_fmt <- paste0(letters[seq_along(notes1)], ". ", notes1)

## Table Sx+1: deaths by profile and age group.
plab <- c(None = "None", DM = "DM only", HTN = "HTN only", CKD = "CKD only", `DM+HTN` = "DM + HTN",
          `DM+CKD` = "DM + CKD", `HTN+CKD` = "HTN + CKD", `DM+HTN+CKD` = "DM + HTN + CKD")
t2 <- support |>
  transmute(profile = plab[core_profile], deaths = paste0(format(total_deaths, big.mark = ","), " / ",
                                                          format(total_cases, big.mark = ",", trim = TRUE)),
            by_age = deaths_by_age, ge10 = as.character(age_groups_ge10), s19 = as.character(age_groups_1_9),
            z = as.character(age_groups_0))
t2 <- rbind(t2, data.frame(profile = "Seven non-reference profiles",
                           deaths = paste0(format(sum(support$total_deaths[-1]), big.mark = ","), " / ",
                                           format(sum(support$total_cases[-1]), big.mark = ",")),
                           by_age = "", ge10 = as.character(strata[["ge10"]]), s19 = as.character(strata[["s1_9"]]),
                           z = as.character(strata[["zero"]])))
ft2 <- flextable(t2) |>
  set_header_labels(profile = "Profile", deaths = "Deaths / cases", by_age = "Deaths by age group\n0–14 / 15–39 / 40–59 / 60–74 / ≥75",
                    ge10 = "Age groups with ≥10 deaths", s19 = "Age groups with 1–9 deaths", z = "Age groups with 0 deaths") |>
  base_style() |> border_remove() |>
  hline_top(part = "header", border = header_rule) |> hline_bottom(part = "header", border = header_rule) |>
  hline(i = 1:(nrow(t2) - 1), part = "body", border = thin_rule) |> hline(i = 1, part = "body", border = header_rule) |>
  hline(i = nrow(t2) - 1, part = "body", border = header_rule) |> hline_bottom(part = "body", border = header_rule) |>
  bold(i = nrow(t2), part = "body") |> italic(i = 1, part = "body") |>
  align(j = 1, align = "left", part = "all") |> align(j = 2:6, align = "center", part = "all")
widths2 <- c(1.75, 1.2, 1.75, 0.85, 0.85, 0.85)
for (j in 1:6) ft2 <- width(ft2, j = j, width = widths2[j])
ft2 <- set_table_properties(ft2, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))
notes2 <- paste0(
  "Observed chikungunya-attributed deaths in the mortality cohort (N = 983,947; 1,050 deaths) by profile and age group ",
  "(years). None (reference) is shown for context and is not included in the total row. Of the 35 non-reference ",
  "profile × age-group strata, ", strata[["s1_9"]], " had 1–9 deaths and ", strata[["zero"]], " had none. The ",
  "10-death threshold is a descriptive indicator of empirical support, not a formal criterion of model validity. ",
  "CKD, chronic kidney disease; DM, diabetes mellitus; HTN, hypertension.")

doc <- read_docx()
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(orient = "portrait", width = 8.27, height = 11.69),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.55, right = 0.55)))
title_par <- function(doc, bold_txt, rest) body_add_fpar(doc, fpar(
  ftext(bold_txt, prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
  ftext(rest, prop = fp_text(font.family = font_family, font.size = 10))))
note_par <- function(doc, txt) body_add_fpar(doc, fpar(ftext(txt, prop = fp_text(font.family = font_family, font.size = 7.5)),
                                                        fp_p = fp_par(padding.bottom = 2)))
doc <- title_par(doc, "Supplementary Table Sx. ",
                 "Model fit and evidence of profile-by-age interactions for hospitalisation and chikungunya-attributed death")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft1, align = "center")
doc <- body_add_par(doc, "", style = "Normal")
for (n in notes1_fmt) doc <- note_par(doc, n)
doc <- body_add_break(doc)
doc <- title_par(doc, "Supplementary Table Sx+1. ",
                 "Observed chikungunya-attributed deaths by multimorbidity profile and age group")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft2, align = "center")
doc <- body_add_par(doc, "", style = "Normal")
doc <- note_par(doc, notes2)
docx_path <- file.path(out_dir, "Supplementary_Table_Age_Model_Selection_FINAL.docx")
print(doc, target = docx_path)
flextable::save_as_image(ft1, path = file.path(out_dir, "Supplementary_Table_Age_Model_Selection_FINAL.png"), res = 220)
flextable::save_as_image(ft2, path = file.path(out_dir, "Supplementary_Table_Mortality_Event_Support_FINAL.png"), res = 220)

## Word table text must equal the CSV strings.
x <- officer::docx_summary(officer::read_docx(docx_path))
cells <- x$text[x$content_type == "table cell"]
chk2 <- all(c(tab$aic_text, tab$delta_text, tab$p_text, t2$by_age[t2$by_age != ""]) %in% cells)
message("[61] Word table strings match CSV: ", chk2)
stopifnot(chk2)
message("[61] done: ", sum(checks$pass), "/", nrow(checks), " checks passed")
