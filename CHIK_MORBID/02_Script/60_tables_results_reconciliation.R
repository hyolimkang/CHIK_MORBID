# -----------------------------------------------------------------------------
# 60_tables_results_reconciliation.R
#
# Reconciles the results tables against the validated models and final
# figures, and writes:
#   - Main Table 2 (new): joint core-3 profiles, 4 columns (from the template
#     of the old joint-profile table, 27_table_joint_profile_standardised_risks.R)
#   - Supplementary Table (was Table 2): individual-condition marginal contrasts
#     (template: 17_table_core3_standardised_risks.R)
#   - numerical discrepancy tables for the reconciliation report
#
# Verification performed here:
#   * individual-condition model (script 12 sections 2-4) is REFITTED and its
#     standardisation re-run with the original seed/draws, and compared with
#     the cached estimates used by the tables;
#   * joint-profile estimates: RR = risk / risk(None) recomputed from
#     unrounded risks; compared with Figure 2 plotted values, with the
#     independent bam refit in script 56, and with pre-Gate-3.1 caches.
#
# Output: 03_Output/tables/tables_2_3_reconciled/ (previous files untouched)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(officer)
  library(flextable)
})

out_dir <- file.path("03_Output", "tables", "tables_2_3_reconciled")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
font_family <- "Arial"
log_msg <- function(...) message("[60] ", sprintf(...))

profile_order <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD")
profile_labels <- c(None = "None (no DM, HTN or CKD)", DM = "DM only", HTN = "HTN only",
                    CKD = "CKD only", `DM+HTN` = "DM + HTN", `DM+CKD` = "DM + CKD",
                    `HTN+CKD` = "HTN + CKD", `DM+HTN+CKD` = "DM + HTN + CKD")
condition_order <- c("Diabetes", "Hypertension", "Chronic kidney disease")

load_obj <- function(path, name) { e <- new.env(); load(path, envir = e); as.data.frame(get(name, envir = e)) }

## ---- 1. Cohorts (script 12 rules) -------------------------------------------
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
elig <- ind |>
  dplyr::filter(is_confirmed_chik, lubridate::year(event_date) >= 2017,
                !is.na(age_years), age_years >= 0, age_years <= 100, sex %in% c("male", "female"),
                dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                              ~ .x %in% c("no", "yes"))) |>
  dplyr::mutate(year = factor(lubridate::year(event_date)), sex = factor(sex, levels = c("female", "male")),
                uf_residence = factor(uf_residence), dm = as.integer(diabetes == "yes"),
                htn = as.integer(hypertension == "yes"), ckd = as.integer(renal_disease == "yes"))
rm(ind); invisible(gc())
cohorts <- list(
  hosp = elig |> dplyr::filter(hospitalised %in% c("no", "yes")) |> dplyr::mutate(y = hospitalised == "yes"),
  death = elig |> dplyr::filter(!is.na(died_from_chik)) |> dplyr::mutate(y = died_from_chik)
)
cohorts <- lapply(cohorts, function(d) dplyr::select(d, y, age_years, sex, year, uf_residence, dm, htn, ckd))
stopifnot(nrow(cohorts$hosp) == 776072, sum(cohorts$hosp$y) == 25363,
          nrow(cohorts$death) == 983947, sum(cohorts$death$y) == 1050)

## ---- 2. Individual-condition model: refit and re-standardise ----------------
## Exactly script 12's fit_*_core3 and standardised_rr() (seed 1, 2,000 draws,
## blocks of 100), applied to the current corrected cohorts.
standardised_rr <- function(fit, data, exposure_var, B = 2000, seed = 1, draw_block_size = 100) {
  combo <- dplyr::count(data, age_years, sex, year, uf_residence, dm, htn, ckd, name = "n")
  c1 <- combo; c0 <- combo; c1[[exposure_var]] <- 1L; c0[[exposure_var]] <- 0L
  X1 <- predict(fit, newdata = c1, type = "lpmatrix"); X0 <- predict(fit, newdata = c0, type = "lpmatrix")
  w <- combo$n; b <- coef(fit)
  r1 <- sum(plogis(as.numeric(X1 %*% b)) * w) / sum(w); r0 <- sum(plogis(as.numeric(X0 %*% b)) * w) / sum(w)
  set.seed(seed)
  bd <- MASS::mvrnorm(n = B, mu = b, Sigma = vcov(fit, unconditional = TRUE))
  d1 <- numeric(B); d0 <- numeric(B)
  for (f in seq.int(1, B, by = draw_block_size)) {
    i <- f:min(f + draw_block_size - 1, B); bb <- t(bd[i, , drop = FALSE])
    d1[i] <- as.numeric(crossprod(w, plogis(X1 %*% bb))) / sum(w)
    d0[i] <- as.numeric(crossprod(w, plogis(X0 %*% bb))) / sum(w)
  }
  q <- function(x, p) quantile(x, p, names = FALSE)
  data.frame(exposure = exposure_var, rr = r1 / r0, rr_lower = q(d1 / d0, 0.025), rr_upper = q(d1 / d0, 0.975),
             risk_present = r1, risk_present_lower = q(d1, 0.025), risk_present_upper = q(d1, 0.975),
             risk_absent = r0, risk_absent_lower = q(d0, 0.025), risk_absent_upper = q(d0, 0.975))
}
f_core3 <- y ~ s(age_years, k = 10, bs = "ts") + sex + year + s(uf_residence, bs = "re") + dm + htn + ckd
refit_core3 <- list()
for (o in names(cohorts)) {
  log_msg("refitting individual-condition model: %s", o)
  fit <- mgcv::bam(f_core3, data = cohorts[[o]], family = binomial(link = "logit"),
                   method = "fREML", discrete = TRUE)
  refit_core3[[o]] <- do.call(rbind, lapply(c("dm", "htn", "ckd"), function(v) standardised_rr(fit, cohorts[[o]], v))) |>
    dplyr::mutate(outcome = o, condition = condition_order[match(exposure, c("dm", "htn", "ckd"))])
}
refit_core3 <- dplyr::bind_rows(refit_core3)

cached_core3 <- dplyr::bind_rows(
  load_obj("01_Data/core3_rr_hosp.RData", "core3_rr_hosp") |> dplyr::mutate(outcome = "hosp"),
  load_obj("01_Data/core3_rr_death.RData", "core3_rr_death") |> dplyr::mutate(outcome = "death"))
legacy_core3 <- dplyr::bind_rows(
  load_obj("01_Data/legacy_pre_gate31/core3_rr_hosp.RData", "core3_rr_hosp") |> dplyr::mutate(outcome = "hosp"),
  load_obj("01_Data/legacy_pre_gate31/core3_rr_death.RData", "core3_rr_death") |> dplyr::mutate(outcome = "death"))

std_core3 <- function(d) d |> dplyr::transmute(
  outcome, condition, risk_absent = standardised_risk_unexposed, risk_absent_lower = standardised_risk_unexposed_lower,
  risk_absent_upper = standardised_risk_unexposed_upper, risk_present = standardised_risk_exposed,
  risk_present_lower = standardised_risk_exposed_lower, risk_present_upper = standardised_risk_exposed_upper,
  rr, rr_lower, rr_upper)
cached_core3 <- std_core3(cached_core3); legacy_core3 <- std_core3(legacy_core3)
num_cols_core3 <- c("risk_absent", "risk_absent_lower", "risk_absent_upper", "risk_present",
                    "risk_present_lower", "risk_present_upper", "rr", "rr_lower", "rr_upper")
cmp_core3_refit <- cached_core3 |>
  dplyr::inner_join(dplyr::select(refit_core3, outcome, condition, dplyr::all_of(num_cols_core3)),
                    by = c("outcome", "condition"), suffix = c("_cached", "_refit"))
max_rel_refit <- max(sapply(num_cols_core3, function(v)
  max(abs(cmp_core3_refit[[paste0(v, "_refit")]] / cmp_core3_refit[[paste0(v, "_cached")]] - 1))))
log_msg("individual-condition refit vs cache: max relative difference %.2e", max_rel_refit)

## ---- 3. Joint-profile estimates -------------------------------------------------
std_prof <- function(d, o) d |> dplyr::transmute(
  outcome = o, core_profile = as.character(core_profile), n_profile, n_events_profile,
  risk = standardised_risk, risk_lower = standardised_risk_lower, risk_upper = standardised_risk_upper,
  rr, rr_lower, rr_upper)
prof <- dplyr::bind_rows(std_prof(load_obj("01_Data/core_profile_rr_hosp.RData", "core_profile_rr_hosp"), "hosp"),
                         std_prof(load_obj("01_Data/core_profile_rr_death.RData", "core_profile_rr_death"), "death"))
legacy_prof <- dplyr::bind_rows(
  std_prof(load_obj("01_Data/legacy_pre_gate31/core_profile_rr_hosp.RData", "core_profile_rr_hosp"), "hosp"),
  std_prof(load_obj("01_Data/legacy_pre_gate31/core_profile_rr_death.RData", "core_profile_rr_death"), "death"))

## RR consistent with the unrounded risks; counts consistent with the cohorts.
rr_check <- prof |> dplyr::group_by(outcome) |>
  dplyr::mutate(rr_from_risks = risk / risk[core_profile == "None"]) |> dplyr::ungroup()
stopifnot(max(abs(rr_check$rr / rr_check$rr_from_risks - 1)) < 1e-12)
stopifnot(sum(prof$n_profile[prof$outcome == "hosp"]) == 776072,
          sum(prof$n_events_profile[prof$outcome == "hosp"]) == 25363,
          sum(prof$n_profile[prof$outcome == "death"]) == 983947,
          sum(prof$n_events_profile[prof$outcome == "death"]) == 1050)

## Figure 2 plotted values.
fig2 <- utils::read.csv("03_Output/figure_revision/figure2_plotted_values.csv") |>
  dplyr::mutate(outcome = ifelse(outcome == "Hospitalisation", "hosp", "death"))
cmp_fig2 <- prof |> dplyr::inner_join(fig2, by = c("outcome", "core_profile"), suffix = c("", "_fig2")) |>
  dplyr::transmute(outcome, core_profile,
                   max_abs_diff = pmax(abs(risk - standardised_risk), abs(risk_lower - standardised_risk_lower),
                                       abs(risk_upper - standardised_risk_upper), abs(rr - rr_fig2),
                                       abs(rr_lower - rr_lower_fig2), abs(rr_upper - rr_upper_fig2)))
log_msg("table vs Figure 2: max abs difference %.2e", max(cmp_fig2$max_abs_diff))

## Independent bam refit (script 56) - point and CI agreement.
indep <- utils::read.csv("03_Output/figure_revision/within_count/within_count_standardised_risks.csv") |>
  dplyr::filter(model == "profile") |> dplyr::rename(core_profile = level)
cmp_indep <- prof |> dplyr::inner_join(indep, by = c("outcome", "core_profile")) |>
  dplyr::transmute(outcome, core_profile, risk_pct = 100 * risk, risk_pct_bam = std_risk_pct,
                   rel_diff_point = std_risk_pct / (100 * risk) - 1,
                   rel_diff_lower = lower_pct / (100 * risk_lower) - 1,
                   rel_diff_upper = upper_pct / (100 * risk_upper) - 1)

## Within-count RD: same estimand, same target; table RD = difference of the
## table's unrounded point risks.
con <- utils::read.csv("03_Output/figure_revision/within_count/within_count_direct_contrasts.csv")
cmp_rd <- con |>
  dplyr::left_join(dplyr::transmute(prof, outcome, profile_a = core_profile, ra = risk), by = c("outcome", "profile_a")) |>
  dplyr::left_join(dplyr::transmute(prof, outcome, profile_b = core_profile, rb = risk), by = c("outcome", "profile_b")) |>
  dplyr::transmute(outcome, contrast, rd_from_table_pp = 100 * (ra - rb), rd_fig4_pp = rd_pp,
                   abs_diff_pp = abs(rd_from_table_pp - rd_fig4_pp),
                   rd_fig4_ci = sprintf("%.2f to %.2f", rd_lower_pp, rd_upper_pp))

## ---- 4. Old tables as currently on disk ---------------------------------------
read_cells <- function(path) {
  x <- officer::docx_summary(officer::read_docx(path))
  tc <- x[x$content_type == "table cell", ]
  m <- tapply(tc$text, list(tc$row_id, tc$cell_id), function(z) z[1])
  list(cells = m, note = x$text[x$content_type == "paragraph" & grepl("^Note", x$text)])
}
old_t2 <- read_cells("03_Output/tables/table_core3_standardised_risks.docx")
old_t3 <- read_cells("03_Output/tables/table_joint_profile_standardised_risks.docx")

## ---- 5. New table strings ---------------------------------------------------------
pct <- function(x) sprintf("%.2f", 100 * x)
risk_ci <- function(r, l, u) paste0(pct(r), " (", pct(l), "–", pct(u), ")")
rr_ci <- function(r, l, u) sprintf("%.2f (%.2f–%.2f)", r, l, u)
hy <- function(s) gsub("–", "-", s)   # old tables used ASCII hyphens

outcome_label <- c(hosp = "Hospitalisation", death = "Chikungunya-attributed death")
new_t2 <- prof |>
  dplyr::mutate(core_profile = factor(core_profile, levels = profile_order),
                outcome = factor(outcome, levels = c("hosp", "death"))) |>
  dplyr::arrange(outcome, core_profile) |>
  dplyr::mutate(
    outcome_label = outcome_label[as.character(outcome)],
    profile_label = profile_labels[as.character(core_profile)],
    risk_text = risk_ci(risk, risk_lower, risk_upper),
    rr_text = ifelse(core_profile == "None", "1.00 (reference)", rr_ci(rr, rr_lower, rr_upper))
  )
new_supp <- cached_core3 |>
  dplyr::mutate(outcome = factor(outcome, levels = c("hosp", "death")),
                condition = factor(condition, levels = condition_order)) |>
  dplyr::arrange(outcome, condition) |>
  dplyr::mutate(outcome_label = outcome_label[as.character(outcome)],
                absent_text = risk_ci(risk_absent, risk_absent_lower, risk_absent_upper),
                present_text = risk_ci(risk_present, risk_present_lower, risk_present_upper),
                rr_text = rr_ci(rr, rr_lower, rr_upper))

## Old-vs-new string comparison.
old3 <- old_t3$cells
cmp_old3 <- dplyr::tibble(row = 2:17) |>
  dplyr::mutate(profile_label = old3[row, 2], old_risk_profile = old3[row, 4], old_risk_none = old3[row, 3],
                old_rr = old3[row, 5], new_risk = hy(new_t2$risk_text), new_rr = hy(new_t2$rr_text),
                risk_match = ifelse(profile_label == "None (no DM, HTN, or CKD)",
                                    old_risk_none == new_risk, old_risk_profile == new_risk),
                rr_match = old_rr == new_rr)
old2 <- old_t2$cells
cmp_old2 <- dplyr::tibble(row = 2:7) |>
  dplyr::mutate(condition = old2[row, 2], old_absent = old2[row, 3], old_present = old2[row, 4], old_rr = old2[row, 5],
                new_absent = hy(new_supp$absent_text), new_present = hy(new_supp$present_text), new_rr = hy(new_supp$rr_text),
                match = old_absent == new_absent & old_present == new_present & old_rr == new_rr)

## Legacy (pre-Gate 3.1) vs corrected.
cmp_legacy_prof <- prof |> dplyr::inner_join(legacy_prof, by = c("outcome", "core_profile"), suffix = c("", "_legacy")) |>
  dplyr::transmute(outcome, core_profile, n_corrected = n_profile, n_legacy = n_profile_legacy,
                   events_corrected = n_events_profile, events_legacy = n_events_profile_legacy,
                   risk_corrected = risk_ci(risk, risk_lower, risk_upper),
                   risk_legacy = risk_ci(risk_legacy, risk_lower_legacy, risk_upper_legacy),
                   rr_corrected = rr_ci(rr, rr_lower, rr_upper), rr_legacy = rr_ci(rr_legacy, rr_lower_legacy, rr_upper_legacy))
cmp_legacy_core3 <- cached_core3 |> dplyr::inner_join(legacy_core3, by = c("outcome", "condition"), suffix = c("", "_legacy")) |>
  dplyr::transmute(outcome, condition,
                   absent_corrected = risk_ci(risk_absent, risk_absent_lower, risk_absent_upper),
                   absent_legacy = risk_ci(risk_absent_legacy, risk_absent_lower_legacy, risk_absent_upper_legacy),
                   present_corrected = risk_ci(risk_present, risk_present_lower, risk_present_upper),
                   present_legacy = risk_ci(risk_present_legacy, risk_present_lower_legacy, risk_present_upper_legacy),
                   rr_corrected = rr_ci(rr, rr_lower, rr_upper), rr_legacy = rr_ci(rr_legacy, rr_lower_legacy, rr_upper_legacy))

for (nm in c("cmp_core3_refit", "cmp_fig2", "cmp_indep", "cmp_rd", "cmp_old3", "cmp_old2",
             "cmp_legacy_prof", "cmp_legacy_core3")) {
  utils::write.csv(get(nm), file.path(out_dir, paste0(nm, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
}
print(cmp_old3[, c("profile_label", "risk_match", "rr_match")]); print(cmp_old2[, c("condition", "match")])
print(cmp_rd); print(old_t3$note)

utils::write.csv(
  new_t2 |> dplyr::transmute(outcome = outcome_label, core_profile, profile_label, n_cases = n_profile,
                             n_events = n_events_profile, std_risk_pct = 100 * risk, std_risk_lower_pct = 100 * risk_lower,
                             std_risk_upper_pct = 100 * risk_upper, rr, rr_lower, rr_upper,
                             risk_display = risk_text, rr_display = rr_text),
  file.path(out_dir, "Table2_Joint_Profiles_Validated.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## ---- 6. Word tables (templates: scripts 27 and 17) --------------------------
style_table <- function(ft, n_rows, split_after, widths) {
  ft <- merge_v(ft, j = 1, part = "body")
  ft <- border_remove(ft)
  thin_rule <- fp_border(color = "#C9D1D5", width = 0.65)
  header_rule <- fp_border(color = "#7E8A90", width = 1.0)
  ft <- hline_top(ft, part = "header", border = header_rule)
  ft <- hline_bottom(ft, part = "header", border = header_rule)
  ft <- hline(ft, i = seq_len(n_rows), part = "body", border = thin_rule)
  ft <- hline(ft, i = split_after, part = "body", border = header_rule)
  ft <- bg(ft, part = "header", bg = "#EEF1F3")
  ft <- color(ft, part = "all", color = "#263238")
  ft <- font(ft, part = "all", fontname = font_family)
  ft <- fontsize(ft, part = "header", size = 8.5)
  ft <- fontsize(ft, part = "body", size = 8.25)
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- bold(ft, j = 1:2, part = "body", bold = TRUE)
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 2, align = "left", part = "all")
  ft <- valign(ft, part = "all", valign = "center")
  ft <- padding(ft, part = "all", padding.top = 3, padding.bottom = 3, padding.left = 5, padding.right = 5)
  ft <- height(ft, part = "header", height = 0.42)
  ft <- height(ft, part = "body", height = 0.28)
  for (j in seq_along(widths)) ft <- width(ft, j = j, width = widths[j])
  set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))
}
write_table_doc <- function(ft, title_bold, title_rest, note, path) {
  doc <- read_docx()
  doc <- body_set_default_section(doc, prop_section(
    page_margins = page_mar(top = 0.65, bottom = 0.65, left = 0.60, right = 0.60)))
  doc <- body_add_fpar(doc, fpar(
    ftext(title_bold, prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
    ftext(title_rest, prop = fp_text(font.family = font_family, font.size = 10))))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft, align = "center")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_fpar(doc, fpar(
    ftext("Note. ", prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)),
    ftext(note, prop = fp_text(font.family = font_family, font.size = 8))))
  print(doc, target = path)
}

t2 <- data.frame(outcome = new_t2$outcome_label, profile = new_t2$profile_label,
                 risk = new_t2$risk_text, rr = new_t2$rr_text, stringsAsFactors = FALSE)
ft2 <- flextable(t2) |>
  set_header_labels(outcome = "Outcome", profile = "Joint core-3 profile",
                    risk = "Population-standardised risk,\n% (95% CI)", rr = "Standardised RR vs None\n(95% CI)") |>
  style_table(n_rows = nrow(t2), split_after = 8, widths = c(1.25, 1.95, 2.05, 1.70))
note2 <- paste0(
  "The eight profiles are mutually exclusive combinations of diabetes mellitus (DM), hypertension (HTN) and ",
  "chronic kidney disease (CKD). Risks are from outcome-specific penalised logistic generalised additive models ",
  "including profile, a smooth function of age, sex, notification year and state of residence (random effect), ",
  "standardised by g-computation to the observed age, sex, year and state distribution of each outcome cohort ",
  "(hospitalisation, N = 776,072 with 25,363 hospitalisations; chikungunya-attributed death, N = 983,947 with ",
  "1,050 deaths). Standardised risk ratios (RR) compare each profile with None. 95% CIs for risks and RRs are ",
  "from the same 2,000 parametric draws of the model coefficients. Observed cases and events per profile are ",
  "given in Table 1; estimates for DM + CKD in the mortality analysis are based on 4 deaths."
)
write_table_doc(ft2, "Table 2. ",
                "Population-standardised risks of hospitalisation and chikungunya-attributed death by joint comorbidity profile",
                note2, file.path(out_dir, "Table2_Joint_Profiles_FINAL.docx"))
flextable::save_as_image(ft2, path = file.path(out_dir, "Table2_Joint_Profiles_FINAL.png"), res = 200)

ts <- data.frame(outcome = new_supp$outcome_label, condition = as.character(new_supp$condition),
                 absent = new_supp$absent_text, present = new_supp$present_text, rr = new_supp$rr_text,
                 stringsAsFactors = FALSE)
fts <- flextable(ts) |>
  set_header_labels(outcome = "Outcome", condition = "Condition", absent = "Risk if absent, %\n(95% CI)",
                    present = "Risk if present, %\n(95% CI)", rr = "Standardised RR\n(95% CI)") |>
  style_table(n_rows = nrow(ts), split_after = 3, widths = c(1.25, 1.55, 1.45, 1.45, 1.40))
notes <- paste0(
  "Marginal individual-condition contrasts: each row compares cases with the stated condition present versus ",
  "absent, regardless of the other two conditions, which keep their observed distribution. These differ from ",
  "the mutually exclusive joint profiles in Table 2. Risks are from outcome-specific penalised logistic generalised ",
  "additive models including DM, HTN and CKD simultaneously, a smooth function of age, sex, notification year and ",
  "state of residence (random effect), standardised by g-computation to each outcome cohort (hospitalisation, ",
  "N = 776,072; chikungunya-attributed death, N = 983,947). Standardised risk ratio (RR) is the ratio of the ",
  "present to the absent standardised risk; 95% CIs for risks and RRs are from the same 2,000 parametric draws ",
  "of the model coefficients. CKD, chronic kidney disease; DM, diabetes mellitus; HTN, hypertension."
)
write_table_doc(fts, "Supplementary Table S[X]. ",
                "Standardised risks of hospitalisation and chikungunya-attributed death by individual core condition",
                notes, file.path(out_dir, "Supplementary_Table_Individual_Conditions_FINAL.docx"))
flextable::save_as_image(fts, path = file.path(out_dir, "Supplementary_Table_Individual_Conditions_FINAL.png"), res = 200)

log_msg("done")
