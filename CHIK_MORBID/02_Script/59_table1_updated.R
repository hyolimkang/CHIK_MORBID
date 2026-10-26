# -----------------------------------------------------------------------------
# 59_table1_updated.R
#
# Table 1. Study population characteristics and outcome completeness.
#
# Updates 18_table1_analytic_cohorts.R's Table 1 with a cohort-selection /
# outcome-completeness section and recomputes every value directly from the
# cleaned SINAN data (not from cached workbooks), separately for the two
# outcome-specific cohorts. Visual design (fonts, rules, shading, widths,
# padding, alignment) is copied from script 18.
#
# Output (03_Output/tables/table1_updated/):
#   Table1_Study_Population_FINAL.docx, Table1_Study_Population_FINAL.png,
#   Table1_Updated_Values.csv, table1_checks.csv
# The original 03_Output/tables/table1_analytic_cohorts.docx is not touched.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(officer)
  library(flextable)
})

out_dir <- file.path("03_Output", "tables", "table1_updated")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
font_family <- "Arial"

## ---- 1. Cohorts (script 12 rules) -------------------------------------------
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
eligible <- ind |>
  dplyr::filter(is_confirmed_chik, lubridate::year(event_date) >= 2017,
                !is.na(age_years), age_years >= 0, age_years <= 100,
                sex %in% c("male", "female"),
                dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                              ~ .x %in% c("no", "yes"))) |>
  dplyr::mutate(
    dm = diabetes == "yes", htn = hypertension == "yes", ckd = renal_disease == "yes",
    profile = dplyr::case_when(
      !dm & !htn & !ckd ~ "None", dm & !htn & !ckd ~ "DM", !dm & htn & !ckd ~ "HTN",
      !dm & !htn & ckd ~ "CKD", dm & htn & !ckd ~ "DM+HTN", dm & !htn & ckd ~ "DM+CKD",
      !dm & htn & ckd ~ "HTN+CKD", TRUE ~ "DM+HTN+CKD"),
    hosp_known = hospitalised %in% c("no", "yes"),
    death_known = !is.na(died_from_chik)
  ) |>
  dplyr::select(age_years, sex, dm, htn, ckd, profile, hosp_known, hospitalised,
                death_known, died_from_chik)
rm(ind); invisible(gc())

hosp <- dplyr::filter(eligible, hosp_known) |> dplyr::mutate(event = hospitalised == "yes")
death <- dplyr::filter(eligible, death_known) |> dplyr::mutate(event = died_from_chik)

## ---- 2. Formatting helpers ---------------------------------------------------
n_fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
## One decimal place; values below 0.1% get two decimals so a non-zero share is
## never shown as 0.0% (applied identically to both columns).
pct_fmt <- function(p) ifelse(p < 0.1, formatC(p, format = "f", digits = 2),
                              formatC(p, format = "f", digits = 1))
np <- function(n, d) paste0(n_fmt(n), " (", pct_fmt(100 * n / d), "%)")
med_iqr <- function(x) {
  q <- stats::quantile(x, c(0.5, 0.25, 0.75), names = FALSE)
  paste0(format(q[1], trim = TRUE), " (", format(q[2], trim = TRUE), "–", format(q[3], trim = TRUE), ")")
}
not_applicable <- "—"

profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD")
profile_labels <- c("None", "DM only", "HTN only", "CKD only", "DM + HTN", "DM + CKD",
                    "HTN + CKD", "DM + HTN + CKD")

N_elig <- nrow(eligible)
col_values <- function(d, which) {
  N <- nrow(d); unknown <- N_elig - N
  prof_n <- vapply(profile_levels, function(p) sum(d$profile == p), numeric(1))
  c(
    elig = n_fmt(N_elig),
    unknown = np(unknown, N_elig),
    final = n_fmt(N),
    age = med_iqr(d$age_years),
    female = np(sum(d$sex == "female"), N),
    male = np(sum(d$sex == "male"), N),
    hosp = if (which == "hosp") np(sum(d$event), N) else not_applicable,
    died = if (which == "death") np(sum(d$event), N) else not_applicable,
    dm = np(sum(d$dm), N), htn = np(sum(d$htn), N), ckd = np(sum(d$ckd), N),
    setNames(np(prof_n, N), paste0("p_", profile_levels))
  )
}
vh <- col_values(hosp, "hosp")
vd <- col_values(death, "death")

## Precision is chosen per row, so both columns of a row always share the same
## number of decimals: two if either column's percentage is below 0.1%.
row_pct <- function(d, key) {
  N <- nrow(d)
  switch(key,
         female = 100 * sum(d$sex == "female") / N, male = 100 * sum(d$sex == "male") / N,
         dm = 100 * sum(d$dm) / N, htn = 100 * sum(d$htn) / N, ckd = 100 * sum(d$ckd) / N,
         if (startsWith(key, "p_")) 100 * sum(d$profile == sub("^p_", "", key)) / N else NA_real_)
}
for (key in names(vh)) {
  ph <- row_pct(hosp, key); pdth <- row_pct(death, key)
  if (!is.na(ph) && !is.na(pdth) && min(ph, pdth) < 0.1) {
    for (side in c("h", "d")) {
      p <- if (side == "h") ph else pdth
      v <- if (side == "h") vh else vd
      v[[key]] <- sub("\\(.*%\\)", paste0("(", formatC(p, format = "f", digits = 2), "%)"), v[[key]])
      if (side == "h") vh <- v else vd <- v
    }
  }
}

## ---- 3. Automated checks ----------------------------------------------------
cnt <- function(d) list(
  N = nrow(d), female = sum(d$sex == "female"), male = sum(d$sex == "male"),
  events = sum(d$event), dm = sum(d$dm), htn = sum(d$htn), ckd = sum(d$ckd),
  prof = vapply(profile_levels, function(p) sum(d$profile == p), numeric(1)))
ch <- cnt(hosp); cd <- cnt(death)
has <- function(cond) grepl(cond, profile_levels, fixed = TRUE)
fig2_death <- c(856342, 15302, 79512, 1640, 28379, 313, 924, 1535)
load("01_Data/core_profile_rr_hosp.RData"); load("01_Data/core_profile_rr_death.RData")
flow <- utils::read.csv("03_Output/tables/figure_cohort_flow_attrition.csv")

checks <- tibble::tribble(
  ~check, ~expected, ~observed,
  "Eligible core-3 population", 1074796, N_elig,
  "Hospitalisation known", 776072, ch$N,
  "Hospitalisation unknown", 298724, N_elig - ch$N,
  "Mortality known", 983947, cd$N,
  "Mortality unknown", 90849, N_elig - cd$N,
  "Hospitalisation known + unknown = eligible", N_elig, ch$N + (N_elig - ch$N),
  "Mortality known + unknown = eligible", N_elig, cd$N + (N_elig - cd$N),
  "Hospitalisations", 25363, ch$events,
  "Chikungunya-attributed deaths", 1050, cd$events,
  "Hosp: female + male = N", ch$N, ch$female + ch$male,
  "Death: female + male = N", cd$N, cd$female + cd$male,
  "Hosp: sum of 8 profiles = N", ch$N, sum(ch$prof),
  "Death: sum of 8 profiles = N", cd$N, sum(cd$prof),
  "Hosp: DM = sum of DM-containing profiles", ch$dm, sum(ch$prof[has("DM")]),
  "Hosp: HTN = sum of HTN-containing profiles", ch$htn, sum(ch$prof[has("HTN")]),
  "Hosp: CKD = sum of CKD-containing profiles", ch$ckd, sum(ch$prof[has("CKD")]),
  "Death: DM = sum of DM-containing profiles", cd$dm, sum(cd$prof[has("DM")]),
  "Death: HTN = sum of HTN-containing profiles", cd$htn, sum(cd$prof[has("HTN")]),
  "Death: CKD = sum of CKD-containing profiles", cd$ckd, sum(cd$prof[has("CKD")]),
  "Death profiles = Figure 2 counts (all 8)", 8, sum(cd$prof == fig2_death),
  "Hosp profiles = Figure 2 cache (all 8)", 8,
    sum(ch$prof == core_profile_rr_hosp$n_profile[match(profile_levels, core_profile_rr_hosp$core_profile)]),
  "Death profiles = Figure 2 cache (all 8)", 8,
    sum(cd$prof == core_profile_rr_death$n_profile[match(profile_levels, core_profile_rr_death$core_profile)]),
  "Flowchart: hospitalisation cohort", flow$retained_n[grepl("Hospitalisation cohort", flow$step)], ch$N,
  "Flowchart: mortality cohort", flow$retained_n[grepl("Mortality cohort", flow$step)], cd$N,
  "Flowchart: eligible population", flow$retained_n[grepl("core-3 complete", flow$step)], N_elig
) |> dplyr::mutate(pass = expected == observed)
print(as.data.frame(checks))
utils::write.csv(checks, file.path(out_dir, "table1_checks.csv"), row.names = FALSE)
stopifnot(all(checks$pass))

## ---- 4. Table layout ----------------------------------------------------------
sections <- c("Cohort selection and outcome completeness", "Demographic characteristics",
              "Clinical outcomes", "Core conditions", "Joint comorbidity profiles")
rows <- list(
  list(sections[1], NULL),
  list("Eligible core-3 population, N", "elig"),
  list("Outcome status unknown, n (%)", "unknown"),
  list("Final analysis cohort, N", "final"),
  list(sections[2], NULL),
  list("Age, median (IQR), years", "age"),
  list("Female, n (%)", "female"),
  list("Male, n (%)", "male"),
  list(sections[3], NULL),
  list("Hospitalised, n (%)", "hosp"),
  list("Died from chikungunya, n (%)", "died"),
  list(sections[4], NULL),
  list("Diabetes mellitus (DM), n (%)", "dm"),
  list("Hypertension (HTN), n (%)", "htn"),
  list("Chronic kidney disease (CKD), n (%)", "ckd"),
  list(sections[5], NULL)
)
rows <- c(rows, lapply(seq_along(profile_levels), function(i)
  list(paste0(profile_labels[i], ", n (%)"), paste0("p_", profile_levels[i]))))

main_table <- data.frame(
  characteristic = vapply(rows, `[[`, character(1), 1),
  hospitalisation = vapply(rows, function(r) if (is.null(r[[2]])) "" else vh[[r[[2]]]], character(1)),
  mortality = vapply(rows, function(r) if (is.null(r[[2]])) "" else vd[[r[[2]]]], character(1)),
  stringsAsFactors = FALSE
)
section_rows <- which(main_table$characteristic %in% sections)
final_row <- which(main_table$characteristic == "Final analysis cohort, N")

utils::write.csv(
  data.frame(row = seq_len(nrow(main_table)), main_table,
             is_section_heading = seq_len(nrow(main_table)) %in% section_rows),
  file.path(out_dir, "Table1_Updated_Values.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## ---- 5. Formatting: copied from 18_table1_analytic_cohorts.R --------------------
ft <- flextable(main_table)
ft <- set_header_labels(ft, characteristic = "Characteristic",
                        hospitalisation = "Hospitalisation\ncohort", mortality = "Mortality\ncohort")
for (section_row in section_rows) ft <- merge_at(ft, i = section_row, j = 1:3, part = "body")
ft <- border_remove(ft)
thin_rule <- fp_border(color = "#D0D7DB", width = 0.6)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
section_rule <- fp_border(color = "#AEB9BE", width = 0.75)
ft <- hline_top(ft, part = "header", border = header_rule)
ft <- hline_bottom(ft, part = "header", border = header_rule)
ft <- hline(ft, i = seq_len(nrow(main_table)), part = "body", border = thin_rule)
ft <- hline(ft, i = setdiff(section_rows - 1L, 0L), part = "body", border = section_rule)
ft <- bg(ft, part = "header", bg = "#EEF1F3")
ft <- bg(ft, i = section_rows, part = "body", bg = "#F5F7F8")
ft <- color(ft, part = "all", color = "#263238")
ft <- font(ft, part = "all", fontname = font_family)
ft <- fontsize(ft, part = "header", size = 9)
ft <- fontsize(ft, part = "body", size = 8.5)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, i = section_rows, part = "body", bold = TRUE)
ft <- bold(ft, i = final_row, part = "body", bold = TRUE)
ft <- align(ft, j = "characteristic", align = "left", part = "all")
ft <- align(ft, j = c("hospitalisation", "mortality"), align = "center", part = "all")
ft <- valign(ft, part = "all", valign = "center")
ft <- padding(ft, part = "all", padding.top = 3.5, padding.bottom = 3.5, padding.left = 5, padding.right = 5)
ft <- height(ft, part = "header", height = 0.44)
ft <- height(ft, part = "body", height = 0.30)
ft <- height(ft, i = section_rows, part = "body", height = 0.27)
ft <- width(ft, j = "characteristic", width = 2.75)
ft <- width(ft, j = "hospitalisation", width = 1.75)
ft <- width(ft, j = "mortality", width = 1.75)
ft <- set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE, keep_with_next = TRUE))

table_note <- paste0(
  "Hospitalisation and mortality cohorts were defined independently from the same eligible ",
  "core-3 population, each comprising cases with known status for that outcome; unknown outcome ",
  "status was not treated as absence of the outcome. Percentages in the cohort-selection section ",
  "use the eligible population as denominator; all other percentages use the corresponding ",
  "outcome-specific analysis cohort. Core-condition prevalences are marginal (a case may have more ",
  "than one condition), whereas joint profiles are mutually exclusive. Percentages below 0.1% are ",
  "shown to two decimal places, and percentages may not sum to 100% because of rounding. The ",
  "complete cohort selection flowchart is shown in Supplementary Figure S1. ",
  "CKD, chronic kidney disease; DM, diabetes mellitus; HTN, hypertension; IQR, interquartile range."
)

doc <- read_docx()
doc <- body_add_fpar(doc, fpar(
  ftext("Table 1. ", prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
  ftext("Study population characteristics and outcome completeness",
        prop = fp_text(font.family = font_family, font.size = 10))))
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_flextable(doc, value = ft, align = "center")
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_fpar(doc, fpar(
  ftext("Note. ", prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)),
  ftext(table_note, prop = fp_text(font.family = font_family, font.size = 8))))
print(doc, target = file.path(out_dir, "Table1_Study_Population_FINAL.docx"))

## Image of the table for visual QA (LibreOffice is not available here).
flextable::save_as_image(ft, path = file.path(out_dir, "Table1_Study_Population_FINAL.png"), res = 200)

print(main_table, row.names = FALSE)
message("[59] Table 1 written to ", out_dir)
