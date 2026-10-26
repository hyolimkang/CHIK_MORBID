# Clinical-outcome tables by recorded underlying condition
#
# Creates dengue-style descriptive tables for chikungunya surveillance data:
# overall, condition absent, condition present, and a crude p value. The
# baseline cohort follows the core-3 complete-case definition in
# 13_core3_descriptive.R, so these descriptive tables align with the main
# analytic population.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(openxlsx)
  library(officer)
  library(flextable)
})

# ---- User-editable settings -------------------------------------------------
data_file <- file.path("01_Data", "chik_sinan_individual_2015_2025.rds")
output_dir <- file.path("03_Output", "tables")
output_docx <- file.path(output_dir, "clinical_outcomes_by_underlying_condition.docx")
output_xlsx <- file.path(output_dir, "clinical_outcomes_by_underlying_condition.xlsx")
output_docx_fallback <- file.path(output_dir, "clinical_outcomes_by_underlying_condition_2017_2025_updated.docx")
output_xlsx_fallback <- file.path(output_dir, "clinical_outcomes_by_underlying_condition_2017_2025_updated.xlsx")
output_long_csv <- file.path(output_dir, "clinical_outcomes_by_underlying_condition_long.csv")
output_denominators_csv <- file.path(output_dir, "clinical_outcomes_by_underlying_condition_denominators_derived.csv")
output_prevalence_csv <- file.path(output_dir, "underlying_condition_prevalence_by_cohort.csv")

# List all seven condition-specific tables. The three primary conditions are
# placed first; the remaining four tables can be retained as supplementary
# material. All tables start from the common core-3 cohort below.
core_condition_vars <- c("diabetes", "hypertension", "renal_disease")
conditions_to_report <- c(
  core_condition_vars, "hepatopathy", "autoimmune", "hematologic", "peptic_ulcer"
)

condition_lookup <- tibble(
  variable = c(
    "diabetes", "hypertension", "renal_disease", "hepatopathy",
    "autoimmune", "hematologic", "peptic_ulcer"
  ),
  condition = c(
    "Diabetes mellitus", "Hypertension", "Chronic kidney disease",
    "Hepatopathy / liver disease", "Autoimmune disease",
    "Haematological disease", "Peptic ulcer disease"
  ),
  short_label = c("Diabetes", "Hypertension", "CKD", "Hepatopathy",
                  "Autoimmune disease", "Haematological disease", "Peptic ulcer disease"),
  sheet_name = c("Diabetes", "Hypertension", "CKD", "Liver", "Autoimmune",
                 "Haematological", "Peptic")
) |>
  filter(variable %in% conditions_to_report)

font_family <- "Arial"

if (!file.exists(data_file)) stop("Data file not found: ", data_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Baseline cohort and outcomes -------------------------------------------
# Use the same analytic source population as the core-3 analyses: confirmed
# records from 2017 onward, valid age/sex, and all three core conditions
# explicitly recorded as no/yes. Thus the Overall N is identical for the DM,
# HTN, and CKD tables.
ind <- readRDS(data_file)
base_cohort <- ind |>
  filter(is_confirmed_chik) |>
  filter(year(event_date) >= 2017) |>
  filter(
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female")
  ) |>
  filter(if_all(all_of(core_condition_vars), ~ .x %in% c("no", "yes"))) |>
  mutate(
    death_known = !is.na(died_from_chik),
    death_event = death_known & died_from_chik,
    hosp_known = hospitalised %in% c("no", "yes"),
    hosp_event = hospitalised == "yes",
    composite_known = death_known & hosp_known,
    composite_event = composite_known & (death_event | hosp_event)
  )

message("Core-3 complete-case population: ", format(nrow(base_cohort), big.mark = ","))

# Outcome-specific analytic cohorts. Death status is complete in the current
# extract, so death_cohort happens to equal base_cohort; it remains defined
# separately so the distinction is explicit and robust to future updates.
hosp_cohort <- base_cohort |> filter(hosp_known)
death_cohort <- base_cohort |> filter(death_known)

# ---- Descriptive prevalence of all seven recorded conditions ----------------
# Conditions are ranked by their prevalence in the core-3 source population.
# For every cohort/condition, N is the number with that condition recorded as
# either no or yes; this makes the small amount of non-core missingness clear.
condition_prevalence <- bind_rows(lapply(seq_len(nrow(condition_lookup)), function(i) {
  condition_var <- condition_lookup$variable[i]

  summarise_prevalence <- function(data) {
    known <- data[[condition_var]] %in% c("no", "yes")
    n_known <- sum(known)
    n_present <- sum(data[[condition_var]] == "yes", na.rm = TRUE)
    tibble(n_known = n_known, n_present = n_present,
           pct_present = if_else(n_known > 0L, 100 * n_present / n_known, NA_real_))
  }

  source <- summarise_prevalence(base_cohort)
  hosp <- summarise_prevalence(hosp_cohort)
  death <- summarise_prevalence(death_cohort)

  tibble(
    condition = condition_lookup$condition[i],
    source_n_known = source$n_known,
    source_n_present = source$n_present,
    source_pct_present = source$pct_present,
    hosp_n_known = hosp$n_known,
    hosp_n_present = hosp$n_present,
    hosp_pct_present = hosp$pct_present,
    death_n_known = death$n_known,
    death_n_present = death$n_present,
    death_pct_present = death$pct_present
  )
})) |>
  arrange(desc(source_pct_present), condition) |>
  mutate(rank = row_number(), .before = condition)

fmt_n_N_pct <- function(n, denominator, pct) {
  if (denominator == 0L) return("0/0 (NA)")
  sprintf("%s/%s (%.1f%%)", format(n, big.mark = ","),
          format(denominator, big.mark = ","), pct)
}

condition_prevalence_display <- condition_prevalence |>
  transmute(
    Rank = rank,
    Condition = condition,
    `Core-3 source population\nn/N (%)` = mapply(
      fmt_n_N_pct, source_n_present, source_n_known, source_pct_present
    ),
    `Hospitalisation cohort\nn/N (%)` = mapply(
      fmt_n_N_pct, hosp_n_present, hosp_n_known, hosp_pct_present
    ),
    `Mortality cohort\nn/N (%)` = mapply(
      fmt_n_N_pct, death_n_present, death_n_known, death_pct_present
    )
  )

# ---- Table calculation helpers ----------------------------------------------
fmt_n_pct <- function(n, denominator) {
  if (denominator == 0L) return("0 (NA)")
  sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / denominator)
}

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  format.pval(p, digits = 3, eps = 0.001)
}

outcome_p <- function(data, condition_var, event_var, known_var) {
  test_data <- data |>
    filter(.data[[known_var]]) |>
    transmute(
      exposure = factor(.data[[condition_var]], levels = c("no", "yes")),
      event = factor(.data[[event_var]], levels = c(FALSE, TRUE))
    )

  if (nrow(test_data) == 0L || n_distinct(test_data$exposure) < 2L ||
      n_distinct(test_data$event) < 2L) return(NA_real_)

  tab <- table(test_data$exposure, test_data$event)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  if (any(chi$expected < 5)) stats::fisher.test(tab)$p.value else chi$p.value
}

make_condition_table <- function(data, condition_var, condition_label, short_label) {
  dat <- data |>
    filter(.data[[condition_var]] %in% c("no", "yes")) |>
    mutate(exposure = .data[[condition_var]])

  absent <- dat |> filter(exposure == "no")
  present <- dat |> filter(exposure == "yes")

  cell <- function(df, indicator) fmt_n_pct(sum(indicator(df)), nrow(df))
  count_event <- function(var) function(df) sum(df[[var]], na.rm = TRUE)
  count_missing <- function(var) function(df) sum(!df[[var]], na.rm = TRUE)

  death_p <- outcome_p(dat, "exposure", "death_event", "death_known")
  hosp_p <- outcome_p(dat, "exposure", "hosp_event", "hosp_known")
  composite_p <- outcome_p(dat, "exposure", "composite_event", "composite_known")

  table_out <- tibble(
    Outcome = c(
      "Died from chikungunya", "Unknown death status",
      "Hospitalised", "Unknown hospitalisation status",
      "Composite severe outcome*", "Unknown composite outcome status"
    ),
    Overall = c(
      cell(dat, count_event("death_event")),
      cell(dat, count_missing("death_known")),
      cell(dat, count_event("hosp_event")),
      cell(dat, count_missing("hosp_known")),
      cell(dat, count_event("composite_event")),
      cell(dat, count_missing("composite_known"))
    ),
    condition_absent = c(
      cell(absent, count_event("death_event")),
      cell(absent, count_missing("death_known")),
      cell(absent, count_event("hosp_event")),
      cell(absent, count_missing("hosp_known")),
      cell(absent, count_event("composite_event")),
      cell(absent, count_missing("composite_known"))
    ),
    condition_present = c(
      cell(present, count_event("death_event")),
      cell(present, count_missing("death_known")),
      cell(present, count_event("hosp_event")),
      cell(present, count_missing("hosp_known")),
      cell(present, count_event("composite_event")),
      cell(present, count_missing("composite_known"))
    ),
    p_value = c(fmt_p(death_p), "", fmt_p(hosp_p), "", fmt_p(composite_p), "")
  )
  names(table_out) <- c("Outcome", "Overall", "Condition absent", "Condition present", "p-value")

  header_labels <- c(
    "Outcome" = "Outcome",
    "Overall" = paste0("Overall\n(N=", format(nrow(dat), big.mark = ","), ")"),
    "Condition absent" = paste0("No ", short_label, "\n(N=", format(nrow(absent), big.mark = ","), ")"),
    "Condition present" = paste0(short_label, "\n(N=", format(nrow(present), big.mark = ","), ")"),
    "p-value" = "p-value"
  )

  list(
    condition = condition_label,
    n_total = nrow(dat),
    n_absent = nrow(absent),
    n_present = nrow(present),
    table = table_out,
    header_labels = header_labels
  )
}

# ---- Generate tables for all requested conditions ---------------------------
condition_tables <- lapply(seq_len(nrow(condition_lookup)), function(i) {
  make_condition_table(
    base_cohort,
    condition_lookup$variable[i],
    condition_lookup$condition[i],
    condition_lookup$short_label[i]
  )
})
names(condition_tables) <- condition_lookup$variable

denominators <- bind_rows(lapply(condition_tables, function(x) {
  tibble(
    condition = x$condition,
    overall_n = x$n_total,
    condition_absent_n = x$n_absent,
    condition_present_n = x$n_present
  )
}))

long_results <- bind_rows(lapply(condition_tables, function(x) {
  x$table |> mutate(condition = x$condition, .before = Outcome)
}))

write.csv(denominators, output_denominators_csv, row.names = FALSE)
write.csv(long_results, output_long_csv, row.names = FALSE)
write.csv(condition_prevalence, output_prevalence_csv, row.names = FALSE)

# ---- Word document: one table per condition --------------------------------
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape", width = 11.69, height = 8.27),
  page_margins = page_mar(top = 0.55, bottom = 0.55, left = 0.55, right = 0.55)
)

make_flextable <- function(result) {
  ft <- flextable(result$table)
  ft <- do.call(set_header_labels, c(list(x = ft), as.list(result$header_labels)))
  ft <- border_remove(ft)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = font_family, part = "all")
  ft <- fontsize(ft, size = 8.5, part = "all")
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, j = 2:5, align = "center", part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- padding(ft, padding.top = 3, padding.bottom = 3,
                padding.left = 4, padding.right = 4, part = "all")
  ft <- width(ft, j = 1, width = 2.45)
  ft <- width(ft, j = 2:4, width = 2.10)
  ft <- width(ft, j = 5, width = 0.90)
  ft <- hline_top(ft, border = fp_border(color = "#4E5C63", width = 1.0), part = "header")
  ft <- hline_bottom(ft, border = fp_border(color = "#4E5C63", width = 1.0), part = "body")
  ft
}

make_prevalence_flextable <- function(data) {
  ft <- flextable(data)
  ft <- border_remove(ft)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = font_family, part = "all")
  ft <- fontsize(ft, size = 8.5, part = "all")
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- align(ft, j = 1:2, align = "left", part = "all")
  ft <- align(ft, j = 3:5, align = "center", part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- padding(ft, padding.top = 3, padding.bottom = 3,
                padding.left = 4, padding.right = 4, part = "all")
  ft <- width(ft, j = 1, width = 0.45)
  ft <- width(ft, j = 2, width = 2.65)
  ft <- width(ft, j = 3:5, width = 2.45)
  ft <- hline_top(ft, border = fp_border(color = "#4E5C63", width = 1.0), part = "header")
  ft <- hline_bottom(ft, border = fp_border(color = "#4E5C63", width = 1.0), part = "body")
  ft
}

doc <- read_docx()
doc <- body_set_default_section(doc, value = landscape_section)

prevalence_title <- paste0(
  "Table S1. Prevalence of seven recorded underlying conditions in the core-3 source population ",
  "and outcome-specific analytic cohorts, Brazil, 2017-2025."
)
prevalence_note <- paste0(
  "Conditions are ordered by prevalence in the core-3 complete-case source population ",
  "(N=", format(nrow(base_cohort), big.mark = ","), "). Values are n/N (%), where N is the number with the relevant ",
  "condition recorded as no or yes. The hospitalisation cohort includes records with hospitalisation status known ",
  "(N=", format(nrow(hosp_cohort), big.mark = ","), "); the mortality cohort includes records with death status known ",
  "(N=", format(nrow(death_cohort), big.mark = ","), ")."
)

doc <- body_add_fpar(
  doc,
  value = fpar(ftext(prevalence_title, prop = fp_text(font.family = font_family, font.size = 10, bold = TRUE)))
)
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_flextable(doc, value = make_prevalence_flextable(condition_prevalence_display), align = "center")
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_fpar(
  doc,
  value = fpar(ftext(prevalence_note, prop = fp_text(font.family = font_family, font.size = 7.5,
                                                     italic = TRUE, color = "#4F5D64")))
)
doc <- body_add_break(doc, pos = "after")

for (i in seq_along(condition_tables)) {
  result <- condition_tables[[i]]
  table_title <- paste0(
    "Table S", i + 1L, ". Clinical outcomes of confirmed chikungunya cases, stratified by ",
    tolower(result$condition), ", Brazil, 2017-2025."
  )
  table_note <- paste(
    "* Composite severe outcome: hospitalised or died from chikungunya.",
    "Percentages use the full condition-specific column denominator, including records with unknown outcome status.",
    "p-values compare condition absent versus present among records with the corresponding outcome status known",
    "(Pearson chi-squared test; Fisher exact test when an expected cell count was <5)."
  )

  doc <- body_add_fpar(
    doc,
    value = fpar(ftext(table_title, prop = fp_text(font.family = font_family, font.size = 10, bold = TRUE)))
  )
  doc <- body_add_par(doc, value = "", style = "Normal")
  doc <- body_add_flextable(doc, value = make_flextable(result), align = "center")
  doc <- body_add_par(doc, value = "", style = "Normal")
  doc <- body_add_fpar(
    doc,
    value = fpar(ftext(table_note, prop = fp_text(font.family = font_family, font.size = 7.5,
                                                   italic = TRUE, color = "#4F5D64")))
  )
  if (i < length(condition_tables)) doc <- body_add_break(doc, pos = "after")
}
write_word_output <- function(doc, target, fallback) {
  tryCatch(
    {
      print(doc, target = target)
      target
    },
    error = function(e) {
      message("Could not overwrite ", target, " (it may be open). Saving to: ", fallback)
      print(doc, target = fallback)
      fallback
    }
  )
}

written_docx <- write_word_output(doc, output_docx, output_docx_fallback)

# ---- Excel workbook and machine-readable outputs ---------------------------
wb <- createWorkbook()
header_style <- createStyle(
  fontName = font_family, fontSize = 10, textDecoration = "bold",
  fgFill = "#EAF0F2", halign = "center", valign = "center",
  border = "Bottom", borderColour = "#6B7B83"
)
body_style <- createStyle(
  fontName = font_family, fontSize = 10, valign = "center",
  border = "Bottom", borderColour = "#D8DEE1"
)
note_style <- createStyle(fontName = font_family, fontSize = 9, textDecoration = "italic", fontColour = "#4F5D64")

addWorksheet(wb, "Condition prevalence")
writeData(
  wb, "Condition prevalence",
  "Prevalence of seven recorded underlying conditions, Brazil, 2017-2025",
  startRow = 1, startCol = 1
)
addStyle(
  wb, "Condition prevalence",
  createStyle(fontName = font_family, fontSize = 12, textDecoration = "bold"),
  rows = 1, cols = 1
)
writeData(wb, "Condition prevalence", condition_prevalence_display,
          startRow = 3, startCol = 1, headerStyle = header_style)
addStyle(wb, "Condition prevalence", body_style,
         rows = 4:(3 + nrow(condition_prevalence_display)),
         cols = 1:ncol(condition_prevalence_display), gridExpand = TRUE)
writeData(
  wb, "Condition prevalence",
  paste0(
    "Values are n/N (%), where N is the number with the condition status known. ",
    "Conditions are ranked by prevalence in the core-3 source population. ",
    "Hospitalisation cohort N=", format(nrow(hosp_cohort), big.mark = ","),
    "; mortality cohort N=", format(nrow(death_cohort), big.mark = ","), "."
  ),
  startRow = 12, startCol = 1
)
addStyle(wb, "Condition prevalence", note_style, rows = 12, cols = 1)
setColWidths(wb, "Condition prevalence", cols = 1, widths = 8)
setColWidths(wb, "Condition prevalence", cols = 2, widths = 30)
setColWidths(wb, "Condition prevalence", cols = 3:5, widths = 28)
setRowHeights(wb, "Condition prevalence", rows = 3, heights = 30)
freezePane(wb, "Condition prevalence", firstRow = TRUE)

for (i in seq_along(condition_tables)) {
  result <- condition_tables[[i]]
  sheet <- condition_lookup$sheet_name[i]
  addWorksheet(wb, sheet)
  title <- paste0(
    "Clinical outcomes of confirmed chikungunya cases, stratified by ",
    tolower(result$condition), ", Brazil, 2017-2025"
  )
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet, createStyle(fontName = font_family, fontSize = 12, textDecoration = "bold"),
           rows = 1, cols = 1, gridExpand = TRUE)
  writeData(wb, sheet, result$table, startRow = 3, startCol = 1, headerStyle = header_style)
  addStyle(wb, sheet, body_style, rows = 4:(3 + nrow(result$table)),
           cols = 1:ncol(result$table), gridExpand = TRUE)
  writeData(
    wb, sheet,
    "* Composite severe outcome: hospitalised or died from chikungunya. p-values compare condition absent versus present among records with known outcome status.",
    startRow = 11, startCol = 1
  )
  addStyle(wb, sheet, note_style, rows = 11, cols = 1)
  setColWidths(wb, sheet, cols = 1, widths = 34)
  setColWidths(wb, sheet, cols = 2:4, widths = 22)
  setColWidths(wb, sheet, cols = 5, widths = 11)
  freezePane(wb, sheet, firstRow = TRUE)
}

addWorksheet(wb, "Denominators")
writeData(wb, "Denominators", denominators, headerStyle = header_style)
addStyle(wb, "Denominators", body_style, rows = 2:(nrow(denominators) + 1),
         cols = 1:ncol(denominators), gridExpand = TRUE)
setColWidths(wb, "Denominators", cols = 1:4, widths = "auto")
save_xlsx_output <- function(workbook, target, fallback) {
  save_attempt <- function(path, fail_if_warning = FALSE) {
    warning_message <- NULL
    withCallingHandlers(
      {
        saveWorkbook(workbook, path, overwrite = TRUE)
      },
      warning = function(w) {
        warning_message <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    # A valid .xlsx is a ZIP archive from which openxlsx can read the required
    # prevalence sheet. Suppress non-material XML parsing warnings here.
    is_valid <- tryCatch(
      "Condition prevalence" %in% suppressWarnings(getSheetNames(path)),
      error = function(e) FALSE
    )
    if (!is_valid) stop("Workbook validation failed after saving: ", path)
    if (fail_if_warning && !is.null(warning_message)) stop(warning_message)
    path
  }

  tryCatch(
    save_attempt(target, fail_if_warning = TRUE),
    error = function(e) {
      message("Could not safely overwrite ", target, " (it may be open). Saving to: ", fallback)
      save_attempt(fallback, fail_if_warning = FALSE)
    }
  )
}

written_xlsx <- save_xlsx_output(wb, output_xlsx, output_xlsx_fallback)

message("\nCondition-specific table denominators:\n")
print(denominators, n = Inf)
message("\nSeven-condition prevalence, ranked in the core-3 source population:\n")
print(condition_prevalence, n = Inf)
message("\nSaved:\n  ", written_docx,
        "\n  ", written_xlsx,
        "\n  ", output_long_csv,
        "\n  ", output_denominators_csv,
        "\n  ", output_prevalence_csv, "\n")
