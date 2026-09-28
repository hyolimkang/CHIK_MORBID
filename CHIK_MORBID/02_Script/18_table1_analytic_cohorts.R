# -----------------------------------------------------------------------------
# Table 1. Characteristics of the two analytic cohorts
#
# Combines the separately generated Table1_hosp and Table1_death sheets from
# 13_core3_descriptive.R into one concise manuscript-ready Word table.
#
# Output:
#   03_Output/tables/table1_analytic_cohorts.docx
# -----------------------------------------------------------------------------

required_packages <- c("openxlsx", "officer", "flextable")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    ". Install with install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "), "))."
  )
}

suppressPackageStartupMessages({
  library(openxlsx)
  library(officer)
  library(flextable)
})

# ---- 1. Inputs and editable text --------------------------------------------

source_workbook <- file.path("03_Output", "tables", "core3_descriptive_tables.xlsx")
output_dir <- file.path("03_Output", "tables")
output_file <- file.path(output_dir, "table1_analytic_cohorts.docx")

table_title <- "Table 1. Characteristics of the two analytic cohorts"
column_labels <- c(
  characteristic = "Characteristic",
  hospitalisation = "Hospitalisation\ncohort",
  mortality = "Mortality\ncohort"
)

table_note <- paste0(
  "Values are n (%) unless otherwise stated. Core conditions are marginal ",
  "prevalences and may overlap. Joint profiles are mutually exclusive ",
  "combinations of diabetes (DM), hypertension (HTN), and chronic kidney ",
  "disease (CKD). Hospitalisation and mortality cohorts were constructed ",
  "independently according to outcome-status availability."
)

font_family <- "Arial"

# ---- 2. Read the existing descriptive tables --------------------------------

if (!file.exists(source_workbook)) {
  stop(
    "Missing ", source_workbook,
    ". Run 02_Script/13_core3_descriptive.R first."
  )
}

available_sheets <- openxlsx::getSheetNames(source_workbook)
required_sheets <- c("Table1_hosp", "Table1_death")
if (!all(required_sheets %in% available_sheets)) {
  stop(
    "Source workbook does not contain: ",
    paste(setdiff(required_sheets, available_sheets), collapse = ", ")
  )
}

table1_hosp <- openxlsx::read.xlsx(source_workbook, sheet = "Table1_hosp")
table1_death <- openxlsx::read.xlsx(source_workbook, sheet = "Table1_death")

required_columns <- c("Characteristic", "Level", "Value")
for (object_name in c("table1_hosp", "table1_death")) {
  missing_columns <- setdiff(required_columns, names(get(object_name)))
  if (length(missing_columns) > 0) {
    stop(object_name, " is missing: ", paste(missing_columns, collapse = ", "))
  }
}

# In older workbooks, the age IQR separator may have been written literally as
# '<U+2013>' under a restrictive R locale. Normalise it for Word output.
normalise_text <- function(x) {
  x <- as.character(x)
  # Convert the UTF-8 en dash used in the cached IQR value to an ASCII hyphen;
  # this avoids locale-dependent rendering in the Word document.
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("<U+2013>", "-", x, fixed = TRUE)
  trimws(x)
}

table1_hosp[] <- lapply(table1_hosp, normalise_text)
table1_death[] <- lapply(table1_death, normalise_text)

lookup_value <- function(data, characteristic, level = NULL) {
  matches <- data$Characteristic == characteristic
  if (!is.null(level)) {
    matches <- matches & data$Level == level
  }
  values <- data$Value[matches]
  if (length(values) != 1L) {
    label <- if (is.null(level)) characteristic else paste(characteristic, level, sep = " / ")
    stop("Could not uniquely find table value for: ", label)
  }
  values[[1]]
}

# ---- 3. Construct the combined Table 1 --------------------------------------

profile_labels <- c(
  "None" = "None",
  "DM" = "DM only",
  "HTN" = "HTN only",
  "CKD" = "CKD only",
  "DM+HTN" = "DM+HTN",
  "DM+CKD" = "DM+CKD",
  "HTN+CKD" = "HTN+CKD",
  "DM+HTN+CKD" = "DM+HTN+CKD"
)

main_table <- data.frame(
  characteristic = c(
    "N",
    "Age, median (IQR), years",
    "Female",
    "Male",
    "Hospitalised",
    "Died from chikungunya",
    "Core conditions",
    "Diabetes",
    "Hypertension",
    "CKD",
    "Joint profiles",
    unname(profile_labels)
  ),
  hospitalisation = c(
    lookup_value(table1_hosp, "N"),
    lookup_value(table1_hosp, "Age, years", "median (IQR)"),
    lookup_value(table1_hosp, "Sex", "female"),
    lookup_value(table1_hosp, "Sex", "male"),
    lookup_value(table1_hosp, "Hospitalised", "Yes"),
    "-",
    "",
    lookup_value(table1_hosp, "Core condition present (marginal - regardless of the other two)", "Diabetes"),
    lookup_value(table1_hosp, "Core condition present (marginal - regardless of the other two)", "Hypertension"),
    lookup_value(table1_hosp, "Core condition present (marginal - regardless of the other two)", "Chronic kidney disease"),
    "",
    vapply(
      names(profile_labels),
      function(profile) lookup_value(table1_hosp, "Joint profile (mutually exclusive)", profile),
      character(1)
    )
  ),
  mortality = c(
    lookup_value(table1_death, "N"),
    lookup_value(table1_death, "Age, years", "median (IQR)"),
    lookup_value(table1_death, "Sex", "female"),
    lookup_value(table1_death, "Sex", "male"),
    "-",
    lookup_value(table1_death, "Died from chikungunya", "Yes"),
    "",
    lookup_value(table1_death, "Core condition present (marginal - regardless of the other two)", "Diabetes"),
    lookup_value(table1_death, "Core condition present (marginal - regardless of the other two)", "Hypertension"),
    lookup_value(table1_death, "Core condition present (marginal - regardless of the other two)", "Chronic kidney disease"),
    "",
    vapply(
      names(profile_labels),
      function(profile) lookup_value(table1_death, "Joint profile (mutually exclusive)", profile),
      character(1)
    )
  ),
  stringsAsFactors = FALSE
)

section_rows <- which(main_table$characteristic %in% c("Core conditions", "Joint profiles"))

# ---- 4. Format the Word table ------------------------------------------------

ft <- flextable(main_table)
ft <- set_header_labels(
  ft,
  characteristic = column_labels[["characteristic"]],
  hospitalisation = column_labels[["hospitalisation"]],
  mortality = column_labels[["mortality"]]
)

# Section headings span all columns and make the two conceptually different
# condition summaries (marginal and joint) explicit.
for (section_row in section_rows) {
  ft <- merge_at(ft, i = section_row, j = 1:3, part = "body")
}

ft <- border_remove(ft)
thin_rule <- fp_border(color = "#D0D7DB", width = 0.6)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
section_rule <- fp_border(color = "#AEB9BE", width = 0.75)
ft <- hline_top(ft, part = "header", border = header_rule)
ft <- hline_bottom(ft, part = "header", border = header_rule)
ft <- hline(ft, i = seq_len(nrow(main_table)), part = "body", border = thin_rule)
# hline() draws the bottom border of a row; apply it to the row preceding
# each section heading to create a slightly stronger separating rule.
ft <- hline(ft, i = section_rows - 1L, part = "body", border = section_rule)

ft <- bg(ft, part = "header", bg = "#EEF1F3")
ft <- bg(ft, i = section_rows, part = "body", bg = "#F5F7F8")
ft <- color(ft, part = "all", color = "#263238")
ft <- font(ft, part = "all", fontname = font_family)
ft <- fontsize(ft, part = "header", size = 9)
ft <- fontsize(ft, part = "body", size = 8.5)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, i = section_rows, part = "body", bold = TRUE)
ft <- bold(ft, i = 1:6, j = "characteristic", part = "body", bold = TRUE)
ft <- align(ft, j = "characteristic", align = "left", part = "all")
ft <- align(ft, j = c("hospitalisation", "mortality"), align = "center", part = "all")
ft <- valign(ft, part = "all", valign = "center")
ft <- padding(ft, part = "all", padding.top = 3.5, padding.bottom = 3.5,
              padding.left = 5, padding.right = 5)
ft <- height(ft, part = "header", height = 0.44)
ft <- height(ft, part = "body", height = 0.30)
ft <- height(ft, i = section_rows, part = "body", height = 0.27)

# Fits cleanly within a standard portrait page with default Word margins.
ft <- width(ft, j = "characteristic", width = 2.75)
ft <- width(ft, j = "hospitalisation", width = 1.75)
ft <- width(ft, j = "mortality", width = 1.75)
ft <- set_table_properties(
  ft, layout = "fixed",
  opts_word = list(split = FALSE, keep_with_next = TRUE)
)

# ---- 5. Write Word document --------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

doc <- read_docx()
doc <- body_add_fpar(
  doc,
  value = fpar(
    ftext("Table 1. ", prop = fp_text(font.family = font_family, bold = TRUE, font.size = 10)),
    ftext(
      "Characteristics of the two analytic cohorts",
      prop = fp_text(font.family = font_family, font.size = 10)
    )
  )
)
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_flextable(doc, value = ft, align = "center")
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_fpar(
  doc,
  value = fpar(
    ftext("Note. ", prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)),
    ftext(table_note, prop = fp_text(font.family = font_family, font.size = 8))
  )
)

print(doc, target = output_file)

message("Combined Table 1 written to: ", output_file)
print(main_table, row.names = FALSE)
