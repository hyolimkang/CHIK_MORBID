# -----------------------------------------------------------------------------
# Main manuscript table: standardised risks and relative risks by joint
# core-3 profile
#
# The eight profiles are mutually exclusive combinations of diabetes (DM),
# hypertension (HTN), and chronic kidney disease (CKD). This is the natural
# counterpart to 17_table_core3_standardised_risks.R. Every profile is
# compared with the common None profile, so the table presents both
# counterfactual risks (None and stated profile) plus their RR.
#
# Inputs (created by 12_core3_condition_specific_rr.R):
#   01_Data/core_profile_rr_hosp.RData
#   01_Data/core_profile_rr_death.RData
#
# Outputs:
#   03_Output/tables/table_joint_profile_standardised_risks.docx
#   03_Output/tables/table_joint_profile_standardised_risks.xlsx
#   03_Output/tables/table_joint_profile_standardised_risks_data.csv
# -----------------------------------------------------------------------------

required_packages <- c("officer", "flextable", "openxlsx")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(officer)
  library(flextable)
  library(openxlsx)
})

# ---- 1. User-editable settings ----------------------------------------------

hosp_input <- file.path("01_Data", "core_profile_rr_hosp.RData")
death_input <- file.path("01_Data", "core_profile_rr_death.RData")
output_dir <- file.path("03_Output", "tables")
output_docx <- file.path(output_dir, "table_joint_profile_standardised_risks.docx")
output_xlsx <- file.path(output_dir, "table_joint_profile_standardised_risks.xlsx")
output_csv <- file.path(output_dir, "table_joint_profile_standardised_risks_data.csv")

font_family <- "Arial"
profile_order <- c(
  "None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD"
)
profile_labels <- c(
  "None" = "None (no DM, HTN, or CKD)",
  "DM" = "DM only",
  "HTN" = "HTN only",
  "CKD" = "CKD only",
  "DM+HTN" = "DM + HTN",
  "DM+CKD" = "DM + CKD",
  "HTN+CKD" = "HTN + CKD",
  "DM+HTN+CKD" = "DM + HTN + CKD"
)

table_title <- paste0(
  "Table X. Standardised risks of hospitalisation and death from chikungunya ",
  "by joint core-3 profile"
)
table_note <- paste0(
  "The eight profiles are mutually exclusive. Standardised risks were obtained ",
  "from outcome-specific logistic models with adjustment for age, sex, year, and ",
  "state of residence; they are standardised over the observed covariate distribution ",
  "in each outcome cohort. Standardised RRs compare each profile with the None ",
  "profile (no diabetes, hypertension, or chronic kidney disease). Risk and RR ",
  "95% CIs are from the same paired 2,000-draw parametric bootstraps. ",
  "Hospitalisation cohort N=776,072; ",
  "mortality cohort N=1,074,796."
)

# ---- 2. Read and validate profile summaries ---------------------------------

if (!file.exists(hosp_input) || !file.exists(death_input)) {
  stop(
    "Missing joint-profile summaries. Run 02_Script/12_core3_condition_specific_rr.R first."
  )
}

load_profile_summary <- function(path, object_name) {
  input_environment <- new.env(parent = emptyenv())
  load(path, envir = input_environment)
  if (!exists(object_name, envir = input_environment, inherits = FALSE)) {
    stop("Object '", object_name, "' was not found in: ", path)
  }
  as.data.frame(get(object_name, envir = input_environment, inherits = FALSE))
}

hosp_summary <- load_profile_summary(hosp_input, "core_profile_rr_hosp")
death_summary <- load_profile_summary(death_input, "core_profile_rr_death")

required_columns <- c(
  "core_profile", "standardised_risk", "standardised_risk_lower",
  "standardised_risk_upper", "rr", "rr_lower", "rr_upper",
  "n_profile", "n_events_profile"
)
for (object_name in c("hosp_summary", "death_summary")) {
  missing_columns <- setdiff(required_columns, names(get(object_name)))
  if (length(missing_columns) > 0) {
    stop(
      object_name, " is missing: ", paste(missing_columns, collapse = ", "),
      ". Re-run 02_Script/12_core3_condition_specific_rr.R."
    )
  }
}

# ---- 3. Format the compact main-manuscript table ----------------------------

format_risk_percent <- function(risk) sprintf("%.2f", 100 * risk)
format_risk_ci <- function(risk, lower, upper) {
  sprintf("%s (%s-%s)", format_risk_percent(risk),
          format_risk_percent(lower), format_risk_percent(upper))
}
format_rr_ci <- function(profile, rr, lower, upper) {
  if (profile == "None") return("1.00 (reference)")
  sprintf("%.2f (%.2f-%.2f)", rr, lower, upper)
}

make_outcome_rows <- function(summary_table, outcome_label) {
  summary_table$core_profile <- as.character(summary_table$core_profile)
  summary_table <- summary_table[match(profile_order, summary_table$core_profile), , drop = FALSE]
  if (anyNA(summary_table$core_profile)) {
    stop("One or more expected joint profiles are absent from: ", outcome_label)
  }
  reference_row <- summary_table[summary_table$core_profile == "None", , drop = FALSE]
  reference_risk <- format_risk_ci(
    reference_row$standardised_risk,
    reference_row$standardised_risk_lower,
    reference_row$standardised_risk_upper
  )

  data.frame(
    outcome = rep(outcome_label, length(profile_order)),
    profile = unname(profile_labels[summary_table$core_profile]),
    risk_none = rep(reference_risk, length(profile_order)),
    risk_profile = ifelse(
      summary_table$core_profile == "None",
      "-",
      mapply(
        format_risk_ci,
        summary_table$standardised_risk,
        summary_table$standardised_risk_lower,
        summary_table$standardised_risk_upper,
        USE.NAMES = FALSE
      )
    ),
    rr_ci = mapply(
      format_rr_ci,
      summary_table$core_profile,
      summary_table$rr,
      summary_table$rr_lower,
      summary_table$rr_upper,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

main_table <- rbind(
  make_outcome_rows(hosp_summary, "Hospitalisation"),
  make_outcome_rows(death_summary, "Death from chikungunya")
)

numeric_output <- rbind(
  transform(hosp_summary, outcome = "Hospitalisation"),
  transform(death_summary, outcome = "Death from chikungunya")
)
numeric_output$core_profile <- as.character(numeric_output$core_profile)
numeric_output$profile_label <- unname(profile_labels[numeric_output$core_profile])
numeric_output <- numeric_output[, c(
  "outcome", "core_profile", "profile_label", "n_profile", "n_events_profile",
  "standardised_risk", "standardised_risk_lower", "standardised_risk_upper",
  "rr", "rr_lower", "rr_upper"
)]

# ---- 4. Publication-style Word table ----------------------------------------

column_labels <- c(
  outcome = "Outcome",
  profile = "Joint core-3 profile",
  risk_none = "Population-standardised risk\nunder None profile, % (95% CI)",
  risk_profile = "Population-standardised risk\nunder stated profile, % (95% CI)",
  rr_ci = "Standardised RR vs None\n(95% CI)"
)

ft <- flextable(main_table)
ft <- do.call(set_header_labels, c(list(x = ft), as.list(column_labels)))
ft <- merge_v(ft, j = c("outcome", "risk_none"), part = "body")
ft <- valign(ft, j = c("outcome", "risk_none"), valign = "center", part = "body")

ft <- border_remove(ft)
thin_rule <- fp_border(color = "#C9D1D5", width = 0.65)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
ft <- hline_top(ft, part = "header", border = header_rule)
ft <- hline_bottom(ft, part = "header", border = header_rule)
ft <- hline(ft, i = seq_len(nrow(main_table)), part = "body", border = thin_rule)
ft <- hline(ft, i = 8, part = "body", border = header_rule)
ft <- bg(ft, part = "header", bg = "#EEF1F3")
ft <- color(ft, part = "all", color = "#263238")
ft <- font(ft, part = "all", fontname = font_family)
ft <- fontsize(ft, part = "header", size = 8.5)
ft <- fontsize(ft, part = "body", size = 8.15)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, j = c("outcome", "profile"), part = "body", bold = TRUE)
ft <- align(ft, j = c("outcome", "risk_none", "risk_profile", "rr_ci"), align = "center", part = "all")
ft <- align(ft, j = "profile", align = "left", part = "all")
ft <- valign(ft, part = "all", valign = "center")
ft <- padding(ft, part = "all", padding.top = 3, padding.bottom = 3,
              padding.left = 5, padding.right = 5)
ft <- height(ft, part = "header", height = 0.42)
ft <- height(ft, part = "body", height = 0.28)
ft <- width(ft, j = "outcome", width = 1.25)
ft <- width(ft, j = "profile", width = 1.90)
ft <- width(ft, j = c("risk_none", "risk_profile"), width = 2.05)
ft <- width(ft, j = "rr_ci", width = 1.55)
ft <- set_table_properties(
  ft, layout = "fixed",
  opts_word = list(split = FALSE, keep_with_next = TRUE)
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
doc <- read_docx()
doc <- body_set_default_section(
  doc,
  value = prop_section(
    page_size = page_size(orient = "landscape", width = 11.69, height = 8.27),
    page_margins = page_mar(top = 0.60, bottom = 0.60, left = 0.60, right = 0.60)
  )
)
doc <- body_add_fpar(
  doc,
  value = fpar(ftext(table_title, prop = fp_text(font.family = font_family, font.size = 10)))
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
print(doc, target = output_docx)

# ---- 5. Excel and machine-readable exports ----------------------------------

write.csv(numeric_output, output_csv, row.names = FALSE)

wb <- createWorkbook()
header_style <- createStyle(
  fontName = font_family, fontSize = 10, textDecoration = "bold",
  fgFill = "#EEF1F3", halign = "center", valign = "center",
  border = "Bottom", borderColour = "#7E8A90", wrapText = TRUE
)
body_style <- createStyle(
  fontName = font_family, fontSize = 10, valign = "center",
  border = "Bottom", borderColour = "#D8DEE1"
)
note_style <- createStyle(fontName = font_family, fontSize = 9,
                          textDecoration = "italic", fontColour = "#4F5D64", wrapText = TRUE)

addWorksheet(wb, "Joint profile RR")
writeData(wb, "Joint profile RR", table_title, startRow = 1, startCol = 1)
addStyle(wb, "Joint profile RR",
         createStyle(fontName = font_family, fontSize = 12, textDecoration = "bold"),
         rows = 1, cols = 1)
writeData(wb, "Joint profile RR", main_table, startRow = 3, startCol = 1,
          headerStyle = header_style)
addStyle(wb, "Joint profile RR", body_style, rows = 4:(3 + nrow(main_table)),
         cols = 1:ncol(main_table), gridExpand = TRUE)
writeData(wb, "Joint profile RR", paste0("Note. ", table_note), startRow = 21, startCol = 1)
addStyle(wb, "Joint profile RR", note_style, rows = 21, cols = 1)
setColWidths(wb, "Joint profile RR", cols = 1, widths = 24)
setColWidths(wb, "Joint profile RR", cols = 2, widths = 26)
setColWidths(wb, "Joint profile RR", cols = 3:4, widths = 34)
setColWidths(wb, "Joint profile RR", cols = 5, widths = 25)
freezePane(wb, "Joint profile RR", firstRow = TRUE)

addWorksheet(wb, "Numeric data")
writeData(wb, "Numeric data", numeric_output, headerStyle = header_style)
addStyle(wb, "Numeric data", body_style, rows = 2:(nrow(numeric_output) + 1),
         cols = 1:ncol(numeric_output), gridExpand = TRUE)
setColWidths(wb, "Numeric data", cols = 1:ncol(numeric_output), widths = "auto")
freezePane(wb, "Numeric data", firstRow = TRUE)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)

message("Joint-profile standardised-risk table written to:\n  ", output_docx,
        "\n  ", output_xlsx,
        "\n  ", output_csv)
print(main_table, row.names = FALSE)
