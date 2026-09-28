# -----------------------------------------------------------------------------
# Main manuscript table: standardised risks by core underlying condition
#
# Reads the cached core-condition summaries created by
# 12_core3_condition_specific_rr.R and writes a concise, manuscript-ready
# Word table. Deliberately excludes crude counts/events and adjusted odds
# ratios: those remain available in the RData inputs and the accompanying
# Excel workbook, but would overburden the main table.
#
# Output:
#   03_Output/tables/table_core3_standardised_risks.docx
# -----------------------------------------------------------------------------

required_packages <- c("officer", "flextable")
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
  library(officer)
  library(flextable)
})

# ---- 1. Inputs and editable labels ------------------------------------------

hosp_input <- file.path("01_Data", "core3_rr_hosp.RData")
death_input <- file.path("01_Data", "core3_rr_death.RData")
output_dir <- file.path("03_Output", "tables")
output_file <- file.path(output_dir, "table_core3_standardised_risks.docx")

table_title <- paste0(
  "Table X. Standardised risks of hospitalisation and death from chikungunya ",
  "by core underlying condition"
)

column_labels <- c(
  outcome = "Outcome",
  condition = "Condition",
  risk_absent = "Risk if absent, %\n(95% CI)",
  risk_present = "Risk if present, %\n(95% CI)",
  rr_ci = "Standardised RR (95% CI)"
)

table_note <- paste0(
  "Risks are standardised predicted risks from mutually adjusted logistic ",
  "models, with the focal condition set to absent or present. Models include ",
  "diabetes, hypertension, and chronic kidney disease simultaneously and ",
  "adjust for age, sex, year, and state of residence. Standardised RR is the ",
  "ratio of the present to absent predicted risk. Risk and RR 95% CIs are ",
  "from the same 2,000 parametric bootstrap draws."
)

font_family <- "Arial"

# ---- 2. Read and validate cached model summaries ----------------------------

if (!file.exists(hosp_input) || !file.exists(death_input)) {
  stop(
    "Missing cached model summaries. Run 02_Script/12_core3_condition_specific_rr.R first."
  )
}

load(hosp_input)   # creates core3_rr_hosp
load(death_input)  # creates core3_rr_death

required_columns <- c(
  "condition", "rr", "rr_lower", "rr_upper",
  "standardised_risk_exposed", "standardised_risk_exposed_lower",
  "standardised_risk_exposed_upper", "standardised_risk_unexposed",
  "standardised_risk_unexposed_lower", "standardised_risk_unexposed_upper"
)
for (object_name in c("core3_rr_hosp", "core3_rr_death")) {
  if (!exists(object_name, inherits = FALSE)) {
    stop("Expected object not found after loading input: ", object_name)
  }
  missing_columns <- setdiff(required_columns, names(get(object_name)))
  if (length(missing_columns) > 0) {
    stop(
      object_name, " is missing: ", paste(missing_columns, collapse = ", ")
    )
  }
}

# ---- 3. Format the concise main-table estimates ------------------------------

condition_order <- c("Diabetes", "Hypertension", "Chronic kidney disease")

# Two decimals retain the information in the bootstrap CIs for both the
# hospitalisation and low-frequency mortality outcomes.
format_risk_percent <- function(risk) {
  sprintf("%.2f", risk * 100)
}

format_rr_ci <- function(rr, lower, upper) {
  # ASCII hyphen is used deliberately for reliable Word output across locales.
  sprintf("%.2f (%.2f-%.2f)", rr, lower, upper)
}

format_risk_ci <- function(risk, lower, upper) {
  paste0(
    format_risk_percent(risk), " (",
    format_risk_percent(lower), "-", format_risk_percent(upper), ")"
  )
}

make_outcome_rows <- function(summary_table, outcome_label) {
  summary_table <- as.data.frame(summary_table)
  summary_table <- summary_table[match(condition_order, summary_table$condition), ]

  if (anyNA(summary_table$condition)) {
    stop("One or more expected core conditions are absent from: ", outcome_label)
  }

  data.frame(
    # Repeating the label is necessary before merge_v() can create a single
    # vertically centred outcome cell spanning the three conditions.
    outcome = rep(outcome_label, 3),
    condition = summary_table$condition,
    risk_absent = format_risk_ci(
      summary_table$standardised_risk_unexposed,
      summary_table$standardised_risk_unexposed_lower,
      summary_table$standardised_risk_unexposed_upper
    ),
    risk_present = format_risk_ci(
      summary_table$standardised_risk_exposed,
      summary_table$standardised_risk_exposed_lower,
      summary_table$standardised_risk_exposed_upper
    ),
    rr_ci = format_rr_ci(
      summary_table$rr, summary_table$rr_lower, summary_table$rr_upper
    ),
    stringsAsFactors = FALSE
  )
}

main_table <- rbind(
  make_outcome_rows(core3_rr_hosp, "Hospitalisation"),
  make_outcome_rows(core3_rr_death, "Death from chikungunya")
)

# ---- 4. Construct a polished Word table -------------------------------------

ft <- flextable(main_table)
ft <- set_header_labels(
  ft,
  outcome = column_labels[["outcome"]],
  condition = column_labels[["condition"]],
  risk_absent = column_labels[["risk_absent"]],
  risk_present = column_labels[["risk_present"]],
  rr_ci = column_labels[["rr_ci"]]
)

# Merge outcome cells while retaining a clean six-row table structure.
ft <- merge_v(ft, j = "outcome", part = "body")
ft <- valign(ft, j = "outcome", valign = "center", part = "body")

# A restrained, publication-style table: grey header, fine horizontal rules,
# no vertical rules or decorative fills in the body.
ft <- border_remove(ft)
thin_rule <- fp_border(color = "#C9D1D5", width = 0.65)
header_rule <- fp_border(color = "#7E8A90", width = 1.0)
ft <- hline_top(ft, part = "header", border = header_rule)
ft <- hline_bottom(ft, part = "header", border = header_rule)
ft <- hline(ft, i = seq_len(nrow(main_table)), part = "body", border = thin_rule)
ft <- bg(ft, part = "header", bg = "#EEF1F3")
ft <- color(ft, part = "all", color = "#263238")
ft <- font(ft, part = "all", fontname = font_family)
ft <- fontsize(ft, part = "header", size = 8.5)
ft <- fontsize(ft, part = "body", size = 8.25)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, j = c("outcome", "condition"), part = "body", bold = TRUE)
ft <- align(ft, j = c("outcome", "risk_absent", "risk_present", "rr_ci"),
            align = "center", part = "all")
ft <- align(ft, j = "condition", align = "left", part = "all")
ft <- valign(ft, part = "all", valign = "center")
ft <- padding(ft, part = "all", padding.top = 4, padding.bottom = 4,
              padding.left = 5, padding.right = 5)
ft <- height(ft, part = "header", height = 0.42)
ft <- height(ft, part = "body", height = 0.34)

# Fixed widths make the table stable when opened in Word.
ft <- width(ft, j = "outcome", width = 0.90)
ft <- width(ft, j = "condition", width = 1.25)
ft <- width(ft, j = "risk_absent", width = 1.40)
ft <- width(ft, j = "risk_present", width = 1.40)
ft <- width(ft, j = "rr_ci", width = 1.65)
ft <- set_table_properties(
  ft, layout = "fixed",
  opts_word = list(split = FALSE, keep_with_next = TRUE)
)

# ---- 5. Write Word document --------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

doc <- read_docx()
doc <- body_set_default_section(
  doc,
  value = prop_section(
    page_margins = page_mar(top = 0.65, bottom = 0.65, left = 0.60, right = 0.60)
  )
)
doc <- body_add_fpar(
  doc,
  value = fpar(
    ftext(table_title, prop = fp_text(font.family = font_family, bold = FALSE, font.size = 10))
  )
)
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_flextable(doc, value = ft, align = "center")
doc <- body_add_par(doc, value = "", style = "Normal")
doc <- body_add_fpar(
  doc,
  value = fpar(
    ftext(
      "Note. ",
      prop = fp_text(font.family = font_family, italic = TRUE, font.size = 8)
    ),
    ftext(
      table_note,
      prop = fp_text(font.family = font_family, font.size = 8)
    )
  )
)

print(doc, target = output_file)

message("Main manuscript table written to: ", output_file)
print(main_table, row.names = FALSE)
