# Evidence prioritisation matrix for SINAN-recorded underlying conditions.
# Text in the figure is deliberately qualitative; the two exported audit tables
# retain the study-level and evidence-family detail used to construct each cell.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(grid)
  library(openxlsx)
  library(officer)
  library(flextable)
})

# ---- Settings ---------------------------------------------------------------
workbook_candidates <- c(
  "C:/Users/user/OneDrive/CHIK_MORBID/chik_morbid_evidence_extraction_completed.xlsx",
  "chik_morbid_evidence_extraction_completed.xlsx"
)
workbook_file <- workbook_candidates[file.exists(workbook_candidates)][1]
if (is.na(workbook_file) || length(workbook_file) == 0) {
  stop("Completed evidence extraction workbook not found. Checked:\n  ",
       paste(workbook_candidates, collapse = "\n  "))
}

figure_dir <- file.path("03_Output", "figures")
table_dir <- file.path("03_Output", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

outputs <- list(
  pdf = file.path(figure_dir, "figure_evidence_prioritisation_matrix.pdf"),
  svg = file.path(figure_dir, "figure_evidence_prioritisation_matrix.svg"),
  png = file.path(figure_dir, "figure_evidence_prioritisation_matrix.png"),
  pptx = file.path(figure_dir, "figure_evidence_prioritisation_matrix_editable.pptx"),
  cell_summary = file.path(table_dir, "evidence_prioritisation_cell_summary.csv"),
  study_level = file.path(table_dir, "evidence_prioritisation_study_level.csv"),
  family_map = file.path(table_dir, "evidence_prioritisation_family_map.csv")
)

font_family <- "Arial"
canvas_width_mm <- 190
canvas_height_mm <- 142
png_dpi <- 600

cols <- list(
  page = "#FFFFFF", ink = "#202B31", muted = "#596871",
  header_fill = "#F0F3F4", grid = "#D8DEE1", rule = "#B8C3C8",
  core_fill = "#DDEDE5", core_border = "#7B9D89", core_text = "#3E7058",
  secondary_fill = "#EEF2F4", secondary_border = "#AAB7BE", secondary_text = "#53646D",
  not_fill = "#F5F6F6", not_border = "#C7CED1", not_text = "#69767B"
)

title_text <- "Evidence prioritisation of SINAN-recorded underlying conditions"
subtitle_text <- "Evidence was separated into independent primary studies, overlapping Brazilian surveillance studies, and evidence syntheses."
up_arrow <- intToUtf8(0x2191L)
h_inc <- paste("H", up_arrow)
s_inc <- paste("S", up_arrow)
d_inc <- paste("D", up_arrow)

footnotes <- c(
  paste0("H = hospitalisation; S = severe disease; D = death; ", up_arrow, " = evidence of increased risk."),
  "Brazilian surveillance studies overlapping the present SINAN population were retained as relevant evidence;\nthey were not counted as independent external replication.",
  "Evidence syntheses were supportive but not independent replication because they could include primary studies\nrepresented elsewhere in the evidence map.",
  "Final prioritisation reflects overall recurrence and relevance across source types, not publication counts."
)

conditions <- tibble(
  condition_key = c("diabetes", "hypertension", "ckd", "liver", "autoimmune", "haematological", "peptic"),
  condition = c("Diabetes mellitus", "Hypertension", "Chronic kidney disease",
                "Hepatopathy / liver disease", "Autoimmune disease",
                "Haematological disease", "Peptic ulcer disease"),
  pattern = c("\\bdiabetes|\\bdm\\b", "hypertension",
              "\\bckd\\b|chronic kidney disease|chronic renal failure|kidney disease",
              "liver|hepatic|hepatopathy", "autoimmune", "haematolog|hematolog", "peptic"),
  final_role = c("CORE", "CORE", "CORE", "SECONDARY", "SECONDARY",
                 "NOT PRIORITISED", "NOT PRIORITISED")
)

source_levels <- c("Independent primary evidence",
                   "Overlapping Brazilian evidence",
                   "Evidence synthesis")

# Workbook-verified, qualitative cell wording. No cell uses publication counts.
cell_text <- tibble(
  condition_key = rep(conditions$condition_key, each = 3),
  evidence_source_type = rep(source_levels, times = nrow(conditions)),
  evidence_summary = c(
    paste0(h_inc, "\nD imprecise"), paste0(h_inc, "\n", d_inc), d_inc,
    paste0(s_inc, "; H mixed\nD imprecise"), paste0(h_inc, "\n", d_inc), d_inc,
    h_inc, paste0(h_inc, "\n", d_inc), d_inc,
    "No direct\nevidence", d_inc, "Limited",
    "No direct\nevidence", paste0(h_inc, "\n", d_inc), "Sparse",
    "Sparse", "Sparse", "Sparse",
    "Sparse", "H mixed /\nno clear increase", "Sparse"
  )
)

# ---- Read and classify the evidence extraction workbook --------------------
# Workbook row 1 is a title and row 2 contains field names. Positional import
# avoids variable Excel header encodings across local systems.
raw <- openxlsx::read.xlsx(workbook_file, sheet = "Evidence extraction",
                           startRow = 2, check.names = TRUE)

extraction <- tibble(
  study_id = suppressWarnings(as.integer(raw[[1]])),
  publication_year = raw[[2]],
  author = raw[[3]],
  title = raw[[4]],
  doi = raw[[5]],
  study_design = raw[[9]],
  outcome_assessed = raw[[12]],
  outcome_category = raw[[14]],
  conditions_assessed = raw[[15]],
  comparison_evidence = raw[[16]],
  evidence_role = raw[[20]],
  evidence_family_note = raw[[21]],
  include_in_matrix = raw[[22]],
  key_takeaway = raw[[23]],
  sinan_overlap = raw[[25]],
  overlap_group = raw[[26]],
  independent_replication = raw[[27]],
  source_url = raw[[28]]
) |>
  filter(!is.na(study_id)) |>
  mutate(
    short_author = str_trim(str_split_fixed(author, ";", 2)[, 1]),
    short_author = str_replace(short_author, ",.*$", ""),
    short_citation = paste0(short_author, " (", publication_year, ")"),
    is_included = str_detect(coalesce(include_in_matrix, ""), regex("^Yes", ignore_case = TRUE)),
    is_synthesis = str_detect(
      paste(coalesce(study_design, ""), coalesce(evidence_role, ""), sep = " "),
      regex("systematic review|meta-analysis|scoping review|synthesis", ignore_case = TRUE)
    ),
    is_independent = str_detect(
      coalesce(independent_replication, ""), regex("^Yes", ignore_case = TRUE)
    )
  ) |>
  filter(is_included) |>
  mutate(
    evidence_source_type = case_when(
      is_synthesis ~ "Evidence synthesis",
      is_independent ~ "Independent primary evidence",
      TRUE ~ "Overlapping Brazilian evidence"
    ),
    # A, B, and C are the overlap groups in the completed workbook. B contains
    # three publications using the same Ceara 2016-2017 surveillance population.
    evidence_family_id = case_when(
      evidence_source_type == "Evidence synthesis" ~ paste0("SYN-", study_id),
      evidence_source_type == "Independent primary evidence" ~ paste0("IND-", study_id),
      str_detect(coalesce(overlap_group, ""), "^A\\.") ~ "BRA-A",
      str_detect(coalesce(overlap_group, ""), "^B\\.") ~ "BRA-B",
      str_detect(coalesce(overlap_group, ""), "^C\\.") ~ "BRA-C",
      TRUE ~ paste0("BRA-", study_id)
    ),
    evidence_family_definition = case_when(
      evidence_family_id == "BRA-A" ~ "National SINAN target-overlapping family (studies 3 and 4)",
      evidence_family_id == "BRA-B" ~ "Ceara 2016-2017 surveillance family (studies 13-15; counted once)",
      evidence_family_id == "BRA-C" ~ "Fortaleza 2017 case-control family (partially overlapping surveillance population)",
      evidence_source_type == "Evidence synthesis" ~ "Evidence synthesis; supportive but not independent replication",
      TRUE ~ "Independent primary-study dataset"
    ),
    condition_text = str_to_lower(paste(coalesce(conditions_assessed, ""),
                                        coalesce(key_takeaway, ""), sep = " | "))
  )

study_level <- tidyr::crossing(
  extraction,
  conditions |> select(condition_key, condition, pattern)
) |>
  filter(str_detect(condition_text, pattern)) |>
  mutate(
    workbook_outcome_codes = str_c(
      if_else(str_detect(paste(outcome_assessed, outcome_category), regex("hospital", ignore_case = TRUE)), "H", ""),
      if_else(str_detect(paste(outcome_assessed, outcome_category), regex("severe|icu|critical", ignore_case = TRUE)), "S", ""),
      if_else(str_detect(paste(outcome_assessed, outcome_category), regex("death|mortality", ignore_case = TRUE)), "D", ""),
      sep = ""
    )
  ) |>
  select(
    condition_key, condition, evidence_source_type, evidence_family_id,
    evidence_family_definition, study_id, short_citation, author, publication_year,
    title, doi, workbook_outcome_codes, outcome_assessed, outcome_category,
    comparison_evidence, evidence_family_note, sinan_overlap, overlap_group, source_url
  ) |>
  arrange(condition_key, evidence_source_type, evidence_family_id, study_id)

family_map <- study_level |>
  group_by(condition_key, condition, evidence_source_type, evidence_family_id,
           evidence_family_definition) |>
  summarise(
    study_ids = paste(sort(unique(study_id)), collapse = "; "),
    studies = paste(unique(short_citation), collapse = "; "),
    overlap_groups = paste(unique(na.omit(overlap_group)), collapse = "; "),
    outcome_codes_in_workbook = paste(sort(unique(workbook_outcome_codes[workbook_outcome_codes != ""])), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(condition_key, evidence_source_type, evidence_family_id)

family_counts <- family_map |>
  count(condition_key, evidence_source_type, name = "distinct_evidence_family_count")

cell_summary <- tidyr::crossing(
  conditions |> select(condition_key, condition, final_role),
  evidence_source_type = source_levels
) |>
  left_join(cell_text, by = c("condition_key", "evidence_source_type")) |>
  left_join(family_counts, by = c("condition_key", "evidence_source_type")) |>
  mutate(
    distinct_evidence_family_count = coalesce(distinct_evidence_family_count, 0L),
    evidence_source_type = factor(evidence_source_type, levels = source_levels)
  )

stopifnot(
  nrow(cell_summary) == 21L,
  !anyDuplicated(cell_summary[c("condition_key", "evidence_source_type")]),
  all(!is.na(cell_summary$evidence_summary))
)

write.csv(cell_summary |> mutate(evidence_source_type = as.character(evidence_source_type)),
          outputs$cell_summary, row.names = FALSE, na = "")
write.csv(study_level, outputs$study_level, row.names = FALSE, na = "")
write.csv(family_map, outputs$family_map, row.names = FALSE, na = "")

# ---- Build a five-column matrix ---------------------------------------------
matrix_rows <- conditions |>
  select(condition_key, condition, final_role) |>
  left_join(
    cell_summary |>
      select(condition_key, evidence_source_type, evidence_summary) |>
      mutate(evidence_source_type = as.character(evidence_source_type)) |>
      pivot_wider(names_from = evidence_source_type, values_from = evidence_summary),
    by = "condition_key"
  )
names(matrix_rows)[names(matrix_rows) == "Independent primary evidence"] <- "independent"
names(matrix_rows)[names(matrix_rows) == "Overlapping Brazilian evidence"] <- "overlapping"
names(matrix_rows)[names(matrix_rows) == "Evidence synthesis"] <- "synthesis"

role_style <- function(role) {
  switch(
    role,
    "CORE" = list(fill = cols$core_fill, text = cols$core_text),
    "SECONDARY" = list(fill = cols$secondary_fill, text = cols$secondary_text),
    "NOT PRIORITISED" = list(fill = cols$not_fill, text = cols$not_text),
    stop("Unknown final role: ", role)
  )
}

draw_cell <- function(x0, x1, y0, y1, fill = cols$page) {
  grid.rect(
    x = unit((x0 + x1) / 2, "npc"), y = unit((y0 + y1) / 2, "npc"),
    width = unit(x1 - x0, "npc"), height = unit(y1 - y0, "npc"),
    gp = gpar(fill = fill, col = NA)
  )
}

draw_text <- function(label, x0, x1, y0, y1, just = "centre", size = 7.5,
                      col = cols$ink, face = "plain", padding = 0.010,
                      lineheight = 0.98) {
  if (just == "left") {
    x <- x0 + padding
    text_just <- c("left", "centre")
  } else {
    x <- (x0 + x1) / 2
    text_just <- c("centre", "centre")
  }
  grid.text(
    label, x = unit(x, "npc"), y = unit((y0 + y1) / 2, "npc"), just = text_just,
    gp = gpar(fontfamily = font_family, fontsize = size, col = col,
              fontface = face, lineheight = lineheight)
  )
}

draw_rule <- function(x0, y0, x1, y1, col = cols$grid, lwd = 0.45) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = col, lwd = lwd, lineend = "butt")
  )
}

draw_matrix <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = cols$page, col = NA))

  grid.text(
    title_text, x = unit(0.04, "npc"), y = unit(0.966, "npc"),
    just = c("left", "centre"),
    gp = gpar(fontfamily = font_family, fontsize = 11.2, fontface = "bold", col = cols$ink)
  )
  grid.text(
    subtitle_text, x = unit(0.04, "npc"), y = unit(0.932, "npc"),
    just = c("left", "centre"),
    gp = gpar(fontfamily = font_family, fontsize = 6.9, col = cols$muted)
  )

  left <- 0.04
  right <- 0.96
  width <- right - left
  relative_widths <- c(0.205, 0.235, 0.245, 0.175, 0.140)
  x <- left + c(0, cumsum(relative_widths)) * width
  header_top <- 0.888
  header_bottom <- 0.820
  row_height <- 0.0775
  table_bottom <- header_bottom - nrow(matrix_rows) * row_height
  headers <- c("Condition", "Independent primary\nevidence",
               "Overlapping Brazilian\nevidence", "Evidence\nsynthesis",
               "Final analysis\nrole")

  for (j in seq_along(headers)) {
    draw_cell(x[j], x[j + 1], header_bottom, header_top, cols$header_fill)
    draw_text(headers[j], x[j], x[j + 1], header_bottom, header_top,
              just = if (j == 1) "left" else "centre", size = 7.5,
              face = "bold", lineheight = 0.96)
  }

  for (i in seq_len(nrow(matrix_rows))) {
    y1 <- header_bottom - (i - 1) * row_height
    y0 <- y1 - row_height
    body_values <- unlist(matrix_rows[i, c("condition", "independent", "overlapping", "synthesis")])
    if (identical(body_values[1], "Hepatopathy / liver disease")) {
      body_values[1] <- "Hepatopathy /\nliver disease"
    }

    for (j in 1:4) {
      draw_cell(x[j], x[j + 1], y0, y1)
      draw_text(
        body_values[j], x[j], x[j + 1], y0, y1,
        just = if (j == 1) "left" else "centre",
        size = if (j == 1) 8.0 else 7.45,
        face = if (j == 1) "bold" else "plain"
      )
    }

    role <- matrix_rows$final_role[i]
    style <- role_style(role)
    draw_cell(x[5], x[6], y0, y1, style$fill)
    role_label <- ifelse(role == "NOT PRIORITISED", "NOT\nPRIORITISED", role)
    draw_text(role_label, x[5], x[6], y0, y1, size = 7.1,
              col = style$text, face = "bold", lineheight = 0.92)
  }

  for (xx in x) draw_rule(xx, table_bottom, xx, header_top)
  y_rules <- c(header_top, header_bottom,
               header_bottom - seq_len(nrow(matrix_rows)) * row_height)
  for (yy in y_rules) {
    draw_rule(
      left, yy, right, yy,
      col = if (yy %in% c(header_top, header_bottom)) cols$rule else cols$grid,
      lwd = if (yy %in% c(header_top, header_bottom)) 0.65 else 0.42
    )
  }

  footnote_rule_y <- table_bottom - 0.030
  draw_rule(left, footnote_rule_y, right, footnote_rule_y, col = cols$rule, lwd = 0.55)
  footnote_y <- c(footnote_rule_y - 0.028, footnote_rule_y - 0.066,
                  footnote_rule_y - 0.111, footnote_rule_y - 0.152)
  for (i in seq_along(footnotes)) {
    grid.text(
      footnotes[i], x = unit(left, "npc"), y = unit(footnote_y[i], "npc"),
      just = c("left", "centre"),
      gp = gpar(fontfamily = font_family, fontsize = 5.85,
                col = cols$muted, lineheight = 0.98)
    )
  }
}

# ---- Editable PowerPoint ----------------------------------------------------
ppt_data <- matrix_rows |>
  transmute(
    condition = condition,
    independent = independent,
    overlapping = overlapping,
    synthesis = synthesis,
    role = if_else(final_role == "NOT PRIORITISED", "NOT\nPRIORITISED", final_role)
  )
names(ppt_data) <- c("Condition", "Independent primary evidence",
                     "Overlapping Brazilian evidence", "Evidence synthesis",
                     "Final analysis role")

ft <- flextable(ppt_data)
ft <- border_remove(ft)
ft <- bg(ft, part = "header", bg = cols$header_fill)
ft <- color(ft, part = "all", color = cols$ink)
ft <- bold(ft, part = "header", bold = TRUE)
ft <- bold(ft, j = 1, part = "body", bold = TRUE)
ft <- font(ft, fontname = font_family, part = "all")
ft <- fontsize(ft, size = 10, part = "body")
ft <- fontsize(ft, size = 10, part = "header")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- align(ft, j = 2:5, align = "center", part = "all")
ft <- valign(ft, valign = "center", part = "all")
ft <- padding(ft, padding.top = 4, padding.bottom = 4,
              padding.left = 5, padding.right = 5, part = "all")
ft <- width(ft, j = 1, width = 2.05)
ft <- width(ft, j = 2, width = 2.55)
ft <- width(ft, j = 3, width = 2.65)
ft <- width(ft, j = 4, width = 1.80)
ft <- width(ft, j = 5, width = 1.70)
ft <- height(ft, part = "header", height = 0.50)
ft <- height(ft, part = "body", height = 0.50)

for (i in seq_len(nrow(matrix_rows))) {
  style <- role_style(matrix_rows$final_role[i])
  ft <- bg(ft, i = i, j = 5, bg = style$fill, part = "body")
  ft <- color(ft, i = i, j = 5, color = style$text, part = "body")
  ft <- bold(ft, i = i, j = 5, bold = TRUE, part = "body")
}

thin_border <- fp_border(color = cols$grid, width = 0.55)
outer_border <- fp_border(color = cols$rule, width = 0.75)
ft <- border(ft, border = thin_border, part = "all")
ft <- border_outer(ft, border = outer_border, part = "all")

ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- ph_with(
  ppt,
  value = fpar(ftext(title_text, prop = fp_text(font.family = font_family,
                                                  font.size = 17, bold = TRUE,
                                                  color = cols$ink))),
  location = ph_location(left = 0.35, top = 0.20, width = 12.60, height = 0.35)
)
ppt <- ph_with(
  ppt,
  value = fpar(ftext(subtitle_text, prop = fp_text(font.family = font_family,
                                                     font.size = 8.5, color = cols$muted))),
  location = ph_location(left = 0.35, top = 0.58, width = 12.60, height = 0.24)
)
ppt <- ph_with(
  ppt, value = ft,
  location = ph_location(left = 0.35, top = 0.94, width = 12.25, height = 4.15)
)
for (i in seq_along(footnotes)) {
  ppt <- ph_with(
    ppt,
    value = fpar(ftext(footnotes[i], prop = fp_text(font.family = font_family,
                                                      font.size = 7.0, color = cols$muted))),
    location = ph_location(left = 0.35, top = 5.27 + (i - 1) * 0.47,
                           width = 12.50, height = 0.38)
  )
}
print(ppt, target = outputs$pptx)

# ---- Export PDF, SVG, and 600-dpi PNG --------------------------------------
width_in <- canvas_width_mm / 25.4
height_in <- canvas_height_mm / 25.4

grDevices::cairo_pdf(outputs$pdf, width = width_in, height = height_in,
                     family = font_family, onefile = TRUE)
draw_matrix()
invisible(grDevices::dev.off())

svglite::svglite(outputs$svg, width = width_in, height = height_in,
                 bg = cols$page, system_fonts = stats::setNames(list(font_family), font_family))
draw_matrix()
invisible(grDevices::dev.off())

ragg::agg_png(outputs$png, width = canvas_width_mm, height = canvas_height_mm,
               units = "mm", res = png_dpi, background = cols$page)
draw_matrix()
invisible(grDevices::dev.off())

message("\nEvidence prioritisation cell summary:\n")
print(cell_summary |> select(condition, evidence_source_type, evidence_summary,
                             distinct_evidence_family_count, final_role), n = Inf)
message("\nSaved figure, editable PowerPoint, and audit tables:\n  ",
        paste(unlist(outputs), collapse = "\n  "), "\n")
