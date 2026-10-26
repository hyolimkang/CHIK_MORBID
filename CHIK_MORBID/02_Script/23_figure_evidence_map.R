# Figure: Evidence map and prioritisation of SINAN-recorded underlying conditions
#
# Uses the completed evidence extraction workbook as the source of truth.
# Bubble size is calculated from DISTINCT evidence families, never publication
# counts. Repeated publications from the same Ceara 2016-2017 surveillance
# dataset are consequently counted once (BRA-B) for each condition/source cell.
#
# Outputs:
#   03_Output/figures/figure_evidence_map.{pdf,svg,png}
#   03_Output/tables/figure_evidence_map_tidy.csv
#   03_Output/tables/figure_evidence_map_study_family_mapping.csv
#   03_Output/tables/figure_evidence_map_bubble_family_map.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(openxlsx)
})

# -----------------------------------------------------------------------------
# 1. Editable settings
# -----------------------------------------------------------------------------
# The first path is the completed workbook supplied for this analysis. Retain a
# repository-local fallback so the script can be reused after copying the file
# into the project root.
workbook_candidates <- c(
  "C:/Users/user/OneDrive/CHIK_MORBID/chik_morbid_evidence_extraction_completed.xlsx",
  "chik_morbid_evidence_extraction_completed.xlsx"
)
workbook_file <- workbook_candidates[file.exists(workbook_candidates)][1]
if (is.na(workbook_file) || length(workbook_file) == 0) {
  stop(
    "Completed evidence extraction workbook not found. Checked:\n  ",
    paste(workbook_candidates, collapse = "\n  ")
  )
}

output_figure_dir <- file.path("03_Output", "figures")
output_table_dir <- file.path("03_Output", "tables")
output_files <- list(
  pdf = file.path(output_figure_dir, "figure_evidence_map.pdf"),
  svg = file.path(output_figure_dir, "figure_evidence_map.svg"),
  png = file.path(output_figure_dir, "figure_evidence_map.png"),
  tidy = file.path(output_table_dir, "figure_evidence_map_tidy.csv"),
  study_map = file.path(output_table_dir, "figure_evidence_map_study_family_mapping.csv"),
  bubble_map = file.path(output_table_dir, "figure_evidence_map_bubble_family_map.csv")
)
dir.create(output_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Arial"
canvas_width_mm <- 190
canvas_height_mm <- 145
png_dpi <- 600

pal <- list(
  ink = "#1F2D35",
  muted = "#60717A",
  rule = "#CDD5D9",
  light_rule = "#E8ECEE",
  supported = "#6E9B83",      # muted green
  limited = "#C39A4B",        # muted amber
  insufficient = "#AAB4B9",   # neutral light grey
  core_fill = "#DCEDE4",
  core_border = "#6E9B83",
  secondary_fill = "#EEF1F2",
  secondary_border = "#9AA7AD",
  not_fill = "#F6F7F7",
  not_border = "#BEC6C9"
)

source_levels <- c(
  "Independent primary evidence",
  "Overlapping Brazilian evidence",
  "Evidence synthesis"
)

conditions <- tibble(
  condition_key = c("diabetes", "hypertension", "ckd", "liver", "autoimmune", "haematological", "peptic"),
  condition = c(
    "Diabetes mellitus", "Hypertension", "Chronic kidney disease",
    "Hepatopathy / liver disease", "Autoimmune disease",
    "Haematological disease", "Peptic ulcer disease"
  ),
  pattern = c(
    "\\bdiabetes|\\bdm\\b",
    "hypertension",
    "\\bckd\\b|chronic kidney disease|chronic renal failure|kidney disease",
    "liver|hepatic|hepatopathy",
    "autoimmune",
    "haematolog|hematolog",
    "peptic"
  ),
  final_role = c("CORE", "CORE", "CORE", "SECONDARY", "SECONDARY", "NOT PRIORITISED", "NOT PRIORITISED")
)

# Strength and compact outcome labels reflect the reviewer classification in the
# workbook. They do not encode the number of publications: counts are derived
# later from evidence_family_id. Cells with no mapped family are automatically
# reset to Insufficient/Sparse below.
assessment_rules <- tribble(
  ~condition_key, ~evidence_source_type,              ~evidence_strength, ~outcome_label,
  "diabetes",       "Independent primary evidence",   "Limited",         "H/D\nmixed",
  "diabetes",       "Overlapping Brazilian evidence", "Supported",       "H/D",
  "diabetes",       "Evidence synthesis",             "Supported",       "S/D",
  "hypertension",   "Independent primary evidence",   "Limited",         "H/S/D\nmixed",
  "hypertension",   "Overlapping Brazilian evidence", "Supported",       "H/D",
  "hypertension",   "Evidence synthesis",             "Supported",       "S/D",
  "ckd",            "Independent primary evidence",   "Limited",         "H\nlimited",
  "ckd",            "Overlapping Brazilian evidence", "Supported",       "H/D",
  "ckd",            "Evidence synthesis",             "Supported",       "H/D",
  "liver",          "Independent primary evidence",   "Insufficient",    "Sparse",
  "liver",          "Overlapping Brazilian evidence", "Limited",         "D\nlimited",
  "liver",          "Evidence synthesis",             "Limited",         "D\nlimited",
  "autoimmune",     "Independent primary evidence",   "Insufficient",    "Sparse",
  "autoimmune",     "Overlapping Brazilian evidence", "Limited",         "H/D\nlimited",
  "autoimmune",     "Evidence synthesis",             "Insufficient",    "Sparse",
  "haematological", "Independent primary evidence",   "Insufficient",    "Sparse",
  "haematological", "Overlapping Brazilian evidence", "Insufficient",    "Sparse",
  "haematological", "Evidence synthesis",             "Insufficient",    "Sparse",
  "peptic",         "Independent primary evidence",   "Insufficient",    "Sparse",
  "peptic",         "Overlapping Brazilian evidence", "Insufficient",    "H\nuncertain",
  "peptic",         "Evidence synthesis",             "Insufficient",    "Sparse"
)

# -----------------------------------------------------------------------------
# 2. Read and classify the evidence extraction workbook
# -----------------------------------------------------------------------------
# The workbook has a title row followed by its data headers in row 2.
raw_extraction <- openxlsx::read.xlsx(
  workbook_file, sheet = "Evidence extraction", startRow = 2, check.names = TRUE
)

# Select fields by position, then assign stable names. This avoids problems with
# hyphen/en-dash characters in Excel header names on different systems.
extraction <- tibble(
  study_id = suppressWarnings(as.integer(raw_extraction[[1]])),
  publication_year = raw_extraction[[2]],
  author = raw_extraction[[3]],
  title = raw_extraction[[4]],
  doi = raw_extraction[[5]],
  inclusion_decision = raw_extraction[[6]],
  study_design = raw_extraction[[9]],
  outcome_assessed = raw_extraction[[12]],
  outcome_category = raw_extraction[[14]],
  conditions_assessed = raw_extraction[[15]],
  comparison_evidence = raw_extraction[[16]],
  acute_relevance = raw_extraction[[19]],
  evidence_role = raw_extraction[[20]],
  evidence_family_note = raw_extraction[[21]],
  include_in_matrix = raw_extraction[[22]],
  key_takeaway = raw_extraction[[23]],
  sinan_overlap = raw_extraction[[25]],
  overlap_group = raw_extraction[[26]],
  independent_replication = raw_extraction[[27]],
  source_url = raw_extraction[[28]]
) |>
  filter(!is.na(study_id))

short_author <- function(x) {
  first_author <- str_trim(str_split_fixed(x, ";", 2)[, 1])
  first_author <- str_replace(first_author, ",.*$", "")
  str_replace(first_author, "\\s+.*$", "")
}

extraction <- extraction |>
  mutate(
    short_citation = paste0(short_author(author), " (", publication_year, ")"),
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
    # A, B and C are the overlap groups defined in the supplied workbook.
    # The three papers in B therefore receive one common family identifier.
    evidence_family_id = case_when(
      evidence_source_type == "Evidence synthesis" ~ paste0("SYN-", study_id),
      evidence_source_type == "Independent primary evidence" ~ paste0("IND-", study_id),
      str_detect(coalesce(overlap_group, ""), "^A\\.") ~ "BRA-A",
      str_detect(coalesce(overlap_group, ""), "^B\\.") ~ "BRA-B",
      str_detect(coalesce(overlap_group, ""), "^C\\.") ~ "BRA-C",
      TRUE ~ paste0("BRA-", study_id)
    ),
    evidence_family_definition = case_when(
      evidence_family_id == "BRA-A" ~ "National SINAN target-overlapping evidence family (studies 3 and 4)",
      evidence_family_id == "BRA-B" ~ "Ceara 2016-2017 surveillance family (studies 13-15; counted once)",
      evidence_family_id == "BRA-C" ~ "Fortaleza 2017 case-control family (partially overlapping surveillance population)",
      evidence_source_type == "Evidence synthesis" ~ "Evidence synthesis; displayed separately from primary evidence",
      evidence_source_type == "Independent primary evidence" ~ "Independent primary-study dataset",
      TRUE ~ "Target-overlapping Brazilian evidence family"
    ),
    condition_text = str_to_lower(paste(
      coalesce(conditions_assessed, ""), coalesce(key_takeaway, ""), sep = " | "
    )),
    workbook_outcome_codes = case_when(
      str_detect(paste(outcome_assessed, outcome_category), regex("hospital", ignore_case = TRUE)) ~ "H",
      TRUE ~ ""
    )
  )

# Match the seven manuscript conditions to the extraction fields. Each row here
# is a study-condition contribution; bubble size is calculated only after
# deduplicating evidence_family_id within each condition/source category.
study_condition_map <- tidyr::crossing(
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
    ) |>
      str_replace_all("(.)(?=.*\\1)", "")
  ) |>
  select(
    condition_key, condition, study_id, short_citation, author, publication_year, title, doi,
    evidence_source_type, evidence_family_id, evidence_family_definition,
    overlap_group, workbook_outcome_codes, outcome_assessed, outcome_category,
    comparison_evidence, evidence_family_note, sinan_overlap, source_url
  ) |>
  arrange(condition_key, evidence_source_type, evidence_family_id, study_id)

# Counts are calculated from distinct family identifiers, not n() of studies.
family_counts <- study_condition_map |>
  distinct(condition_key, evidence_source_type, evidence_family_id) |>
  count(condition_key, evidence_source_type, name = "evidence_family_count")

workbook_codes <- study_condition_map |>
  group_by(condition_key, evidence_source_type) |>
  summarise(
    workbook_outcomes = paste(sort(unique(workbook_outcome_codes[workbook_outcome_codes != ""])), collapse = "; "),
    .groups = "drop"
  )

# One tidy row per condition by evidence-source category, as requested.
figure_tidy <- tidyr::crossing(
  conditions |> select(condition_key, condition, final_role),
  evidence_source_type = source_levels
) |>
  left_join(family_counts, by = c("condition_key", "evidence_source_type")) |>
  left_join(workbook_codes, by = c("condition_key", "evidence_source_type")) |>
  left_join(assessment_rules, by = c("condition_key", "evidence_source_type")) |>
  mutate(
    evidence_family_count = coalesce(evidence_family_count, 0L),
    workbook_outcomes = na_if(workbook_outcomes, ""),
    evidence_strength = if_else(evidence_family_count == 0L, "Insufficient", evidence_strength),
    outcome_label = if_else(evidence_family_count == 0L, "Sparse", outcome_label),
    plot_size = pmax(evidence_family_count, 0.60),
    evidence_source_type = factor(evidence_source_type, levels = source_levels),
    condition = factor(condition, levels = rev(conditions$condition))
  )

stopifnot(
  nrow(figure_tidy) == nrow(conditions) * length(source_levels),
  !anyDuplicated(figure_tidy[c("condition_key", "evidence_source_type")]),
  all(figure_tidy$evidence_strength %in% c("Supported", "Limited", "Insufficient"))
)

# A study-level table provides the exact audit trail from every bubble to its
# extracted publications and shared evidence-family identifier.
study_family_mapping <- study_condition_map |>
  mutate(evidence_source_type = as.character(evidence_source_type)) |>
  arrange(condition_key, evidence_source_type, evidence_family_id, study_id)

bubble_family_mapping <- study_condition_map |>
  group_by(condition_key, condition, evidence_source_type, evidence_family_id, evidence_family_definition) |>
  summarise(
    study_ids = paste(sort(unique(study_id)), collapse = "; "),
    studies = paste(unique(short_citation), collapse = "; "),
    outcome_codes_in_workbook = paste(sort(unique(workbook_outcome_codes[workbook_outcome_codes != ""])), collapse = "; "),
    overlap_groups = paste(unique(na.omit(overlap_group)), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(condition_key, evidence_source_type, evidence_family_id)

write.csv(figure_tidy |> mutate(condition = as.character(condition), evidence_source_type = as.character(evidence_source_type)),
          output_files$tidy, row.names = FALSE, na = "")
write.csv(study_family_mapping, output_files$study_map, row.names = FALSE, na = "")
write.csv(bubble_family_mapping, output_files$bubble_map, row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# 3. Bubble-matrix figure
# -----------------------------------------------------------------------------
strength_colours <- c(
  "Supported" = pal$supported,
  "Limited" = pal$limited,
  "Insufficient" = pal$insufficient
)

role_style <- tibble(
  final_role = c("CORE", "SECONDARY", "NOT PRIORITISED"),
  role_fill = c(pal$core_fill, pal$secondary_fill, pal$not_fill),
  role_border = c(pal$core_border, pal$secondary_border, pal$not_border)
)

plot_data <- figure_tidy |>
  left_join(role_style, by = "final_role") |>
  mutate(
    x = as.numeric(evidence_source_type),
    point_shape = case_when(
      evidence_source_type == "Evidence synthesis" ~ 23,
      TRUE ~ 21
    ),
    # Open circles distinguish target-overlapping surveillance evidence while
    # retaining the same green/amber/grey evidence-strength outline.
    point_fill = if_else(
      evidence_source_type == "Overlapping Brazilian evidence",
      "#FFFFFF", strength_colours[evidence_strength]
    ),
    evidence_colour = strength_colours[evidence_strength],
    text_size = case_when(
      outcome_label == "Sparse" ~ 2.8,
      nchar(gsub("\\n", "", outcome_label)) >= 6 ~ 2.6,
      TRUE ~ 3.2
    )
  )

role_data <- conditions |>
  mutate(
    condition = factor(condition, levels = rev(conditions$condition)),
    x = 4.18,
    role_label = if_else(final_role == "NOT PRIORITISED", "NOT\nPRIORITISED", final_role)
  ) |>
  left_join(role_style, by = "final_role")

source_headers <- tibble(
  x = c(1, 2, 3, 4.18),
  label = c(
    "Independent\nprimary\nevidence",
    "Overlapping\nBrazilian\nevidence",
    "Evidence\nsynthesis",
    "Final analysis\nrole"
  )
)

condition_y <- seq_along(rev(conditions$condition))

figure <- ggplot(plot_data, aes(x = x, y = condition)) +
  geom_hline(
    yintercept = seq(0.5, length(condition_y) + 0.5, by = 1),
    colour = pal$light_rule, linewidth = 0.35
  ) +
  geom_vline(xintercept = c(1.5, 2.5, 3.55), colour = pal$rule, linewidth = 0.42) +
  geom_point(
    aes(size = plot_size, shape = factor(point_shape), fill = point_fill, colour = evidence_colour),
    stroke = 1.05, show.legend = FALSE
  ) +
  geom_text(
    aes(label = outcome_label),
    family = font_family, fontface = "bold", colour = pal$ink,
    size = 2.35, lineheight = 0.86, show.legend = FALSE
  ) +
  geom_label(
    data = role_data,
    aes(x = x, y = condition, label = role_label, fill = role_fill, colour = role_border),
    inherit.aes = FALSE, family = font_family, fontface = "bold", size = 2.95,
    label.size = 0.45, label.r = grid::unit(1.4, "mm"), label.padding = grid::unit(1.8, "mm"),
    show.legend = FALSE
  ) +
  geom_text(
    data = source_headers,
    aes(x = x, y = 7.88, label = label),
    inherit.aes = FALSE, family = font_family, fontface = "bold", size = 2.65,
    colour = pal$ink, lineheight = 0.93
  ) +
  annotate(
    "text", x = 2.56, y = 8.77,
    label = "Evidence map and prioritisation of SINAN-recorded underlying conditions",
    family = font_family, fontface = "bold", size = 3.25, colour = pal$ink
  ) +
  annotate(
    "segment", x = 0.48, xend = 4.65, y = 8.40, yend = 8.40,
    colour = pal$rule, linewidth = 0.45
  ) +
  annotate("point", x = 0.58, y = -0.30, shape = 21, size = 3.8, stroke = 0.9,
           fill = pal$supported, colour = pal$supported) +
  annotate("text", x = 0.78, y = -0.30, label = "independent primary", hjust = 0,
           family = font_family, size = 2.55, colour = pal$muted) +
  annotate("point", x = 1.72, y = -0.30, shape = 21, size = 3.8, stroke = 0.9,
           fill = "white", colour = pal$supported) +
  annotate("text", x = 1.92, y = -0.30, label = "overlapping Brazil", hjust = 0,
           family = font_family, size = 2.55, colour = pal$muted) +
  annotate("point", x = 2.96, y = -0.30, shape = 23, size = 4.3, stroke = 0.9,
           fill = pal$supported, colour = pal$supported) +
  annotate("text", x = 3.18, y = -0.30, label = "evidence synthesis", hjust = 0,
           family = font_family, size = 2.55, colour = pal$muted) +
  annotate("text", x = 0.48, y = -0.76,
           label = "Colour: green = supportive/recurrent; amber = mixed, limited, or outcome-specific;\ngrey = sparse/insufficient.",
           hjust = 0, family = font_family, size = 2.45, colour = pal$muted) +
  annotate("text", x = 0.48, y = -1.34,
           label = "Bubble area represents distinct evidence families, not publications. Overlapping Brazilian surveillance\nstudies and evidence syntheses are shown separately and are not independent replication.",
           hjust = 0, family = font_family, size = 2.40, colour = pal$muted) +
  annotate("text", x = 0.48, y = -1.92,
           label = "H = hospitalisation; S = severe disease; D = death.",
           hjust = 0, family = font_family, size = 2.40, colour = pal$muted) +
  scale_shape_manual(values = c("21" = 21, "23" = 23), guide = "none") +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_size_continuous(range = c(8.5, 18.5), guide = "none") +
  scale_y_discrete(limits = rev(conditions$condition), expand = expansion(add = c(0, 0))) +
  scale_x_continuous(limits = c(0.45, 4.68), breaks = NULL, expand = c(0, 0)) +
  coord_cartesian(ylim = c(-2.28, 9.04), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = font_family) +
  theme(
    plot.margin = margin(t = 9, r = 8, b = 10, l = 38, unit = "mm"),
    axis.text.y = element_text(family = font_family, face = "bold", size = 8.6,
                               colour = pal$ink, hjust = 1),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# -----------------------------------------------------------------------------
# 4. Export and report
# -----------------------------------------------------------------------------
ggsave(output_files$pdf, figure, width = canvas_width_mm, height = canvas_height_mm,
       units = "mm", device = cairo_pdf, dpi = png_dpi, bg = "white")
ggsave(output_files$svg, figure, width = canvas_width_mm, height = canvas_height_mm,
       units = "mm", device = svglite::svglite, bg = "white")
ggsave(output_files$png, figure, width = canvas_width_mm, height = canvas_height_mm,
       units = "mm", device = ragg::agg_png, dpi = png_dpi, bg = "white")

message("\nEvidence families per condition/source cell:\n")
print(figure_tidy |> select(condition, evidence_source_type, evidence_family_count,
                            evidence_strength, outcome_label, final_role), n = Inf)
message("\nSaved figure outputs and audit tables:\n  ", paste(unlist(output_files), collapse = "\n  "), "\n")
