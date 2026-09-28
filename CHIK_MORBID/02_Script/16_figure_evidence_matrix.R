# -----------------------------------------------------------------------------
# Evidence matrix: chikungunya severity and underlying conditions in Brazil
#
# A publication-ready table figure. The manuscript caption (rather than the
# artwork) should supply the figure title, e.g.:
#   "Evidence-informed prioritisation of underlying conditions for models of
#    chikungunya severity in Brazil."
#
# Outputs:
#   03_Output/figures/figure_evidence_matrix.pdf
#   03_Output/figures/figure_evidence_matrix.svg
#   03_Output/figures/figure_evidence_matrix.png  (600 dpi)
#
# Dependencies: R >= 4.1, grid (bundled with R), svglite, and ragg.
# Install the two export packages if needed with:
#   install.packages(c("svglite", "ragg"))
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(grid))

# ---- 1. Editable figure content ---------------------------------------------

# Edit evidence classifications, reference codes, and rationales here. Evidence
# categories must be one of: "Supported", "Limited", or "Insufficient".
matrix_rows <- data.frame(
  condition = c(
    "Diabetes",
    "Hypertension",
    "Chronic kidney\ndisease",
    "Liver disease /\nhepatopathy",
    "Autoimmune disease",
    "Haematologic\ndisease",
    "Peptic / acid-peptic\ndisease"
  ),
  hospitalisation = c(
    "Supported", "Supported", "Supported", "Insufficient",
    "Limited", "Insufficient", "Insufficient"
  ),
  severe_fatal = c(
    "Supported", "Limited", "Supported", "Limited",
    "Insufficient", "Insufficient", "Insufficient"
  ),
  analysis_role = c(
    "CORE", "CORE", "CORE", "SECONDARY", "SECONDARY",
    "NOT\nPRIORITISED", "NOT\nPRIORITISED"
  ),
  evidence_refs = c(
    "A, B, D,\nE, F, G", "A, B, C,\nD, E", "A, B, D,\nE, G", "E", "E", "G", "G"
  ),
  key_rationale = c(
    "Consistent signals for\nhospitalisation and mortality",
    "Strong hospitalisation signal;\nlimited severe/fatal evidence",
    "Largest risk signal; evidence\nchiefly from Brazil",
    "Outcome-specific death signal\nin Brazilian national data",
    "Limited association with\nhospital admission",
    "Sparse and heterogeneous\nprognostic evidence",
    "Very limited prognostic\nevidence"
  ),
  stringsAsFactors = FALSE
)

column_labels <- c(
  "Condition",
  "Hospitalisation\nevidence",
  "Severe / fatal\nevidence",
  "Analysis\nrole",
  "Evidence\nrefs",
  "Key rationale"
)

footnotes <- c(
  "Evidence categories: Supported = consistent evidence of association; Limited = suggestive, outcome-specific, or sparse evidence; Insufficient = little or no prognostic evidence.",
  "Operational prioritisation framework informed by a focused review; not a formal GRADE assessment. Evidence references are coded A through G."
)

# Figure palette: traffic-light fills are restricted to evidence assessments.
# Analysis roles deliberately use typography only, avoiding a second coding
# system that could compete with the three evidence categories.
colours <- list(
  page = "#FFFFFF",
  header_fill = "#EDF0F2",
  header_text = "#263238",
  text = "#263238",
  muted_text = "#5F6B71",
  grid = "#D2D9DD",
  rule = "#B9C3C8",
  supported_fill = "#DDEDE5",
  supported_border = "#7FA08F",
  supported_text = "#2F6654",
  limited_fill = "#F4E8C8",
  limited_border = "#C5A25E",
  limited_text = "#7C5A1C",
  insufficient_fill = "#F4DEDE",
  insufficient_border = "#BE8585",
  insufficient_text = "#7B4242"
)

sizes <- list(
  header = 8.2,
  body = 8.0,
  rationale = 7.5,
  evidence = 7.6,
  role = 7.1,
  refs = 7.2,
  footnote = 6.5,
  grid_width = 0.55,
  rule_width = 0.7
)

font_family <- "Arial"  # Change to "Helvetica" if required by the journal.

# Relative column widths (sum must equal 1).
column_widths <- c(0.174, 0.126, 0.126, 0.130, 0.105, 0.339)
stopifnot(abs(sum(column_widths) - 1) < 1e-9)

# Full-width journal canvas and output locations.
canvas <- list(width_mm = 190, height_mm = 128, png_dpi = 600)
output_dir <- file.path("03_Output", "figures")
output_files <- list(
  pdf = file.path(output_dir, "figure_evidence_matrix.pdf"),
  svg = file.path(output_dir, "figure_evidence_matrix.svg"),
  png = file.path(output_dir, "figure_evidence_matrix.png")
)

# ---- 2. Reusable drawing helpers --------------------------------------------

evidence_style <- function(category) {
  switch(
    category,
    "Supported" = list(
      fill = colours$supported_fill, border = colours$supported_border,
      text = colours$supported_text
    ),
    "Limited" = list(
      fill = colours$limited_fill, border = colours$limited_border,
      text = colours$limited_text
    ),
    "Insufficient" = list(
      fill = colours$insufficient_fill, border = colours$insufficient_border,
      text = colours$insufficient_text
    ),
    stop("Unknown evidence category: ", category)
  )
}

draw_cell <- function(x_left, x_right, y_bottom, y_top,
                      fill = colours$page, border = NA, border_width = 0.5) {
  grid.rect(
    x = unit((x_left + x_right) / 2, "npc"),
    y = unit((y_bottom + y_top) / 2, "npc"),
    width = unit(x_right - x_left, "npc"),
    height = unit(y_top - y_bottom, "npc"),
    gp = gpar(fill = fill, col = border, lwd = border_width)
  )
}

draw_cell_text <- function(label, x_left, x_right, y_bottom, y_top,
                           just = "centre", fontsize = sizes$body,
                           colour = colours$text, fontface = "plain",
                           left_padding = 0.010, lineheight = 1.02) {
  if (identical(just, "left")) {
    x <- x_left + left_padding
    text_just <- c("left", "centre")
  } else {
    x <- (x_left + x_right) / 2
    text_just <- c("centre", "centre")
  }
  grid.text(
    label,
    x = unit(x, "npc"), y = unit((y_bottom + y_top) / 2, "npc"),
    just = text_just,
    gp = gpar(
      col = colour, fontsize = fontsize, fontfamily = font_family,
      fontface = fontface, lineheight = lineheight
    )
  )
}

draw_table_rule <- function(x0, y0, x1, y1, width = sizes$grid_width,
                            colour = colours$grid) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = colour, lwd = width, lineend = "butt")
  )
}

# ---- 3. Evidence matrix ------------------------------------------------------

draw_evidence_matrix <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = colours$page, col = NA))

  # Table geometry. No figure title is drawn; use the manuscript caption.
  table_left <- 0.030
  table_right <- 0.970
  header_top <- 0.925
  header_bottom <- 0.847
  row_height <- 0.0875
  table_width <- table_right - table_left
  x_breaks <- table_left + c(0, cumsum(column_widths)) * table_width

  # Header cells.
  for (j in seq_along(column_labels)) {
    draw_cell(
      x_breaks[j], x_breaks[j + 1], header_bottom, header_top,
      fill = colours$header_fill
    )
    header_just <- if (j %in% c(1, 6)) "left" else "centre"
    draw_cell_text(
      column_labels[j], x_breaks[j], x_breaks[j + 1], header_bottom, header_top,
      just = header_just, fontsize = sizes$header,
      colour = colours$header_text, fontface = "bold", lineheight = 0.94
    )
  }

  # Body cells. Evidence fills are applied only to the two evidence columns.
  for (i in seq_len(nrow(matrix_rows))) {
    y_top <- header_bottom - (i - 1) * row_height
    y_bottom <- y_top - row_height

    # Standard white cells: condition, role, refs, rationale.
    for (j in c(1, 4, 5, 6)) {
      draw_cell(x_breaks[j], x_breaks[j + 1], y_bottom, y_top)
    }

    # Muted traffic-light evidence cells.
    hosp_style <- evidence_style(matrix_rows$hospitalisation[i])
    draw_cell(
      x_breaks[2], x_breaks[3], y_bottom, y_top,
      fill = hosp_style$fill, border = hosp_style$border
    )
    severe_style <- evidence_style(matrix_rows$severe_fatal[i])
    draw_cell(
      x_breaks[3], x_breaks[4], y_bottom, y_top,
      fill = severe_style$fill, border = severe_style$border
    )

    draw_cell_text(
      matrix_rows$condition[i], x_breaks[1], x_breaks[2], y_bottom, y_top,
      just = "left", fontsize = sizes$body, fontface = "bold", lineheight = 0.97
    )
    draw_cell_text(
      matrix_rows$hospitalisation[i], x_breaks[2], x_breaks[3], y_bottom, y_top,
      fontsize = sizes$evidence, colour = hosp_style$text, fontface = "bold"
    )
    draw_cell_text(
      matrix_rows$severe_fatal[i], x_breaks[3], x_breaks[4], y_bottom, y_top,
      fontsize = sizes$evidence, colour = severe_style$text, fontface = "bold"
    )
    draw_cell_text(
      matrix_rows$analysis_role[i], x_breaks[4], x_breaks[5], y_bottom, y_top,
      fontsize = sizes$role, colour = colours$text, fontface = "bold", lineheight = 0.95
    )
    draw_cell_text(
      matrix_rows$evidence_refs[i], x_breaks[5], x_breaks[6], y_bottom, y_top,
      fontsize = sizes$refs, colour = colours$muted_text, lineheight = 0.98
    )
    draw_cell_text(
      matrix_rows$key_rationale[i], x_breaks[6], x_breaks[7], y_bottom, y_top,
      just = "left", fontsize = sizes$rationale, colour = colours$text,
      lineheight = 0.98
    )
  }

  # Fine rules are placed on top of fills for a coherent, journal-style grid.
  table_bottom <- header_bottom - nrow(matrix_rows) * row_height
  for (x in x_breaks) {
    draw_table_rule(x, table_bottom, x, header_top)
  }
  y_breaks <- c(header_top, header_bottom,
                header_bottom - seq_len(nrow(matrix_rows)) * row_height)
  for (y in y_breaks) {
    is_header_rule <- y %in% c(header_top, header_bottom)
    draw_table_rule(
      table_left, y, table_right, y,
      width = if (is_header_rule) sizes$rule_width else sizes$grid_width,
      colour = if (is_header_rule) colours$rule else colours$grid
    )
  }

  # Compact explanatory footnote, separated from the table by a fine rule.
  footnote_rule_y <- 0.160
  draw_table_rule(table_left, footnote_rule_y, table_right, footnote_rule_y,
                  width = sizes$rule_width, colour = colours$rule)
  grid.text(
    footnotes[1], x = unit(table_left, "npc"), y = unit(0.119, "npc"),
    just = c("left", "centre"),
    gp = gpar(
      col = colours$muted_text, fontsize = sizes$footnote,
      fontfamily = font_family
    )
  )
  grid.text(
    footnotes[2], x = unit(table_left, "npc"), y = unit(0.075, "npc"),
    just = c("left", "centre"),
    gp = gpar(
      col = colours$muted_text, fontsize = sizes$footnote,
      fontfamily = font_family, fontface = "italic"
    )
  )
}

# ---- 4. Export ---------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
width_in <- canvas$width_mm / 25.4
height_in <- canvas$height_mm / 25.4

# Vector PDF export.
grDevices::cairo_pdf(
  filename = output_files$pdf,
  width = width_in, height = height_in,
  family = font_family, onefile = TRUE
)
draw_evidence_matrix()
invisible(grDevices::dev.off())

# Vector SVG export.
if (!requireNamespace("svglite", quietly = TRUE)) {
  stop("Package 'svglite' is required for SVG export. Install it first.")
}
svglite::svglite(
  file = output_files$svg,
  width = width_in, height = height_in,
  bg = colours$page,
  system_fonts = stats::setNames(list(font_family), font_family)
)
draw_evidence_matrix()
invisible(grDevices::dev.off())

# High-resolution PNG export.
if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required for PNG export. Install it first.")
}
ragg::agg_png(
  filename = output_files$png,
  width = canvas$width_mm, height = canvas$height_mm,
  units = "mm", res = canvas$png_dpi,
  background = colours$page, scaling = 1
)
draw_evidence_matrix()
invisible(grDevices::dev.off())

message("Evidence matrix exports written to:")
message("  ", output_files$pdf)
message("  ", output_files$svg)
message("  ", output_files$png, " (", canvas$png_dpi, " dpi)")
