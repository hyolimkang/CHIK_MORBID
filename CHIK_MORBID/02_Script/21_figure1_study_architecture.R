# -----------------------------------------------------------------------------
# Figure 1. Study architecture: chikungunya severity and underlying conditions
#
# A publication-ready conceptual study-design figure. The title is omitted
# from the artwork so it can be supplied in the manuscript figure caption.
#
# Outputs:
#   03_Output/figures/figure1_study_architecture.pdf
#   03_Output/figures/figure1_study_architecture.svg
#   03_Output/figures/figure1_study_architecture.png  (600 dpi)
#
# Dependencies: R >= 4.1, grid (bundled with R), svglite, ragg.
# Install export packages if needed: install.packages(c("svglite", "ragg"))
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(grid))

# ---- 1. Editable content, style, and output settings ------------------------

labels <- list(
  source = "Source data",
  prioritisation = "Condition prioritisation",
  analyses = "Primary analyses",
  outputs = "Main outputs",

  source_title = "Brazil SINAN surveillance\ndata\n2017-2024",
  source_cases = "Confirmed chikungunya cases",
  source_outcomes = "Outcomes recorded",
  source_outcome_detail = "Hospitalisation\nChikungunya-attributed death",
  source_conditions = "Seven recorded underlying\nconditions",

  review = "Focused prognostic\nevidence review",
  assessment = "Evidence assessed for\nhospitalisation and severe/fatal\noutcomes",
  core_title = "Core conditions\nfor primary models",
  core_note = "Diabetes mellitus; hypertension;\nchronic kidney disease",
  secondary_title = "Secondary / not prioritised\nfor primary models",
  secondary_conditions = paste(
    "Liver disease / hepatopathy",
    "Autoimmune disease",
    "Haematologic disease",
    "Peptic / acid-peptic disease",
    sep = "\n"
  ),

  analysis_1 = "Condition-specific\nstandardised relative risk",
  analysis_1_note = "Core condition present vs absent",
  analysis_2 = "Joint core-3 profile\nanalysis",
  analysis_2_note = "Multimorbidity combinations",
  analysis_3 = "Age-specific risk analysis",
  analysis_3_note = "Hospitalisation and death across age",

  output_1 = "Core-condition\nrelative risk",
  output_2 = "Multimorbidity profile\nrelative risk",
  output_3 = "Age-dependent clinical\nrisk patterns",
  output_3_note = "Hospitalisation and death"
)

# Muted, print-friendly journal palette. Accent fills are reserved for the
# evidence-selected core conditions and the analysis modules.
colours <- list(
  page = "#FFFFFF",
  text = "#263238",
  muted_text = "#617078",
  rule = "#D5DCE0",
  border = "#B8C3C9",
  arrow = "#6D7B82",
  neutral_fill = "#F5F7F8",
  teal_fill = "#E2F0EB",
  teal_border = "#4E8978",
  teal_text = "#326A5B",
  secondary_fill = "#F2F3F4",
  secondary_border = "#C6CDD1",
  blue_fill = "#E8F1F6",
  blue_border = "#5D8197",
  blue_text = "#355E74",
  sage_fill = "#EDF4F1",
  sage_border = "#6C9688",
  grey_fill = "#F3F4F5",
  grey_border = "#9BA7AD",
  white = "#FFFFFF"
)

sizes <- list(
  column_title = 10.2,
  box_title = 8.1,
  box_body = 7.0,
  small = 6.3,
  chip = 7.1,
  line_width = 0.75,
  arrow_width = 0.85
)

font_family <- "Arial"  # Change to "Helvetica" if required by the journal.
canvas <- list(width_mm = 190, height_mm = 120, png_dpi = 600)

output_dir <- file.path("03_Output", "figures")
output_files <- list(
  pdf = file.path(output_dir, "figure1_study_architecture.pdf"),
  svg = file.path(output_dir, "figure1_study_architecture.svg"),
  png = file.path(output_dir, "figure1_study_architecture.png")
)

# ---- 2. Reusable drawing helpers --------------------------------------------

draw_rule <- function(x0, y0, x1, y1, colour = colours$rule,
                      width = sizes$line_width, lty = 1) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = colour, lwd = width, lty = lty, lineend = "round")
  )
}

draw_arrow <- function(x0, y0, x1, y1, colour = colours$arrow,
                       width = sizes$arrow_width, head_mm = 2.0) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = colour, lwd = width, lineend = "round"),
    arrow = arrow(length = unit(head_mm, "mm"), type = "closed")
  )
}

draw_column_heading <- function(x, title) {
  grid.text(
    title, x = unit(x, "npc"), y = unit(0.932, "npc"),
    just = c("centre", "centre"),
    gp = gpar(
      col = colours$text, fontsize = sizes$column_title,
      fontfamily = font_family, fontface = "bold"
    )
  )
  draw_rule(x - 0.098, 0.884, x + 0.098, 0.884, width = 0.7)
}

draw_box <- function(x, y, width, height, title, body = NULL,
                     fill = colours$neutral_fill,
                     border = colours$border,
                     title_colour = colours$text,
                     body_colour = colours$muted_text,
                     title_size = sizes$box_title,
                     body_size = sizes$box_body,
                     title_face = "bold",
                     radius = 0.014) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(radius, "npc"),
    gp = gpar(fill = fill, col = border, lwd = sizes$line_width)
  )

  if (is.null(body) || !nzchar(body)) {
    title_y <- y
  } else {
    title_y <- y + height * 0.19
  }
  grid.text(
    title, x = unit(x, "npc"), y = unit(title_y, "npc"),
    gp = gpar(
      col = title_colour, fontsize = title_size,
      fontfamily = font_family, fontface = title_face, lineheight = 0.96
    )
  )
  if (!is.null(body) && nzchar(body)) {
    grid.text(
      body, x = unit(x, "npc"), y = unit(y - height * 0.20, "npc"),
      gp = gpar(
        col = body_colour, fontsize = body_size,
        fontfamily = font_family, lineheight = 0.98
      )
    )
  }
}

draw_source_card <- function(x, y, width, height) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(0.014, "npc"),
    gp = gpar(fill = colours$neutral_fill, col = colours$border, lwd = sizes$line_width)
  )

  grid.text(
    labels$source_title, x = unit(x, "npc"), y = unit(y + 0.178, "npc"),
    gp = gpar(
      col = colours$text, fontsize = sizes$box_title + 0.2,
      fontfamily = font_family, fontface = "bold", lineheight = 0.96
    )
  )
  draw_rule(x - width * 0.37, y + 0.104, x + width * 0.37, y + 0.104,
            colour = colours$rule, width = 0.55)
  grid.text(
    labels$source_cases, x = unit(x, "npc"), y = unit(y + 0.064, "npc"),
    gp = gpar(col = colours$text, fontsize = sizes$box_body, fontfamily = font_family)
  )
  grid.text(
    labels$source_outcomes, x = unit(x, "npc"), y = unit(y - 0.014, "npc"),
    gp = gpar(
      col = colours$text, fontsize = sizes$small + 0.2,
      fontfamily = font_family, fontface = "bold"
    )
  )
  grid.text(
    labels$source_outcome_detail, x = unit(x, "npc"), y = unit(y - 0.065, "npc"),
    gp = gpar(
      col = colours$muted_text, fontsize = sizes$small,
      fontfamily = font_family, lineheight = 1.03
    )
  )
  draw_rule(x - width * 0.37, y - 0.120, x + width * 0.37, y - 0.120,
            colour = colours$rule, width = 0.55)
  grid.text(
    labels$source_conditions, x = unit(x, "npc"), y = unit(y - 0.171, "npc"),
    gp = gpar(
      col = colours$text, fontsize = sizes$box_body,
      fontfamily = font_family, lineheight = 0.98
    )
  )
}

draw_core_card <- function(x, y, width, height) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(0.015, "npc"),
    gp = gpar(fill = colours$teal_fill, col = colours$teal_border, lwd = 0.9)
  )
  grid.text(
    labels$core_title, x = unit(x, "npc"), y = unit(y + 0.065, "npc"),
    gp = gpar(
      col = colours$teal_text, fontsize = sizes$box_title,
      fontfamily = font_family, fontface = "bold"
    )
  )
  grid.text(
    labels$core_note, x = unit(x, "npc"), y = unit(y + 0.010, "npc"),
    gp = gpar(col = colours$muted_text, fontsize = sizes$small, fontfamily = font_family)
  )

  chip_y <- y - 0.046
  chip_x <- x + c(-0.057, 0, 0.057)
  chip_label <- c("DM", "HTN", "CKD")
  for (i in seq_along(chip_label)) {
    grid.roundrect(
      x = unit(chip_x[i], "npc"), y = unit(chip_y, "npc"),
      width = unit(0.046, "npc"), height = unit(0.050, "npc"),
      r = unit(0.010, "npc"),
      gp = gpar(fill = colours$white, col = colours$teal_border, lwd = 0.7)
    )
    grid.text(
      chip_label[i], x = unit(chip_x[i], "npc"), y = unit(chip_y, "npc"),
      gp = gpar(
        col = colours$teal_text, fontsize = sizes$chip,
        fontfamily = font_family, fontface = "bold"
      )
    )
  }
}

draw_secondary_card <- function(x, y, width, height) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(0.014, "npc"),
    gp = gpar(fill = colours$secondary_fill, col = colours$secondary_border,
              lwd = sizes$line_width)
  )
  grid.text(
    labels$secondary_title, x = unit(x, "npc"), y = unit(y + 0.050, "npc"),
    gp = gpar(
      col = colours$muted_text, fontsize = sizes$small + 0.1,
      fontfamily = font_family, fontface = "bold", lineheight = 0.95
    )
  )
  grid.text(
    labels$secondary_conditions, x = unit(x, "npc"), y = unit(y - 0.036, "npc"),
    gp = gpar(
      col = colours$muted_text, fontsize = sizes$small - 0.3,
      fontfamily = font_family, lineheight = 1.04
    )
  )
}

# ---- 3. Complete figure ------------------------------------------------------

draw_figure <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = colours$page, col = NA))

  # Four evenly weighted columns with whitespace rather than decorative panels.
  x_source <- 0.135
  x_priority <- 0.380
  x_analysis <- 0.625
  x_output <- 0.870
  column_width <- 0.205

  draw_column_heading(x_source, labels$source)
  draw_column_heading(x_priority, labels$prioritisation)
  draw_column_heading(x_analysis, labels$analyses)
  draw_column_heading(x_output, labels$outputs)

  # Column 1: source data.
  source_y <- 0.490
  source_h <- 0.550
  draw_source_card(x_source, source_y, column_width, source_h)

  # Column 2: evidence-informed condition prioritisation.
  draw_box(
    x_priority, 0.755, column_width, 0.110, labels$review,
    fill = colours$neutral_fill, border = colours$border,
    title_size = sizes$box_title - 0.5
  )
  draw_arrow(x_priority, 0.693, x_priority, 0.667)
  draw_box(
    x_priority, 0.610, column_width, 0.115, labels$assessment,
    fill = colours$neutral_fill, border = colours$border,
    title_size = sizes$box_title - 1.1
  )
  draw_arrow(x_priority, 0.550, x_priority, 0.507)
  draw_core_card(x_priority, 0.402, column_width, 0.192)
  draw_rule(x_priority, 0.303, x_priority, 0.261,
            colour = colours$arrow, width = 0.65, lty = 2)
  draw_secondary_card(x_priority, 0.157, column_width, 0.195)

  # Source data feed directly into the evidence review.
  source_right <- x_source + column_width / 2
  priority_left <- x_priority - column_width / 2
  draw_arrow(source_right + 0.004, 0.755, priority_left - 0.004, 0.755)

  # Column 3: three primary analysis modules.
  analysis_x_left <- x_analysis - column_width / 2
  analysis_y <- c(0.716, 0.481, 0.246)
  analysis_fill <- c(colours$blue_fill, colours$sage_fill, colours$grey_fill)
  analysis_border <- c(colours$blue_border, colours$sage_border, colours$grey_border)
  analysis_title_col <- c(colours$blue_text, colours$teal_text, colours$text)
  analysis_titles <- c(labels$analysis_1, labels$analysis_2, labels$analysis_3)
  analysis_notes <- c(labels$analysis_1_note, labels$analysis_2_note, labels$analysis_3_note)
  for (i in seq_along(analysis_y)) {
    draw_box(
      x_analysis, analysis_y[i], column_width, 0.152,
      title = analysis_titles[i], body = analysis_notes[i],
      fill = analysis_fill[i], border = analysis_border[i],
      title_colour = analysis_title_col[i], title_size = sizes$box_title - 0.2,
      body_size = sizes$small + 0.2
    )
  }

  # Core selection feeds all three analyses, shown with a discreet splitter.
  core_right <- x_priority + column_width / 2
  splitter_x <- analysis_x_left - 0.019
  draw_rule(core_right + 0.004, 0.402, splitter_x, 0.402,
            colour = colours$arrow, width = sizes$arrow_width)
  draw_rule(splitter_x, analysis_y[3], splitter_x, analysis_y[1],
            colour = colours$arrow, width = sizes$arrow_width)
  for (y in analysis_y) {
    draw_arrow(splitter_x, y, analysis_x_left - 0.004, y)
  }

  # Column 4: one corresponding output for each analysis module.
  output_x_left <- x_output - column_width / 2
  output_titles <- c(labels$output_1, labels$output_2, labels$output_3)
  output_notes <- c("", "", labels$output_3_note)
  output_fill <- c(colours$white, colours$white, colours$white)
  output_border <- c(colours$blue_border, colours$sage_border, colours$grey_border)
  output_text <- c(colours$blue_text, colours$teal_text, colours$text)
  for (i in seq_along(analysis_y)) {
    draw_box(
      x_output, analysis_y[i], column_width, 0.152,
      title = output_titles[i], body = output_notes[i],
      fill = output_fill[i], border = output_border[i],
      title_colour = output_text[i], title_size = sizes$box_title - 0.1,
      body_size = sizes$small + 0.1
    )
    draw_arrow(
      x_analysis + column_width / 2 + 0.004, analysis_y[i],
      output_x_left - 0.004, analysis_y[i]
    )
  }
}

# ---- 4. Export ---------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
width_in <- canvas$width_mm / 25.4
height_in <- canvas$height_mm / 25.4

grDevices::cairo_pdf(
  filename = output_files$pdf,
  width = width_in, height = height_in,
  family = font_family, onefile = TRUE
)
draw_figure()
invisible(grDevices::dev.off())

if (!requireNamespace("svglite", quietly = TRUE)) {
  stop("Package 'svglite' is required for SVG export. Install it first.")
}
svglite::svglite(
  file = output_files$svg,
  width = width_in, height = height_in,
  bg = colours$page,
  system_fonts = stats::setNames(list(font_family), font_family)
)
draw_figure()
invisible(grDevices::dev.off())

if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required for PNG export. Install it first.")
}
ragg::agg_png(
  filename = output_files$png,
  width = canvas$width_mm, height = canvas$height_mm,
  units = "mm", res = canvas$png_dpi,
  background = colours$page, scaling = 1
)
draw_figure()
invisible(grDevices::dev.off())

message("Study architecture figure written to:")
message("  ", output_files$pdf)
message("  ", output_files$svg)
message("  ", output_files$png, " (", canvas$png_dpi, " dpi)")
