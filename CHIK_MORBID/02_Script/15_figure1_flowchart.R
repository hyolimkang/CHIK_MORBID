# -----------------------------------------------------------------------------
# Figure 1. Study population and analysis framework
#
# Publication-ready, two-panel flowchart. The title is intentionally omitted
# from the artwork so it can be supplied as the manuscript figure caption.
#
# Outputs:
#   03_Output/figures/figure1_flowchart.pdf
#   03_Output/figures/figure1_flowchart.svg
#   03_Output/figures/figure1_flowchart.png  (600 dpi)
#
# Dependencies: R >= 4.1, grid (bundled with R), svglite, and ragg.
# Install the two export packages if needed with:
#   install.packages(c("svglite", "ragg"))
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(grid))

# ---- 1. Editable content -----------------------------------------------------

# Replace only the bracketed values when final cohort counts are available.
# Keeping counts separate from labels makes updates safe and easy to audit.
counts <- list(
  all_records     = "[insert]",
  confirmed       = "[insert]",
  year_2017_plus  = "[insert]",
  valid_age_sex   = "[insert]",
  core3_known     = "[insert]",
  hospitalisation = "[insert]",
  mortality       = "[insert]"
)

labels <- list(
  panel_a = "Cohort construction",
  panel_b = "Evidence-informed condition selection",

  a1 = "All SINAN chikungunya records,\n2015\u20132024",
  a2 = "Confirmed chikungunya cases",
  a3 = "Notified / onset in 2017 or later",
  a4 = "Valid age and sex",
  a5 = "DM, HTN, and CKD status known",
  a6_title = "Hospitalisation cohort",
  a6_detail = "Hospitalisation status\nknown",
  a7_title = "Mortality cohort",
  a7_detail = "Chikungunya death status\nknown",
  branch_note = "Outcome-specific cohorts constructed independently",

  b1 = "Seven SINAN-recorded\nunderlying conditions",
  b2 = "Focused prognostic\nevidence review",
  b3 = "Assessment of evidence for\nhospitalisation and severe / fatal outcomes",
  core_title = "Primary core conditions",
  core_note = "Prioritised for primary models",
  dm = "DM",
  htn = "HTN",
  ckd = "CKD",
  secondary_title = "Secondary / not prioritised\nfor primary models",
  secondary_items = paste(
    "Liver disease /",
    "hepatopathy",
    "Autoimmune disease",
    "Hematologic disease",
    "Peptic / acid-peptic",
    "disease",
    sep = "\n"
  )
)

# Restrained journal palette. All text/background combinations have strong
# contrast and remain distinguishable if printed in greyscale.
colours <- list(
  page             = "#FFFFFF",
  panel_fill       = "#FFFFFF",
  panel_border     = "#D8DEE2",
  text             = "#263238",
  text_muted       = "#626D73",
  neutral_fill     = "#F5F7F8",
  neutral_border   = "#B8C1C7",
  blue_fill        = "#E6F0F6",
  blue_border      = "#4F7892",
  blue_dark        = "#355F78",
  teal_fill        = "#E3F0EC",
  teal_border      = "#4E8978",
  teal_dark        = "#326A5B",
  secondary_fill   = "#F2F3F4",
  secondary_border = "#C3C9CD",
  arrow            = "#68747A",
  arrow_secondary  = "#A4ADB2",
  white            = "#FFFFFF"
)

sizes <- list(
  panel_letter = 13.0,
  panel_title  = 11.2,
  box_title    = 8.6,
  box_detail   = 7.4,
  count        = 7.4,
  note         = 6.8,
  core_chip    = 9.2,
  secondary    = 6.8,
  line_width   = 0.8,
  arrow_width  = 0.9
)

font_family <- "Arial"  # Change to "Helvetica" if preferred by the journal.

# Full-width journal canvas (190 x 142 mm).
canvas <- list(width_mm = 190, height_mm = 142, png_dpi = 600)

output_dir <- file.path("03_Output", "figures")
output_files <- list(
  pdf = file.path(output_dir, "figure1_flowchart.pdf"),
  svg = file.path(output_dir, "figure1_flowchart.svg"),
  png = file.path(output_dir, "figure1_flowchart.png")
)

# ---- 2. Drawing helpers -----------------------------------------------------

count_line <- function(value) paste0("n = ", value)

draw_round_box <- function(x, y, width, height,
                           title,
                           detail = NULL,
                           count = NULL,
                           fill = colours$neutral_fill,
                           border = colours$neutral_border,
                           title_col = colours$text,
                           detail_col = colours$text_muted,
                           title_size = sizes$box_title,
                           detail_size = sizes$box_detail,
                           count_size = sizes$count,
                           radius = 0.018,
                           border_width = sizes$line_width,
                           title_face = "plain") {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(radius, "npc"),
    gp = gpar(fill = fill, col = border, lwd = border_width)
  )

  has_detail <- !is.null(detail) && nzchar(detail)
  has_count <- !is.null(count) && nzchar(count)

  if (has_detail && has_count) {
    title_y <- y + height * 0.25
    detail_y <- y - height * 0.03
    count_y <- y - height * 0.30
  } else if (has_detail) {
    title_y <- y + height * 0.18
    detail_y <- y - height * 0.22
    count_y <- NA_real_
  } else if (has_count) {
    title_y <- y + height * 0.17
    detail_y <- NA_real_
    count_y <- y - height * 0.25
  } else {
    title_y <- y
    detail_y <- NA_real_
    count_y <- NA_real_
  }

  grid.text(
    title, x = unit(x, "npc"), y = unit(title_y, "npc"),
    gp = gpar(
      col = title_col, fontsize = title_size,
      fontfamily = font_family, fontface = title_face,
      lineheight = 0.96
    )
  )

  if (has_detail) {
    grid.text(
      detail, x = unit(x, "npc"), y = unit(detail_y, "npc"),
      gp = gpar(
        col = detail_col, fontsize = detail_size,
        fontfamily = font_family, lineheight = 0.96
      )
    )
  }

  if (has_count) {
    grid.text(
      count_line(count), x = unit(x, "npc"), y = unit(count_y, "npc"),
      gp = gpar(
        col = detail_col, fontsize = count_size,
        fontfamily = font_family, fontface = "bold"
      )
    )
  }
}

draw_arrow <- function(x0, y0, x1, y1,
                       colour = colours$arrow,
                       width = sizes$arrow_width,
                       head_mm = 2.1) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = colour, lwd = width, lineend = "round"),
    arrow = arrow(length = unit(head_mm, "mm"), type = "closed")
  )
}

draw_line <- function(x0, y0, x1, y1,
                      colour = colours$arrow,
                      width = sizes$arrow_width) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    gp = gpar(col = colour, lwd = width, lineend = "round")
  )
}

draw_panel_heading <- function(letter, title) {
  grid.text(
    letter, x = unit(0.045, "npc"), y = unit(0.952, "npc"),
    just = c("left", "centre"),
    gp = gpar(
      col = colours$text, fontsize = sizes$panel_letter,
      fontfamily = font_family, fontface = "bold"
    )
  )
  grid.text(
    title, x = unit(0.105, "npc"), y = unit(0.952, "npc"),
    just = c("left", "centre"),
    gp = gpar(
      col = colours$text, fontsize = sizes$panel_title,
      fontfamily = font_family, fontface = "bold"
    )
  )
  draw_line(0.045, 0.915, 0.955, 0.915,
            colour = colours$panel_border, width = 0.7)
}

draw_panel_frame <- function(x, y, width, height) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(width, "npc"), height = unit(height, "npc"),
    r = unit(2.2, "mm"),
    gp = gpar(
      fill = colours$panel_fill,
      col = colours$panel_border,
      lwd = 0.8
    )
  )
}

# ---- 3. Panel A: cohort construction ---------------------------------------

draw_panel_a <- function() {
  draw_panel_heading("A", labels$panel_a)

  box_x <- 0.50
  box_w <- 0.74
  box_h <- 0.090
  ys <- c(0.842, 0.710, 0.578, 0.446, 0.314)

  titles <- c(labels$a1, labels$a2, labels$a3, labels$a4, labels$a5)
  ns <- c(
    counts$all_records, counts$confirmed, counts$year_2017_plus,
    counts$valid_age_sex, counts$core3_known
  )

  # First five sequential eligibility steps.
  for (i in seq_along(ys)) {
    draw_round_box(
      box_x, ys[i], box_w, box_h,
      title = titles[i], count = ns[i],
      fill = colours$neutral_fill,
      border = colours$neutral_border
    )
    if (i < length(ys)) {
      draw_arrow(
        box_x, ys[i] - box_h / 2 - 0.006,
        box_x, ys[i + 1] + box_h / 2 + 0.006
      )
    }
  }

  # Independent outcome-specific branch.
  branch_y <- 0.202
  left_x <- 0.265
  right_x <- 0.735
  final_w <- 0.415
  final_h <- 0.116
  final_y <- 0.085

  draw_line(box_x, ys[5] - box_h / 2 - 0.006, box_x, branch_y)
  draw_line(left_x, branch_y, right_x, branch_y)
  draw_arrow(left_x, branch_y, left_x, final_y + final_h / 2 + 0.006)
  draw_arrow(right_x, branch_y, right_x, final_y + final_h / 2 + 0.006)

  # Small white-backed note makes the independence explicit without clutter.
  grid.roundrect(
    x = unit(0.50, "npc"), y = unit(branch_y, "npc"),
    width = unit(0.43, "npc"), height = unit(0.037, "npc"),
    r = unit(0.010, "npc"),
    gp = gpar(fill = colours$white, col = NA)
  )
  grid.text(
    labels$branch_note,
    x = unit(0.50, "npc"), y = unit(branch_y, "npc"),
    gp = gpar(
      col = colours$text_muted, fontsize = sizes$note,
      fontfamily = font_family, fontface = "italic"
    )
  )

  draw_round_box(
    left_x, final_y, final_w, final_h,
    title = labels$a6_title,
    detail = labels$a6_detail,
    count = counts$hospitalisation,
    fill = colours$blue_fill,
    border = colours$blue_border,
    title_col = colours$blue_dark,
    detail_size = sizes$box_detail - 0.5,
    title_face = "bold"
  )
  draw_round_box(
    right_x, final_y, final_w, final_h,
    title = labels$a7_title,
    detail = labels$a7_detail,
    count = counts$mortality,
    fill = colours$blue_fill,
    border = colours$blue_border,
    title_col = colours$blue_dark,
    detail_size = sizes$box_detail - 0.5,
    title_face = "bold"
  )
}

# ---- 4. Panel B: evidence-informed selection -------------------------------

draw_core_chip <- function(x, label) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(0.142, "npc"),
    width = unit(0.145, "npc"), height = unit(0.065, "npc"),
    r = unit(0.014, "npc"),
    gp = gpar(fill = colours$white, col = colours$teal_border, lwd = 0.8)
  )
  grid.text(
    label, x = unit(x, "npc"), y = unit(0.142, "npc"),
    gp = gpar(
      col = colours$teal_dark, fontsize = sizes$core_chip,
      fontfamily = font_family, fontface = "bold"
    )
  )
}

draw_panel_b <- function() {
  draw_panel_heading("B", labels$panel_b)

  # Evidence pathway.
  draw_round_box(
    0.50, 0.822, 0.72, 0.105,
    title = labels$b1,
    fill = colours$neutral_fill,
    border = colours$neutral_border
  )
  draw_arrow(0.50, 0.763, 0.50, 0.722)

  draw_round_box(
    0.50, 0.664, 0.72, 0.105,
    title = labels$b2,
    fill = colours$neutral_fill,
    border = colours$neutral_border
  )
  draw_arrow(0.50, 0.605, 0.50, 0.564)

  draw_round_box(
    0.50, 0.500, 0.80, 0.115,
    title = labels$b3,
    fill = colours$neutral_fill,
    border = colours$neutral_border
  )

  # The evidence assessment leads to a prominent primary set and a visibly
  # subordinate secondary set. Different arrow tones reinforce that hierarchy.
  branch_y <- 0.401
  primary_x <- 0.330
  secondary_x <- 0.780
  draw_line(0.50, 0.436, 0.50, branch_y)
  draw_line(primary_x, branch_y, secondary_x, branch_y)
  draw_arrow(primary_x, branch_y, primary_x, 0.343)
  draw_arrow(
    secondary_x, branch_y, secondary_x, 0.343,
    colour = colours$arrow_secondary, width = 0.75, head_mm = 1.8
  )

  # Primary core-condition card.
  grid.roundrect(
    x = unit(primary_x, "npc"), y = unit(0.205, "npc"),
    width = unit(0.54, "npc"), height = unit(0.265, "npc"),
    r = unit(0.018, "npc"),
    gp = gpar(fill = colours$teal_fill, col = colours$teal_border, lwd = 1.0)
  )
  grid.text(
    labels$core_title,
    x = unit(primary_x, "npc"), y = unit(0.286, "npc"),
    gp = gpar(
      col = colours$teal_dark, fontsize = sizes$box_title + 0.4,
      fontfamily = font_family, fontface = "bold"
    )
  )
  grid.text(
    labels$core_note,
    x = unit(primary_x, "npc"), y = unit(0.246, "npc"),
    gp = gpar(
      col = colours$text_muted, fontsize = sizes$note,
      fontfamily = font_family
    )
  )
  draw_core_chip(0.150, labels$dm)
  draw_core_chip(0.330, labels$htn)
  draw_core_chip(0.510, labels$ckd)

  # Secondary-condition card: readable, but deliberately lower emphasis.
  grid.roundrect(
    x = unit(secondary_x, "npc"), y = unit(0.205, "npc"),
    width = unit(0.34, "npc"), height = unit(0.265, "npc"),
    r = unit(0.018, "npc"),
    gp = gpar(
      fill = colours$secondary_fill,
      col = colours$secondary_border,
      lwd = 0.8
    )
  )
  grid.text(
    labels$secondary_title,
    x = unit(secondary_x, "npc"), y = unit(0.284, "npc"),
    gp = gpar(
      col = colours$text_muted, fontsize = sizes$secondary + 0.2,
      fontfamily = font_family, fontface = "bold", lineheight = 0.94
    )
  )
  grid.text(
    labels$secondary_items,
    x = unit(secondary_x, "npc"), y = unit(0.171, "npc"),
    gp = gpar(
      col = colours$text_muted, fontsize = sizes$secondary - 0.2,
      fontfamily = font_family, lineheight = 1.07
    )
  )
}

# ---- 5. Complete figure -----------------------------------------------------

draw_figure <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = colours$page, col = NA))

  # Balanced horizontal layout with a narrow inter-panel gutter.
  panel_y <- 0.50
  panel_h <- 0.955
  panel_w <- 0.472
  panel_a_x <- 0.252
  panel_b_x <- 0.748

  draw_panel_frame(panel_a_x, panel_y, panel_w, panel_h)
  draw_panel_frame(panel_b_x, panel_y, panel_w, panel_h)

  pushViewport(viewport(
    x = unit(panel_a_x, "npc"), y = unit(panel_y, "npc"),
    width = unit(panel_w, "npc"), height = unit(panel_h, "npc")
  ))
  draw_panel_a()
  popViewport()

  pushViewport(viewport(
    x = unit(panel_b_x, "npc"), y = unit(panel_y, "npc"),
    width = unit(panel_w, "npc"), height = unit(panel_h, "npc")
  ))
  draw_panel_b()
  popViewport()
}

# ---- 6. Export --------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

width_in <- canvas$width_mm / 25.4
height_in <- canvas$height_mm / 25.4

# cairo_pdf embeds a vector figure and handles anti-aliased text reliably.
grDevices::cairo_pdf(
  filename = output_files$pdf,
  width = width_in, height = height_in,
  family = font_family, onefile = TRUE
)
draw_figure()
invisible(grDevices::dev.off())

# svglite writes standards-compliant editable SVG vector output.
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

# ragg provides publication-quality anti-aliasing for the 600 dpi raster copy.
if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required for PNG export. Install it first.")
}
ragg::agg_png(
  filename = output_files$png,
  width = canvas$width_mm, height = canvas$height_mm,
  units = "mm", res = canvas$png_dpi,
  background = colours$page,
  scaling = 1
)
draw_figure()
invisible(grDevices::dev.off())

message("Figure 1 exports written to:")
message("  ", output_files$pdf)
message("  ", output_files$svg)
message("  ", output_files$png, " (", canvas$png_dpi, " dpi)")
