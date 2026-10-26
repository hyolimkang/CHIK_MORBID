# ---------------------------------------------------------------------------
# figure_revision_theme.R
#
# Shared visual language for the revised manuscript figures (scripts 48-50).
# Same typography/geometry as the Figure 5 validation figures (script 47):
# Arial, small type, white background, hairline axes, faint major gridlines.
# Sourced, not run on its own.
# ---------------------------------------------------------------------------

fig_rev_dir <- "03_Output/figure_revision"
if (!dir.exists(fig_rev_dir)) dir.create(fig_rev_dir, recursive = TRUE)

core_profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD",
                         "HTN+CKD", "DM+HTN+CKD")

## Display labels: spaced "+" reads better at small sizes.
profile_display <- c(
  "None" = "None", "DM" = "DM", "HTN" = "HTN", "CKD" = "CKD",
  "DM+HTN" = "DM + HTN", "DM+CKD" = "DM + CKD", "HTN+CKD" = "HTN + CKD",
  "DM+HTN+CKD" = "DM + HTN + CKD"
)

profile_count <- c("None" = 0, "DM" = 1, "HTN" = 1, "CKD" = 1, "DM+HTN" = 2,
                   "DM+CKD" = 2, "HTN+CKD" = 2, "DM+HTN+CKD" = 3)

count_labels <- c("0" = "None", "1" = "One condition", "2" = "Two conditions",
                  "3" = "Three conditions")

## Condition count is ordinal, so it gets one hue stepped light -> dark
## (sequential), with the reference group in neutral grey.
count_colours <- c("0" = "#7A7A7A", "1" = "#5B9BD5", "2" = "#1F5FA8", "3" = "#0B2E59")

## Profile colours for line charts (Okabe-Ito; fixed per profile across all
## figures so a profile never changes colour between figures).
profile_colours <- c(
  "None" = "#5F5F5F", "DM" = "#E69F00", "HTN" = "#0072B2", "CKD" = "#56B4E9",
  "DM+HTN" = "#009E73", "DM+CKD" = "#B8A300", "HTN+CKD" = "#D55E00",
  "DM+HTN+CKD" = "#CC79A7"
)
profile_shapes <- c("None" = 21, "DM" = 24, "HTN" = 22, "CKD" = 23, "DM+HTN" = 25,
                    "DM+CKD" = 4, "HTN+CKD" = 23, "DM+HTN+CKD" = 21)

age_levels <- c("0-14", "15-39", "40-59", "60-74", "75+")
age_labels <- setNames(c("0–14", "15–39", "40–59", "60–74", "≥75"),
                       age_levels)

ink <- "#1A1A1A"
ink_soft <- "#4D4D4D"
ink_muted <- "#8C8C8C"
grid_col <- "#ECECEC"

theme_rev <- function(base_size = 8) {
  ggplot2::theme_classic(base_size = base_size, base_family = "Arial") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "#333333"),
      axis.title = ggplot2::element_text(size = base_size, colour = ink),
      axis.line = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      axis.ticks.length = ggplot2::unit(1.2, "mm"),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.2, colour = grid_col),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size, colour = ink),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1.5,
                                         colour = ink, margin = ggplot2::margin(b = 2)),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(size = base_size - 0.5, colour = ink_soft,
                                            margin = ggplot2::margin(b = 4)),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.5),
      legend.key.size = ggplot2::unit(3.5, "mm"),
      legend.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 6, 4, 4),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

## Panel tag style for patchwork ("A", "B").
tag_theme <- ggplot2::theme(
  plot.tag = ggplot2::element_text(face = "bold", size = 10, family = "Arial", colour = ink)
)

save_figure <- function(plot, stem, width, height, dir = fig_rev_dir,
                        formats = c("pdf", "png", "svg")) {
  if ("pdf" %in% formats) {
    ggplot2::ggsave(file.path(dir, paste0(stem, ".pdf")), plot, width = width,
                    height = height, units = "mm", device = grDevices::cairo_pdf)
  }
  if ("png" %in% formats) {
    ggplot2::ggsave(file.path(dir, paste0(stem, ".png")), plot, width = width,
                    height = height, units = "mm", dpi = 600, bg = "white",
                    device = ragg::agg_png)
  }
  if ("svg" %in% formats) {
    ggplot2::ggsave(file.path(dir, paste0(stem, ".svg")), plot, width = width,
                    height = height, units = "mm", device = svglite::svglite)
  }
}

fmt_ci <- function(est, lo, hi, digits = 2) {
  f <- function(x) formatC(x, format = "f", digits = digits)
  paste0(f(est), " (", f(lo), "–", f(hi), ")")
}

fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
