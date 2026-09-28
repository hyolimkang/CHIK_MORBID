# Figure: Cohort selection from Brazilian SINAN chikungunya surveillance data
#
# Reproduces the filters in 13_core3_descriptive.R and derives all figure Ns
# from the current individual-level data. The final Ns are independently
# checked against the filtering logic used in 12_core3_condition_specific_rr.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(grid)
})

# -----------------------------------------------------------------------------
# User-editable settings
# -----------------------------------------------------------------------------
data_file <- file.path("01_Data", "chik_sinan_individual_2015_2025.rds")
figure_dir <- file.path("03_Output", "figures")
table_dir <- file.path("03_Output", "tables")
output_pdf <- file.path(figure_dir, "figure_cohort_flow.pdf")
output_svg <- file.path(figure_dir, "figure_cohort_flow.svg")
output_png <- file.path(figure_dir, "figure_cohort_flow.png")
output_attrition <- file.path(table_dir, "figure_cohort_flow_attrition.csv")

font_family <- "Arial"  # A widely available journal-style sans-serif font.
figure_width_mm <- 190
figure_height_mm <- 222
core_cols <- c("diabetes", "hypertension", "renal_disease")

cols <- list(
  ink = "#17252F", line = "#6D8796",
  retained_fill = "#DCEBF2", retained_key_fill = "#C9E0EB",
  retained_border = "#5B7B8C",
  excluded_fill = "#F1F3F4", excluded_border = "#9AA5AC",
  excluded_text = "#39444B", note = "#5C6970"
)

if (!file.exists(data_file)) {
  stop("Data file not found: ", normalizePath(data_file, winslash = "/", mustWork = FALSE))
}
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Cohort construction: exact filters from 13_core3_descriptive.R
# -----------------------------------------------------------------------------
ind <- readRDS(data_file)

cohort_raw <- ind
cohort_confirmed <- cohort_raw |> filter(is_confirmed_chik)
cohort_2017 <- cohort_confirmed |> filter(year(event_date) >= 2017)
cohort_valid <- cohort_2017 |> filter(
  !is.na(age_years), age_years >= 0, age_years <= 100,
  sex %in% c("male", "female")
)
cohort_core3 <- cohort_valid |> filter(
  if_all(all_of(core_cols), ~ .x %in% c("no", "yes"))
)

# These outcome cohorts intentionally start from the same core-3 population.
cohort_hosp <- cohort_core3 |> filter(hospitalised %in% c("no", "yes"))
cohort_death <- cohort_core3 |> filter(!is.na(died_from_chik))

# -----------------------------------------------------------------------------
# Validation against the filtering logic in 12_core3_condition_specific_rr.R
# -----------------------------------------------------------------------------
prepare_as_script12 <- function(dat) {
  dat |>
    filter(is_confirmed_chik) |>
    filter(year(event_date) >= 2017) |>
    filter(!is.na(age_years), age_years >= 0, age_years <= 100,
           sex %in% c("male", "female")) |>
    filter(if_all(all_of(core_cols), ~ .x %in% c("no", "yes")))
}

script12_base <- prepare_as_script12(ind)
script12_hosp_n <- script12_base |> filter(hospitalised %in% c("no", "yes")) |> nrow()
script12_death_n <- script12_base |> filter(!is.na(died_from_chik)) |> nrow()
stopifnot(nrow(cohort_hosp) == script12_hosp_n,
          nrow(cohort_death) == script12_death_n)

# The last two rows share cohort_core3 as their parent, not each other.
attrition <- tibble(
  step = c(
    "All SINAN chikungunya records, 2015-2025",
    "Confirmed chikungunya cases",
    "Notified/onset in 2017 or later",
    "Valid age and sex",
    "DM, HTN, and CKD status known (core-3 complete case)",
    "Hospitalisation cohort: status known",
    "Mortality cohort: chikungunya death status known"
  ),
  parent_n = c(NA_integer_, nrow(cohort_raw), nrow(cohort_confirmed),
               nrow(cohort_2017), nrow(cohort_valid), nrow(cohort_core3),
               nrow(cohort_core3)),
  retained_n = c(nrow(cohort_raw), nrow(cohort_confirmed), nrow(cohort_2017),
                 nrow(cohort_valid), nrow(cohort_core3), nrow(cohort_hosp),
                 nrow(cohort_death)),
  exclusion_reason = c(
    NA_character_, "Not confirmed chikungunya",
    "Before 2017 or missing event date", "Invalid/missing age or sex",
    "At least one core-condition field unknown/missing",
    "Hospitalisation status unknown", "Chikungunya death status unknown"
  )
) |>
  mutate(excluded_n = if_else(is.na(parent_n), NA_integer_, parent_n - retained_n)) |>
  select(step, parent_n, retained_n, excluded_n, exclusion_reason)

write.csv(attrition, output_attrition, row.names = FALSE, na = "")
message("\nAttrition table used for the figure:\n")
print(attrition, n = Inf, width = Inf)
message(
  "\nValidation passed: script 12 logic gives N = ",
  format(script12_hosp_n, big.mark = ","), " for hospitalisation and N = ",
  format(script12_death_n, big.mark = ","), " for mortality.\n"
)

# -----------------------------------------------------------------------------
# grid drawing helpers
# -----------------------------------------------------------------------------
fmt_n <- function(x) paste0("N = ", format(x, big.mark = ",", trim = TRUE))
fmt_excluded <- function(x) paste0("n = ", format(x, big.mark = ",", trim = TRUE))

draw_box <- function(x, y, w, h, label, n_text, fill, border,
                     label_size = 8.2, n_size = 8.4, label_face = "bold",
                     text_col = cols$ink) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(w, "npc"), height = unit(h, "npc"), r = unit(2.1, "mm"),
    gp = gpar(fill = fill, col = border, lwd = 0.75)
  )
  grid.text(
    label, x = unit(x, "npc"), y = unit(y + h * 0.16, "npc"), just = "centre",
    gp = gpar(fontfamily = font_family, fontsize = label_size,
              fontface = label_face, col = text_col, lineheight = 0.98)
  )
  grid.text(
    n_text, x = unit(x, "npc"), y = unit(y - h * 0.23, "npc"), just = "centre",
    gp = gpar(fontfamily = font_family, fontsize = n_size,
              fontface = "bold", col = text_col)
  )
}

draw_exclusion <- function(x, y, w, h, reason, n_excluded) {
  draw_box(
    x, y, w, h, paste("Excluded", reason, sep = "\n"), fmt_excluded(n_excluded),
    cols$excluded_fill, cols$excluded_border,
    label_size = 6.6, n_size = 7.3, label_face = "plain", text_col = cols$excluded_text
  )
}

draw_arrow <- function(x0, y0, x1, y1, col = cols$line, lwd = 0.75) {
  grid.lines(
    x = unit(c(x0, x1), "npc"), y = unit(c(y0, y1), "npc"),
    arrow = arrow(type = "closed", length = unit(2.0, "mm")),
    gp = gpar(col = col, lwd = lwd, lineend = "round")
  )
}

draw_line <- function(x0, y0, x1, y1, col = cols$line, lwd = 0.75) {
  grid.lines(
    x = unit(c(x0, x1), "npc"), y = unit(c(y0, y1), "npc"),
    gp = gpar(col = col, lwd = lwd, lineend = "round")
  )
}

draw_flowchart <- function() {
  grid.newpage()
  pushViewport(viewport(gp = gpar(fontfamily = font_family)))

  # The manuscript caption can carry the formal figure number and title.
  grid.text(
    "Flowchart of cohort selection from Brazilian SINAN chikungunya surveillance data, 2015-2025",
    x = unit(0.5, "npc"), y = unit(0.972, "npc"),
    gp = gpar(fontfamily = font_family, fontsize = 10.2, fontface = "bold", col = cols$ink)
  )
  grid.lines(x = unit(c(0.055, 0.945), "npc"), y = unit(c(0.946, 0.946), "npc"),
             gp = gpar(col = "#D5DADD", lwd = 0.55))

  # Main flow: vertically aligned retained cohorts and a right-side exclusion column.
  x_main <- 0.345; w_main <- 0.425; h_main <- 0.084
  x_excl <- 0.775; w_excl <- 0.335; h_excl <- 0.066
  y_main <- c(raw = 0.862, confirmed = 0.714, year2017 = 0.566,
              valid = 0.418, core3 = 0.270)
  x_hosp <- 0.180; x_death <- 0.460; w_final <- 0.250; h_final <- 0.104; y_final <- 0.077

  draw_box(x_main, y_main[["raw"]], w_main, h_main,
           "All SINAN chikungunya records\n2015-2025", fmt_n(nrow(cohort_raw)),
           cols$retained_fill, cols$retained_border)
  draw_box(x_main, y_main[["confirmed"]], w_main, h_main,
           "Confirmed chikungunya cases", fmt_n(nrow(cohort_confirmed)),
           cols$retained_fill, cols$retained_border)
  draw_box(x_main, y_main[["year2017"]], w_main, h_main,
           "Notified/onset in 2017 or later", fmt_n(nrow(cohort_2017)),
           cols$retained_fill, cols$retained_border)
  draw_box(x_main, y_main[["valid"]], w_main, h_main,
           "Valid age (0-100 years) and sex", fmt_n(nrow(cohort_valid)),
           cols$retained_fill, cols$retained_border)
  draw_box(x_main, y_main[["core3"]], w_main, h_main,
           "DM, HTN, and CKD status known\n(core-3 complete case)", fmt_n(nrow(cohort_core3)),
           cols$retained_key_fill, cols$retained_border)

  sequential_steps <- list(
    list(from = "raw", to = "confirmed", reason = "Not confirmed chikungunya", n = attrition$excluded_n[2]),
    list(from = "confirmed", to = "year2017", reason = "Before 2017 or missing\nevent date", n = attrition$excluded_n[3]),
    list(from = "year2017", to = "valid", reason = "Invalid/missing age or sex", n = attrition$excluded_n[4]),
    list(from = "valid", to = "core3", reason = "At least one DM, HTN, or CKD\nfield unknown/missing", n = attrition$excluded_n[5])
  )

  for (step in sequential_steps) {
    y_from_bottom <- y_main[[step$from]] - h_main / 2
    y_to_top <- y_main[[step$to]] + h_main / 2
    y_branch <- mean(c(y_from_bottom, y_to_top))
    draw_arrow(x_main, y_from_bottom, x_main, y_to_top)
    draw_line(x_main, y_branch, x_main + w_main / 2 + 0.010, y_branch)
    draw_arrow(x_main + w_main / 2 + 0.010, y_branch, x_excl - w_excl / 2, y_branch)
    draw_exclusion(x_excl, y_branch, w_excl, h_excl, step$reason, step$n)
  }

  # Independent, outcome-specific derivations from the shared core-3 cohort.
  grid.text(
    "Outcome-specific cohorts constructed independently",
    x = unit(x_main, "npc"), y = unit(0.207, "npc"), just = "centre",
    gp = gpar(fontfamily = font_family, fontsize = 7.1, fontface = "italic", col = cols$note)
  )
  y_outcome_branch <- 0.173
  draw_line(x_main, y_main[["core3"]] - h_main / 2, x_main, y_outcome_branch)
  draw_line(x_hosp, y_outcome_branch, x_death, y_outcome_branch)
  draw_line(x_main, y_outcome_branch, x_hosp, y_outcome_branch)
  draw_arrow(x_hosp, y_outcome_branch, x_hosp, y_final + h_final / 2)
  draw_arrow(x_death, y_outcome_branch, x_death, y_final + h_final / 2)

  draw_box(x_hosp, y_final, w_final, h_final,
           "Hospitalisation cohort\nHospitalisation status known", fmt_n(nrow(cohort_hosp)),
           cols$retained_key_fill, cols$retained_border, label_size = 7.3, n_size = 8.1)
  draw_box(x_death, y_final, w_final, h_final,
           "Mortality cohort\nChikungunya death status known", fmt_n(nrow(cohort_death)),
           cols$retained_key_fill, cols$retained_border, label_size = 7.3, n_size = 8.1)

  # Show status exclusions for both independent outcome cohorts. The death-status
  # box remains even when zero, so the audit trail stays explicit.
  y_hosp_excl <- 0.173; y_death_excl <- 0.077
  draw_arrow(x_main + w_main / 2, y_outcome_branch, x_excl - w_excl / 2, y_hosp_excl)
  draw_exclusion(x_excl, y_hosp_excl, w_excl, h_excl,
                 "Hospitalisation status unknown", attrition$excluded_n[6])
  draw_arrow(x_death + w_final / 2, y_final, x_excl - w_excl / 2, y_death_excl)
  draw_exclusion(x_excl, y_death_excl, w_excl, h_excl,
                 "Chikungunya death status unknown", attrition$excluded_n[7])

  grid.text(
    "DM = diabetes mellitus; HTN = hypertension; CKD = chronic kidney disease.",
    x = unit(0.055, "npc"), y = unit(0.014, "npc"), just = c("left", "centre"),
    gp = gpar(fontfamily = font_family, fontsize = 6.4, col = cols$note)
  )
  popViewport()
}

# PDF and SVG are vector outputs; PNG is high-resolution for journal systems.
grDevices::cairo_pdf(output_pdf, width = figure_width_mm / 25.4,
                     height = figure_height_mm / 25.4, family = font_family)
draw_flowchart()
grDevices::dev.off()

svglite::svglite(output_svg, width = figure_width_mm / 25.4,
                 height = figure_height_mm / 25.4,
                 system_fonts = list(sans = font_family))
draw_flowchart()
grDevices::dev.off()

ragg::agg_png(output_png, width = figure_width_mm, height = figure_height_mm,
               units = "mm", res = 600, background = "white")
draw_flowchart()
grDevices::dev.off()

message(
  "\nSaved figure files:\n  ", normalizePath(output_pdf, winslash = "/"),
  "\n  ", normalizePath(output_svg, winslash = "/"),
  "\n  ", normalizePath(output_png, winslash = "/"),
  "\nSaved attrition table:\n  ", normalizePath(output_attrition, winslash = "/"), "\n"
)
