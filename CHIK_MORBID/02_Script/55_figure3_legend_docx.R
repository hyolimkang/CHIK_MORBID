# ---------------------------------------------------------------------------
# 55_figure3_legend_docx.R
#
# Word document containing only the title and legend for the two-panel
# Figure 3 drawn by 54_figure3_standalone_panels.R (A: hospitalisation,
# B: chikungunya-attributed death). Cohort sizes and the display rule are
# taken from the same outputs the panels use, so the legend cannot drift
# from the figure.
#
# Output: 03_Output/figure_revision/figure3_final/panels/Figure3_title_and_legend.docx
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(officer)
  library(readr)
  library(dplyr)
})

panel_dir <- "03_Output/figure_revision/figure3_final/panels"
cells <- readr::read_csv(file.path(panel_dir, "Figure3_panelsAB_cells_and_display_status.csv"),
                         show_col_types = FALSE)

n_fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
tot <- cells |>
  dplyr::group_by(panel) |>
  dplyr::summarise(n = sum(n_cases), events = sum(n_events), .groups = "drop")
stopifnot(tot$n[tot$panel == "A"] == 776072, tot$events[tot$panel == "A"] == 25363,
          tot$n[tot$panel == "B"] == 983947, tot$events[tot$panel == "B"] == 1050)

## Profiles with no displayed cell in either panel (named in the legend).
never_shown <- cells |>
  dplyr::group_by(core_profile) |>
  dplyr::summarise(any_shown = any(shown), .groups = "drop") |>
  dplyr::filter(!any_shown) |>
  dplyr::pull(core_profile)
stopifnot(identical(never_shown, "DM+CKD"))

title <- paste0(
  "Figure 3. Age-specific standardised risks of hospitalisation and chikungunya-attributed ",
  "death by multimorbidity profile among confirmed chikungunya cases, Brazil, 2017–2025"
)

legend <- paste0(
  "(A) Standardised risk of hospitalisation by age group among ",
  n_fmt(tot$n[tot$panel == "A"]), " cases with recorded hospitalisation status (",
  n_fmt(tot$events[tot$panel == "A"]), " hospitalisations). ",
  "(B) Standardised risk of chikungunya-attributed death by age group among ",
  n_fmt(tot$n[tot$panel == "B"]), " cases with recorded outcome (",
  n_fmt(tot$events[tot$panel == "B"]), " deaths). ",
  "Profiles are the mutually exclusive combinations of diabetes (DM), hypertension (HTN) and ",
  "chronic kidney disease (CKD); None denotes cases with none of these conditions and is shown ",
  "in every age group as the reference trajectory. ",
  "Risks were estimated from penalised logistic generalised additive models including ",
  "profile-specific age effects, sex, notification year and state of residence, and were ",
  "standardised by g-computation: within each age group, every profile was assigned the observed ",
  "joint distribution of exact age, sex, notification year and state of residence of all cases ",
  "in that age group in the respective outcome cohort. ",
  "Only age × profile cells with at least 10 outcome events and at least 10 non-events are ",
  "displayed, so that the figure shows empirically supported estimates only; DM+CKD did not meet ",
  "this threshold in any age group and is not shown. Lines connect estimates in adjacent displayed ",
  "age groups only. Case and event counts for all age × profile cells, together with estimates ",
  "for sparse cells not displayed here, are provided in Supplementary Table S[X]. ",
  "Error bars denote 95% confidence intervals from 2,000 parametric draws of the model coefficients. ",
  "Vertical axes are logarithmic and differ in scale between panels."
)

body_text <- officer::fp_text(font.family = "Arial", font.size = 11)
title_text <- officer::fp_text(font.family = "Arial", font.size = 11, bold = TRUE)
para <- officer::fp_par(text.align = "justify", line_spacing = 1.5, padding.bottom = 10)

doc <- officer::read_docx() |>
  officer::body_add_fpar(officer::fpar(officer::ftext(title, title_text), fp_p = para)) |>
  officer::body_add_fpar(officer::fpar(officer::ftext(legend, body_text), fp_p = para))

## read_docx()'s template starts with one empty paragraph; drop it so the
## document contains only the title and the legend.
doc <- officer::cursor_begin(doc)
if (nchar(officer::docx_summary(doc)$text[1]) == 0) doc <- officer::body_remove(doc)

out_file <- file.path(panel_dir, "Figure3_title_and_legend.docx")
print(doc, target = out_file)

chk <- officer::docx_summary(officer::read_docx(out_file))
print(chk[, c("doc_index", "content_type", "text")])
message("[55] written ", out_file, " (", sum(nchar(chk$text) > 0), " paragraphs; legend ",
        length(strsplit(legend, "\\s+")[[1]]), " words)")
