# ---------------------------------------------------------------------------
# 31_figures_nature_style_background_prevalence.R
#
# Publication-style (Nature Medicine-like) re-render of the two "final"
# figures from the background-prevalence prototype (29_/30_):
#   - fig_profile_prevalence_brazil_two_extremes_PROTOTYPE  (independence vs
#     maximum-clustering bounds, 29_ Parts C/D)
#   - fig_marginal_prevalence_brazil_PROTOTYPE               (DM/HTN/CKD
#     marginal prevalence by age, 30_)
# The other 3 PROTOTYPE figures from this exploration (single-panel
# independence, pooled-r_k-is-broken comparison, age-band-r_k 3-panel) are
# diagnostic/working figures, not manuscript-track - left as-is.
#
# Per the user's request: the images themselves carry NO title/subtitle text
# (journal-style - only axis labels, legend, and panel strip labels remain
# in-image); the title + full caption for each figure instead live in a
# separate Word document, so a reader matches "Figure N" + filename there to
# the image file. Word styling mirrors 17_table_core3_standardised_risks.R's
# established house style (Arial, #263238 text, #EEF1F3 header fill,
# #7E8A90/#C9D1D5 rules).
#
# Output:
#   03_Output/figures/fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png/.pdf
#   03_Output/figures/fig_marginal_prevalence_brazil_PROTOTYPE.png/.pdf
#   03_Output/tables/figure_legends_background_prevalence_PROTOTYPE.docx
#
# Needs: 01_Data/brazil_profile_prevalence_two_extremes.RData (from 29_)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(officer); library(flextable)
})
options(scipen = 999)

if (!file.exists("01_Data/brazil_profile_prevalence_two_extremes.RData")) {
  stop("Run 29_background_prevalence_core3_brazil.R first.")
}
load("01_Data/brazil_profile_prevalence_two_extremes.RData")

fig_dir <- "03_Output/figures"
table_dir <- "03_Output/tables"
font_family <- "Arial"

core_profile_levels <- c("None","DM","HTN","CKD","DM+HTN","DM+CKD","HTN+CKD","DM+HTN+CKD")

keep_age <- c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
              "75-79","80-84","85-89","90-94","95+")
age_lookup <- tibble::tibble(age_clean = keep_age) |>
  dplyr::mutate(
    age_start = dplyr::case_when(
      stringr::str_detect(age_clean, "^<")  ~ 0,
      stringr::str_detect(age_clean, "\\+") ~ as.numeric(stringr::str_remove(age_clean, "\\+")),
      TRUE ~ as.numeric(stringr::str_extract(age_clean, "^[0-9]+"))
    ),
    age_end = dplyr::case_when(
      stringr::str_detect(age_clean, "^<")  ~ as.numeric(stringr::str_remove(age_clean, "^<")) - 1,
      stringr::str_detect(age_clean, "\\+") ~ Inf,
      TRUE ~ as.numeric(stringr::str_extract(age_clean, "[0-9]+$"))
    )
  )

## ---- shared Nature Medicine-style theme: no title/subtitle, thin axes,
## minimal chrome, small serif-free type -------------------------------------

theme_nat_med <- function(base_size = 8.5) {
  ggplot2::theme_classic(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", size = base_size, colour = "#1A1A1A",
                                          hjust = 0, margin = ggplot2::margin(4, 0, 4, 6)),
      axis.title = ggplot2::element_text(size = base_size, colour = "#1A1A1A"),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "#333333"),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, colour = "#333333"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.5),
      legend.key.size = grid::unit(3.2, "mm"),
      legend.margin = ggplot2::margin(0, 0, 2, 0),
      plot.margin = ggplot2::margin(4, 8, 4, 4),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
}

profile_colours <- c(
  "None" = "#BFBFBF", "DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868",
  "DM+HTN" = "#C44E52", "DM+CKD" = "#8172B2", "HTN+CKD" = "#937860",
  "DM+HTN+CKD" = "#CCB974"
)

## ============================================================
## Figure 1: independence vs maximum-clustering bounds (2-panel)
## ============================================================

indep_long <- profile_prev_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "prevalence") |>
  dplyr::mutate(method = "A    Independence (minimum clustering)")

max_long <- max_clustering_brazil |>
  dplyr::select(age_clean, dplyr::all_of(core_profile_levels)) |>
  tidyr::pivot_longer(dplyr::all_of(core_profile_levels), names_to = "profile", values_to = "prevalence") |>
  dplyr::mutate(method = "B    Maximum clustering (comonotonic)")

plot_both <- dplyr::bind_rows(indep_long, max_long) |>
  dplyr::left_join(age_lookup, by = "age_clean") |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    profile = factor(profile, levels = rev(core_profile_levels)),
    method = factor(method, levels = c("A    Independence (minimum clustering)", "B    Maximum clustering (comonotonic)"))
  )

fig1_two_extremes <- ggplot2::ggplot(plot_both, ggplot2::aes(x = age_mid, y = prevalence, fill = profile)) +
  ggplot2::geom_area(position = "stack", colour = "white", linewidth = 0.12) +
  ggplot2::facet_wrap(~method, ncol = 2) +
  ggplot2::scale_fill_manual(values = profile_colours, breaks = core_profile_levels) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  ggplot2::labs(x = "Age (years)", y = "Share of population", fill = NULL) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
  theme_nat_med()

ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png"),
                 fig1_two_extremes, width = 183, height = 100, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.pdf"),
                 fig1_two_extremes, width = 183, height = 100, units = "mm", device = grDevices::cairo_pdf)

message("[31] Figure 1 (two extremes) re-rendered: PNG + PDF, no in-image title.")

## ============================================================
## Figure 2: DM/HTN/CKD marginal prevalence by age (single panel)
## ============================================================

marginal_long <- bg_prev_brazil3 |>
  dplyr::select(age_clean, age_start, age_end, DM, HTN, CKD) |>
  tidyr::pivot_longer(c(DM, HTN, CKD), names_to = "condition", values_to = "prevalence") |>
  dplyr::mutate(
    age_mid = ifelse(is.infinite(age_end), age_start + 2.5, (age_start + age_end) / 2),
    condition = factor(condition, levels = c("DM", "HTN", "CKD"))
  )

condition_colours <- c("DM" = "#4C72B0", "HTN" = "#DD8452", "CKD" = "#55A868")

fig2_marginal <- ggplot2::ggplot(marginal_long, ggplot2::aes(x = age_mid, y = prevalence, colour = condition)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.1) +
  ggplot2::scale_colour_manual(values = condition_colours) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(x = "Age (years)", y = "Prevalence", colour = NULL) +
  theme_nat_med()

ggplot2::ggsave(file.path(fig_dir, "fig_marginal_prevalence_brazil_PROTOTYPE.png"),
                 fig2_marginal, width = 120, height = 95, units = "mm", dpi = 600, bg = "white")
ggplot2::ggsave(file.path(fig_dir, "fig_marginal_prevalence_brazil_PROTOTYPE.pdf"),
                 fig2_marginal, width = 120, height = 95, units = "mm", device = grDevices::cairo_pdf)

message("[31] Figure 2 (marginal prevalence) re-rendered: PNG + PDF, no in-image title.")

## ============================================================
## Figure legends (Word doc) - title + full caption per figure, linked by
## filename, mirroring 17_table_core3_standardised_risks.R's house style.
## ============================================================

legends <- tibble::tibble(
  figure = c("Figure 1", "Figure 2"),
  filename = c(
    "fig_profile_prevalence_brazil_two_extremes_PROTOTYPE.png / .pdf",
    "fig_marginal_prevalence_brazil_PROTOTYPE.png / .pdf"
  ),
  title = c(
    "Age-specific distribution of DM/HTN/CKD joint profiles in Brazil under two theoretical bounds on condition clustering.",
    "Background prevalence of diabetes, hypertension, and chronic kidney disease by age in Brazil."
  ),
  legend = c(
    paste(
      "Each of the 8 mutually exclusive diabetes (DM) / hypertension (HTN) / chronic kidney disease (CKD) profiles,",
      "as a share of the Brazilian population by age, under the two extremes consistent with the three conditions'",
      "marginal prevalences. (A) Independence (minimum clustering): profile prevalence computed directly from the",
      "product of the three marginal prevalences, i.e. assuming no correlation between conditions. (B) Maximum",
      "clustering (comonotonic bound): the same three marginal prevalences arranged so the rarer condition is always",
      "fully nested within the more common ones - the mathematical upper bound on pairwise overlap - which permits",
      "only four non-zero profiles (None; the largest-prevalence condition alone; the largest and middle conditions",
      "together; all three together). The true population distribution lies between panels A and B. Marginal",
      "prevalences: diabetes and chronic kidney disease from the Global Burden of Disease Study 2023 (both sexes);",
      "hypertension from NCD-RisC (2019, ages 30-79), sexes combined using the age-specific male proportion observed",
      "in the Brazil SINAN chikungunya cohort, and extrapolated below 30 and above 79 years with a logit-quadratic",
      "fit to the ten NCD-RisC-observed age bands."
    ),
    paste(
      "Prevalence of diabetes mellitus (DM), hypertension (HTN), and chronic kidney disease (CKD) by age in Brazil,",
      "each condition estimated independently of the others (no assumption about co-occurrence). Diabetes and",
      "chronic kidney disease: Global Burden of Disease Study 2023 (both sexes). Hypertension: NCD-RisC (2019),",
      "reported for ages 30-79 only; sexes combined using the age-specific male proportion observed in the Brazil",
      "SINAN chikungunya cohort, with values below 30 and above 79 years obtained by logit-quadratic extrapolation",
      "of the ten NCD-RisC-observed age bands."
    )
  )
)

ft <- flextable::flextable(legends)
ft <- flextable::set_header_labels(ft, figure = "Figure", filename = "File", title = "Title", legend = "Legend")
ft <- flextable::border_remove(ft)
thin_rule <- officer::fp_border(color = "#C9D1D5", width = 0.65)
header_rule <- officer::fp_border(color = "#7E8A90", width = 1.0)
ft <- flextable::hline_top(ft, part = "header", border = header_rule)
ft <- flextable::hline_bottom(ft, part = "header", border = header_rule)
ft <- flextable::hline(ft, i = seq_len(nrow(legends)), part = "body", border = thin_rule)
ft <- flextable::bg(ft, part = "header", bg = "#EEF1F3")
ft <- flextable::color(ft, part = "all", color = "#263238")
ft <- flextable::font(ft, part = "all", fontname = font_family)
ft <- flextable::fontsize(ft, part = "header", size = 9)
ft <- flextable::fontsize(ft, part = "body", size = 8.5)
ft <- flextable::bold(ft, part = "header", bold = TRUE)
ft <- flextable::bold(ft, j = c("figure", "title"), part = "body", bold = TRUE)
ft <- flextable::align(ft, j = "figure", align = "center", part = "all")
ft <- flextable::valign(ft, part = "all", valign = "top")
ft <- flextable::padding(ft, part = "all", padding.top = 5, padding.bottom = 5, padding.left = 6, padding.right = 6)
ft <- flextable::width(ft, j = "figure", width = 0.55)
ft <- flextable::width(ft, j = "filename", width = 1.55)
ft <- flextable::width(ft, j = "title", width = 1.9)
ft <- flextable::width(ft, j = "legend", width = 3.3)
ft <- flextable::set_table_properties(ft, layout = "fixed", opts_word = list(split = TRUE))

doc <- officer::read_docx()
doc <- officer::body_set_default_section(
  doc, value = officer::prop_section(
    page_margins = officer::page_mar(top = 0.65, bottom = 0.65, left = 0.55, right = 0.55),
    page_size = officer::page_size(orient = "landscape")
  )
)
doc <- officer::body_add_fpar(
  doc, value = officer::fpar(
    officer::ftext("Figure legends - background prevalence prototype (Brazil)",
                    prop = officer::fp_text(font.family = font_family, bold = TRUE, font.size = 12))
  )
)
doc <- officer::body_add_par(doc, value = "", style = "Normal")
doc <- flextable::body_add_flextable(doc, value = ft, align = "left")

out_docx <- file.path(table_dir, "figure_legends_background_prevalence_PROTOTYPE.docx")
print(doc, target = out_docx)

message("[31] Saved figure legends: ", out_docx)
message("[31] DONE.")
