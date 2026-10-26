# ---------------------------------------------------------------------------
# 54_figure3_standalone_panels.R
#
# Standalone single-panel versions of the age-specific absolute-risk
# figure, for assembly into a main manuscript figure:
#   Panel A  hospitalisation risk (%) by age group and profile
#   Panel B  chikungunya-attributed death risk (%) by age group and profile
#
# Display rule (both panels): a profile x age cell is shown only if it has
# >=10 events AND >=10 non-events; the None profile is shown in every age
# group as the reference trajectory. No count table, title or caption is
# drawn - counts go to a Supplementary Table.
#
# Estimates are read, not refitted:
#   hospitalisation: H2 model, 03_Output/figure_revision/figure3_final/ (script 52)
#   death: validated age model, 03_Output/validation/figure5_age_validation/ (script 46)
#
# Output: 03_Output/figure_revision/figure3_final/panels/
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

out_dir <- file.path(fig_rev_dir, "figure3_final", "panels")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

min_events <- 10
## TRUE drops profile-specific points at 0-14 even where counts are adequate
## (recorded comorbidity in children is of doubtful validity; see
## HOSPITAL_AGE_VALIDATION.md section 2). FALSE follows the support rule only.
exclude_child_profiles <- FALSE

h <- readr::read_csv(file.path(fig_rev_dir, "figure3_final", "hospital_age_profile_all_model_estimates.csv"),
                     show_col_types = FALSE) |>
  dplyr::filter(model == "H2") |>
  dplyr::transmute(age_group, core_profile, n_cases, n_events = n_hosp,
                   std_risk_pct, std_risk_lower_pct, std_risk_upper_pct)
m <- readr::read_csv("03_Output/validation/figure5_age_validation/age_profile_standardised_estimates.csv",
                     show_col_types = FALSE) |>
  dplyr::transmute(age_group, core_profile, n_cases, n_events = n_deaths,
                   std_risk_pct, std_risk_lower_pct, std_risk_upper_pct)

stopifnot(sum(h$n_cases) == 776072, sum(h$n_events) == 25363,
          sum(m$n_cases) == 983947, sum(m$n_events) == 1050)

select_cells <- function(d) {
  d |>
    dplyr::mutate(
      shown = core_profile == "None" |
        (n_events >= min_events & (n_cases - n_events) >= min_events &
           !(exclude_child_profiles & age_group == "0-14")),
      age_group = factor(age_group, levels = age_levels),
      core_profile = factor(core_profile, levels = core_profile_levels)
    )
}
h <- select_cells(h)
m <- select_cells(m)

## Restrained palette: the project's fixed per-profile hues (Okabe-Ito based),
## None in neutral grey; each profile also has its own marker.
pal <- profile_colours
shp <- c("None" = 21, "DM" = 24, "HTN" = 22, "CKD" = 23, "DM+HTN" = 25,
         "DM+CKD" = 4, "HTN+CKD" = 23, "DM+HTN+CKD" = 21)
## Two profiles share a fillable shape family; distinguish by fill.
fill_solid <- c("None" = TRUE, "DM" = FALSE, "HTN" = FALSE, "CKD" = FALSE, "DM+HTN" = FALSE,
                "DM+CKD" = FALSE, "HTN+CKD" = TRUE, "DM+HTN+CKD" = TRUE)

## `style` controls mark weights: "standalone" for the single panels, "light"
## for the combined figure, where seven profiles share each age group and
## lighter lines/intervals keep the panel uncluttered.
mark_styles <- list(
  standalone = list(line_w = 0.45, line_alpha = 0.5, ci_w = 0.5, ci_alpha = 1, pt = 1.9, stroke = 0.65),
  light      = list(line_w = 0.35, line_alpha = 0.4, ci_w = 0.4, ci_alpha = 0.75, pt = 1.55, stroke = 0.55)
)

risk_panel <- function(d, tag, y_lab, breaks, limits, title = NULL, style = "standalone") {
  st <- mark_styles[[style]]
  pd <- dplyr::filter(d, shown) |> droplevels()
  ## Lines join only consecutive shown age groups (no bridging of gaps).
  pd <- pd |>
    dplyr::group_by(core_profile) |>
    dplyr::arrange(age_group, .by_group = TRUE) |>
    dplyr::mutate(run = cumsum(c(1, diff(as.integer(age_group)) != 1))) |>
    dplyr::ungroup() |>
    dplyr::mutate(fill_col = ifelse(fill_solid[as.character(core_profile)],
                                    pal[as.character(core_profile)], "white"))
  ## Fixed per-profile horizontal offsets (same in every age group and in
  ## both panels), so lines always meet their own markers; position_dodge
  ## would re-space points by whichever profiles are present at each age.
  offsets <- setNames(seq(-0.3, 0.3, length.out = 8), core_profile_levels)
  pd <- dplyr::mutate(pd, x = as.integer(age_group) + offsets[as.character(core_profile)])
  profs <- levels(pd$core_profile)

  ggplot2::ggplot(pd, ggplot2::aes(x = x, y = std_risk_pct, colour = core_profile,
                                   shape = core_profile, group = core_profile)) +
    ggplot2::geom_line(ggplot2::aes(group = interaction(core_profile, run)),
                       linewidth = st$line_w, alpha = st$line_alpha) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = std_risk_lower_pct, ymax = std_risk_upper_pct),
                            linewidth = st$ci_w, alpha = st$ci_alpha) +
    ggplot2::geom_point(ggplot2::aes(fill = core_profile), size = st$pt,
                        stroke = st$stroke) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_levels), labels = unname(age_labels[age_levels]),
                                limits = c(0.55, length(age_levels) + 0.45), expand = c(0, 0)) +
    ggplot2::scale_y_log10(breaks = breaks, limits = limits,
                           labels = function(x) paste0(format(x, drop0trailing = TRUE,
                                                              scientific = FALSE, trim = TRUE), "%")) +
    ggplot2::scale_colour_manual(values = pal[profs], labels = profile_display[profs], name = NULL) +
    ggplot2::scale_shape_manual(values = shp[profs], labels = profile_display[profs], name = NULL) +
    ggplot2::scale_fill_manual(values = ifelse(fill_solid[profs], pal[profs], "white"),
                               labels = profile_display[profs], name = NULL) +
    ggplot2::labs(tag = tag, title = title, x = "Age group (years)", y = y_lab) +
    theme_rev(base_size = 8.5) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 12, colour = ink),
      plot.tag.position = c(0.005, 0.995),
      legend.position = "top", legend.justification = "right",
      legend.box.margin = ggplot2::margin(0, 0, -4, 0),
      legend.text = ggplot2::element_text(size = 7.4),
      legend.key.width = ggplot2::unit(4.5, "mm"),
      axis.text.x = ggplot2::element_text(size = 8.5, colour = ink, face = "bold",
                                          margin = ggplot2::margin(t = 3)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, colour = "#E6E6E6"),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 8, 6, 8)
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                                   override.aes = list(linewidth = 0.5)))
}

pA <- risk_panel(h, "A", "Hospitalisation risk (%, log scale)",
                 breaks = c(1, 2, 3, 5, 10, 20, 30), limits = c(1.8, 36))
pB <- risk_panel(m, "B", "Death risk (%, log scale)",
                 breaks = c(0.03, 0.1, 0.3, 1, 3, 10), limits = c(0.02, 14))

save_figure(pA, "Figure3_panelA_hospitalisation", width = 130, height = 100, dir = out_dir)
save_figure(pB, "Figure3_panelB_death", width = 130, height = 100, dir = out_dir)
## Side-by-side preview of the two panels as they would be assembled.
## Both panels show the same profiles with the same encodings, so the
## assembled preview collects them into one shared legend.
preview <- (pA | pB) + patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "top", legend.justification = "center")
save_figure(preview, "Figure3_panelsAB_preview", width = 183, height = 92, dir = out_dir,
            formats = c("png", "pdf"))

## ------------------------------------------------------------
## Combined two-panel figure: panel titles, panel letters, one shared
## legend centred at the bottom, lighter marks. No overall title, no
## counts, no caption. Same cells and encodings as the panels above.
## ------------------------------------------------------------
cA <- risk_panel(h, "A", "Hospitalisation risk (%, log scale)",
                 breaks = c(2, 3, 5, 10, 20, 30), limits = c(1.8, 36),
                 title = "Hospitalisation risk by age group and multimorbidity profile", style = "light")
cB <- risk_panel(m, "B", "Death risk (%, log scale)",
                 breaks = c(0.03, 0.1, 0.3, 1, 3, 10), limits = c(0.02, 14),
                 title = "Death risk by age group and multimorbidity profile", style = "light")

combined <- (cA | cB) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 8.1, colour = ink,
                                       margin = ggplot2::margin(b = 6)),
    plot.title.position = "plot",
    plot.tag.position = "topleft",
    plot.tag = ggplot2::element_text(face = "bold", size = 12, colour = ink,
                                     margin = ggplot2::margin(r = 2)),
    legend.position = "bottom", legend.justification = "center",
    legend.box.margin = ggplot2::margin(t = 2),
    legend.text = ggplot2::element_text(size = 7.6, margin = ggplot2::margin(r = 6)),
    plot.margin = ggplot2::margin(6, 8, 4, 6)
  ) &
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 0.45)))

save_figure(combined, "Figure3_combined_AB_final", width = 183, height = 95, dir = out_dir)

## Exactly what is drawn, for the Supplementary Table and audit.
shown <- dplyr::bind_rows(
  dplyr::mutate(h, panel = "A", outcome = "Hospitalisation", model = "H2"),
  dplyr::mutate(m, panel = "B", outcome = "Chikungunya-attributed death", model = "fs (validated)")
) |>
  dplyr::mutate(n_nonevents = n_cases - n_events) |>
  dplyr::relocate(panel, outcome, model)
readr::write_csv(shown, file.path(out_dir, "Figure3_panelsAB_cells_and_display_status.csv"))

message("[54] panels written to ", out_dir, "; cells shown: A = ", sum(h$shown), ", B = ", sum(m$shown))
