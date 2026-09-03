# ---------------------------------------------------------------------------
# 09_copula_result_graphs.R
#
# Visualises the copula simulation results from 08_copula_simulation.R:
# comorbidity-count distribution for a handful of illustrative countries,
# a country x age heatmap of P(2+ comorbidities), the population-weighted
# global average, and the mutually-exclusive condition-category breakdown.
#
# Needs (from earlier scripts, already in the session):
#   bg_count_dist_wide, bg_count_dist_pop, bg_exclusive_dist   from 08_copula_simulation.R
#
# Fixes applied while restructuring (2026-09): this file used to reference
# a non-existent prev_comorb_3plus column (bg_count_dist_wide only ever had
# prev_comorb_0/1/2plus - the "3+" category was dropped when the pipeline
# moved from a 4-group to a 3-group comorbidity-count scheme) and its
# ggsave() call saved a different, undefined object (p_heatmap_3plus)
# instead of the one actually built (p_heatmap_2plus). Both are fixed below.
# ---------------------------------------------------------------------------

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

## Shared theme for this script and 10_background_burden_graphs.R.
## Deliberately named differently from 03_relative_risk.R's
## theme_lancet_clean() (a different, more elaborate function) so the two
## never silently shadow each other in the shared session.
theme_lancet_gbd <- function(base_size = 9) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.text = ggplot2::element_text(colour = "black", size = base_size - 1),
      axis.title = ggplot2::element_text(colour = "black", size = base_size),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = "black",
        hjust = 0,
        size = base_size
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      plot.title = ggplot2::element_text(
        face = "bold",
        colour = "black",
        size = base_size + 1,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "black",
        size = base_size - 1,
        hjust = 0
      ),
      panel.spacing.y = grid::unit(0.8, "lines"),
      panel.spacing.x = grid::unit(0.8, "lines")
    )
}

## Shared 3-group palette (matches comorb_count_group = "0"/"1"/"2+" used
## throughout the pipeline). Reused by 10_background_burden_graphs.R.
count_palette <- c(
  "0"  = "#BDBDBD",
  "1"  = "#FDD081",
  "2+" = "#B2182B"
)

## ============================================================
## Comorbidity-count distribution, selected countries
## ============================================================

plot_df <- bg_count_dist_wide |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_mid = dplyr::case_when(
      is.infinite(age_end) ~ age_start + 2.5,
      TRUE ~ (age_start + age_end) / 2
    )
  ) |>
  tidyr::pivot_longer(
    cols = c(
      prev_comorb_0,
      prev_comorb_1,
      prev_comorb_2plus
    ),
    names_to = "comorb_count_group",
    values_to = "prevalence"
  ) |>
  dplyr::mutate(
    comorb_count_group = dplyr::case_when(
      comorb_count_group == "prev_comorb_0" ~ "0",
      comorb_count_group == "prev_comorb_1" ~ "1",
      comorb_count_group == "prev_comorb_2plus" ~ "2+"
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2+")
    )
  )

selected_iso3 <- c("BRA", "IND", "KOR", "MKD", "BOL")

p_stacked_area_fixed2 <- plot_df |>
  dplyr::filter(iso3 %in% selected_iso3) |>
  dplyr::arrange(
    country_name,
    comorb_count_group,
    age_mid
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      y = prevalence,
      fill = comorb_count_group,
      group = comorb_count_group
    )
  ) +
  ggplot2::geom_area(
    stat = "identity",
    position = "stack",
    alpha = 1,
    colour = NA,
    linewidth = 0
  ) +
  ggplot2::facet_wrap(~ country_name, ncol = 2) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    name = "Number of comorbidities"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = "Age",
    y = "Share of population",
    fill = "Number of comorbidities",
    title = "Age-specific distribution of comorbidity count by country",
    subtitle = "Estimated from country-age-specific marginal prevalence and Brazil-derived age-specific comorbidity correlation structure"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    axis.title = ggplot2::element_text(color = "black"),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey30")
  )

p_stacked_area_fixed2

p_heatmap_2plus <- bg_count_dist_wide |>
  dplyr::filter(iso3 %in% selected_iso3) |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_group = factor(
      age_group,
      levels = unique(age_group[order(age_start)])
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_group,
      y = forcats::fct_reorder(country_name, prev_comorb_2plus, .fun = max),
      fill = prev_comorb_2plus
    )
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(
    labels = scales::percent_format(accuracy = 1),
    name = "P(2+)"
  ) +
  ggplot2::labs(
    x = "Age group",
    y = NULL,
    title = "Estimated prevalence of 2+ comorbidities by country and age"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = ggplot2::element_text(color = "black"),
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold")
  )

p_heatmap_2plus

ggsave(
  filename = file.path(fig_dir, "fig_heatmap_2plus.jpg"),
  plot = p_heatmap_2plus,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)

## ============================================================
## Global (population-weighted) average
## ============================================================

global_age_summary <- plot_df |>
  dplyr::group_by(age_start, age_end, age_mid, comorb_count_group) |>
  dplyr::summarise(
    mean_prevalence = mean(prevalence, na.rm = TRUE),
    lower = stats::quantile(prevalence, 0.25, na.rm = TRUE),
    upper = stats::quantile(prevalence, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_global_age <- global_age_summary |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      y = mean_prevalence,
      colour = comorb_count_group,
      fill = comorb_count_group
    )
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.15,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_colour_manual(values = count_palette) +
  ggplot2::scale_fill_manual(values = count_palette) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  ggplot2::labs(
    x = "Age",
    y = "Mean share across countries",
    colour = "Number of comorbidities",
    fill = "Number of comorbidities",
    title = "Average age-specific comorbidity count distribution across countries"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")
  )

p_global_age

global_count_plot <- bg_count_dist_pop |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_mid = dplyr::case_when(
      is.infinite(age_end) ~ 97.5,
      TRUE ~ (age_start + age_end) / 2
    )
  ) |>
  tidyr::pivot_longer(
    cols = c(
      pop_comorb_0,
      pop_comorb_1,
      pop_comorb_2plus
    ),
    names_to = "comorb_count_group",
    values_to = "population_count"
  ) |>
  dplyr::mutate(
    comorb_count_group = dplyr::case_when(
      comorb_count_group == "pop_comorb_0" ~ "0",
      comorb_count_group == "pop_comorb_1" ~ "1",
      comorb_count_group == "pop_comorb_2plus" ~ "2+"
    )
  ) |>
  dplyr::group_by(
    age_start,
    age_end,
    age_mid,
    comorb_count_group
  ) |>
  dplyr::summarise(
    population_count = sum(population_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(
    age_start,
    age_end,
    age_mid
  ) |>
  dplyr::mutate(
    total_population = sum(population_count, na.rm = TRUE),
    population_percent = population_count / total_population
  ) |>
  dplyr::ungroup()

global_pct_ribbon <- global_count_plot |>
  dplyr::mutate(
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("2+", "1", "0")
    )
  ) |>
  dplyr::arrange(
    age_mid,
    comorb_count_group
  ) |>
  dplyr::group_by(
    age_start,
    age_end,
    age_mid
  ) |>
  dplyr::mutate(
    ymax = cumsum(population_percent),
    ymin = ymax - population_percent,
    ymin = pmax(ymin, 0),
    ymax = pmin(ymax, 1)
  ) |>
  dplyr::ungroup()

p_global_stack <- global_pct_ribbon |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      ymin = ymin,
      ymax = ymax,
      fill = comorb_count_group,
      group = comorb_count_group
    )
  ) +
  ggplot2::geom_ribbon(
    alpha = 1,
    colour = NA
  ) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    breaks = c("0", "1", "2+"),
    name = "Comorbidity count"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1),
    clip = "on"
  ) +
  ggplot2::labs(
    x = "Age",
    y = "Global population, %",
    title = "Global age-specific distribution of comorbidity count",
    subtitle = "Population-weighted estimates from country-age-specific marginal prevalence and Brazil-derived comorbidity correlation structure"
  ) +
  theme_lancet_gbd(base_size = 9) +
  ggplot2::theme(
    legend.position = "top"
  )

p_global_stack

ggsave(
  filename = file.path(fig_dir, "fig_global_stack.jpg"),
  plot = p_global_stack,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)

## ============================================================
## Mutually exclusive condition-category breakdown
## ============================================================

selected_iso3_exclusive <- c("BRA", "IND", "KOR", "GBR", "ZMB")

exclusive_levels <- c(
  "No selected condition",
  "Hypertension only",
  "Diabetes only",
  "Renal disease only",
  "Liver disease only",
  "Haematologic disease only",
  "Peptic ulcer disease only",
  "Autoimmune disease only",
  "Multimorbidity, ≥2 selected conditions"
)

exclusive_plot_df <- bg_exclusive_dist |>
  dplyr::filter(iso3 %in% selected_iso3_exclusive) |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_mid = dplyr::case_when(
      is.infinite(age_end) ~ age_start + 2.5,
      TRUE ~ (age_start + age_end) / 2
    ),
    exclusive_group = factor(
      exclusive_group,
      levels = exclusive_levels
    )
  )

exclusive_palette <- c(
  "No selected condition" = "#BDBDBD",
  "Hypertension only" = "#FDD17A",
  "Diabetes only" = "#F28E2B",
  "Renal disease only" = "#E15759",
  "Liver disease only" = "#76B7B2",
  "Haematologic disease only" = "#4E79A7",
  "Peptic ulcer disease only" = "#59A14F",
  "Autoimmune disease only" = "#9C755F",
  "Multimorbidity, ≥2 selected conditions" = "#A50F15"
)

p_exclusive_stacked_area <- ggplot2::ggplot(
  exclusive_plot_df,
  ggplot2::aes(
    x = age_mid,
    y = prevalence,
    fill = exclusive_group,
    group = exclusive_group
  )
) +
  ggplot2::geom_area(
    stat = "identity",
    position = "stack",
    alpha = 1,
    colour = NA,
    linewidth = 0
  ) +
  ggplot2::facet_wrap(
    ~ country_name,
    ncol = 2
  ) +
  ggplot2::scale_fill_manual(
    values = exclusive_palette,
    breaks = exclusive_levels,
    name = "Category"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = "Age, years",
    y = "Share of population",
    title = "Age-specific distribution of selected condition categories"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    axis.title = ggplot2::element_text(color = "black"),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    legend.text = ggplot2::element_text(size = 8),
    legend.key.size = grid::unit(0.45, "cm"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey30")
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  )

p_exclusive_stacked_area

ggsave(
  filename = file.path(fig_dir, "fig_global_stack_cat.jpg"),
  plot = p_exclusive_stacked_area,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)
