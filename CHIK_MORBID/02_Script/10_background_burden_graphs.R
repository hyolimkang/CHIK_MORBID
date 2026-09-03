# ---------------------------------------------------------------------------
# 10_background_burden_graphs.R
#
# Last step of the main pipeline. Visualises the between-country prevalence
# distribution by condition and age (bg_prev_country_wide, from
# 07_gbd_age_alignment.R), and population-by-comorbidity-count by world
# region (bg_count_dist_wide + population, from 08_copula_simulation.R /
# 04_background_prevalence.R).
#
# Needs (from earlier scripts, already in the session):
#   bg_prev_country_wide          from 07_gbd_age_alignment.R
#   bg_count_dist_wide            from 08_copula_simulation.R
#   pop_country_age               from 04_background_prevalence.R
#   theme_lancet_gbd(), count_palette   from 09_copula_result_graphs.R
# ---------------------------------------------------------------------------

## ============================================================
## Reshape global comorbidity prevalence into long format
## ============================================================

condition_labels <- c(
  bg_prev_autoimmune =
    "Autoimmune disease",
  
  bg_prev_diabetes =
    "Diabetes",
  
  bg_prev_hematologic =
    "Haematological conditions",
  
  bg_prev_hepatopathy =
    "Liver disease",
  
  bg_prev_hypertension =
    "Hypertension",
  
  bg_prev_peptic_ulcer =
    "Peptic ulcer disease",
  
  bg_prev_renal_disease =
    "Renal disease"
)

bg_prev_long <- bg_prev_country_wide |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("bg_prev_"),
    names_to = "condition",
    values_to = "prevalence"
  ) |>
  dplyr::mutate(
    condition = dplyr::recode(
      condition,
      !!!condition_labels
    ),
    
    age_mid =
      (
        age_start +
          age_end
      ) / 2,
    
    age_group = paste0(
      age_start,
      "–",
      age_end
    )
  ) |>
  dplyr::filter(
    !is.na(prevalence),
    is.finite(prevalence),
    prevalence >= 0,
    prevalence <= 1
  )

## ============================================================
## Summarise between-country prevalence distributions
## ============================================================

bg_prev_age_summary <- bg_prev_long |>
  dplyr::filter(
    is.finite(age_start),
    is.finite(age_end),
    is.finite(age_mid),
    is.finite(prevalence)
  ) |>
  dplyr::group_by(
    condition,
    age_start,
    age_end,
    age_mid,
    age_group
  ) |>
  dplyr::summarise(
    median = stats::median(
      prevalence,
      na.rm = TRUE
    ),
    
    q25 = stats::quantile(
      prevalence,
      probs = 0.25,
      na.rm = TRUE
    ),
    
    q75 = stats::quantile(
      prevalence,
      probs = 0.75,
      na.rm = TRUE
    ),
    
    p05 = stats::quantile(
      prevalence,
      probs = 0.05,
      na.rm = TRUE
    ),
    
    p95 = stats::quantile(
      prevalence,
      probs = 0.95,
      na.rm = TRUE
    ),
    
    n_countries =
      dplyr::n_distinct(iso3),
    
    .groups = "drop"
  )
## ============================================================
## Plot age-specific prevalence distributions across countries
## ============================================================
## ============================================================
## Lancet colour palette for comorbidity conditions
## ============================================================

condition_colours <- c(
  "Autoimmune disease" =
    "#00468B",
  
  "Diabetes" =
    "#ED0000",
  
  "Haematological conditions" =
    "#42B540",
  
  "Liver disease" =
    "#0099B4",
  
  "Hypertension" =
    "#925E9F",
  
  "Peptic ulcer disease" =
    "#FDAF91",
  
  "Renal disease" =
    "#AD002A"
)

## ============================================================
## Plot age-specific prevalence across countries
## ============================================================

p_comorbidity_age <- ggplot2::ggplot(
  bg_prev_age_summary,
  ggplot2::aes(
    x = age_mid,
    y = median * 100,
    colour = condition,
    fill = condition
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = q25 * 100,
      ymax = q75 * 100
    ),
    alpha = 0.18,
    colour = NA
  ) +
  ggplot2::geom_line(
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    size = 1.7
  ) +
  ggplot2::facet_wrap(
    ~ condition,
    scales = "free_y",
    ncol = 3
  ) +
  ggplot2::scale_colour_manual(
    values = condition_colours,
    drop = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = condition_colours,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(
      0,
      90,
      by = 10
    ),
    limits = c(
      0,
      90
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.01,
        0.03
      )
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      accuracy = 0.1,
      suffix = "%"
    ),
    expand = ggplot2::expansion(
      mult = c(
        0,
        0.08
      )
    )
  ) +
  ggplot2::labs(
    title =
      "Age-specific prevalence of underlying conditions",
    
    subtitle = paste0(
      "Lines show the median across countries; ",
      "shaded areas show the interquartile range"
    ),
    
    x =
      "Age, years",
    
    y =
      "Prevalence (%)",
    
    caption = paste0(
      "Countries are equally weighted. ",
      "Shaded intervals represent between-country variation, ",
      "not statistical uncertainty."
    )
  ) +
  theme_lancet_gbd(
    base_size = 10
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    ),
    
    strip.background =
      ggplot2::element_blank(),
    
    strip.text =
      ggplot2::element_text(
        face = "bold",
        size = 10,
        colour = "black"
      ),
    
    legend.position =
      "none",
    
    plot.subtitle =
      ggplot2::element_text(
        size = 9.5,
        colour = "grey30"
      ),
    
    plot.caption =
      ggplot2::element_text(
        size = 8.5,
        colour = "grey30",
        hjust = 0
      ),
    
    panel.spacing =
      grid::unit(
        1.1,
        "lines"
      )
  )

p_comorbidity_age

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

ggsave(
  filename = file.path(fig_dir, "fig_comorbidity_age_by_country.jpg"),
  plot = p_comorbidity_age,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

## ============================================================
## Population by comorbidity count, by region
## ============================================================
## Reuses bg_count_dist_pop, already built by 08_copula_simulation.R
## (3 comorbidity-count columns: prev_comorb_0 / prev_comorb_1 /
## prev_comorb_2plus, matching analysis_df's comorb_count_group scheme).

count_pop_long <- bg_count_dist_pop |>
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
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2+")
    )
  )

count_pop_long <- count_pop_long |>
  dplyr::mutate(
    region = countrycode::countrycode(
      iso3,
      origin = "iso3c",
      destination = "continent"
    )
  )

region_count_plot <- count_pop_long |>
  dplyr::filter(!is.na(region)) |>
  dplyr::group_by(
    region,
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
    region,
    age_start,
    age_end,
    age_mid
  ) |>
  dplyr::mutate(
    total_population = sum(population_count, na.rm = TRUE),
    population_percent = population_count / total_population
  ) |>
  dplyr::ungroup()

## count_palette defined by 09_copula_result_graphs.R, reused here.

p_abs <- region_count_plot |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      y = population_count / 1e6,
      fill = comorb_count_group
    )
  ) +
  ggplot2::geom_area(
    colour = "white",
    linewidth = 0.15,
    alpha = 0.98
  ) +
  ggplot2::facet_wrap(
    ~ region,
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    name = "Comorbidity count"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.04))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Population, millions",
    title = "Population by comorbidity count"
  ) +
  theme_lancet_gbd(base_size = 8)

ggsave(
  filename = file.path(fig_dir, "fig_region_comorb_share.jpg"),
  plot = p_abs,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)

region_pct_ribbon <- region_count_plot |>
  dplyr::mutate(
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("2+", "1", "0")
    )
  ) |>
  dplyr::arrange(
    region,
    age_mid,
    comorb_count_group
  ) |>
  dplyr::group_by(
    region,
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

p_pct <- region_pct_ribbon |>
  dplyr::arrange(
    region,
    comorb_count_group,
    age_mid
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age_mid,
      ymin = ymin,
      ymax = ymax,
      fill = comorb_count_group,
      group = interaction(region, comorb_count_group)
    )
  ) +
  ggplot2::geom_ribbon(
    alpha = 1,
    colour = NA
  ) +
  ggplot2::facet_wrap(
    ~ region,
    ncol = 1
  ) +
  ggplot2::scale_fill_manual(
    values = count_palette,
    breaks = c("0", "1", "2+"),
    name = "Comorbidity count"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 20),
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
    y = "Population, %",
    title = "Distribution by comorbidity count"
  ) +
  theme_lancet_gbd(base_size = 8) +
  ggplot2::theme(
    legend.position = "none"
  )

p_pct

ggsave(
  filename = file.path(fig_dir, "fig_region_comorb_stack.jpg"),
  plot = p_pct,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)