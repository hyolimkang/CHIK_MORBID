## ============================================================
## Brazil SINAN observed marginal hospitalisation probability
## Comorbidity groups pooled
## ============================================================

brazil_observed_hosp_by_age <-
  analysis_df |>
  dplyr::filter(
    !is.na(age_years),
    age_years >= 0,
    age_years < 80,
    !is.na(hosp_only)
  ) |>
  dplyr::mutate(
    burden_age_group = cut(
      age_years,
      breaks = c(seq(0, 80, by = 10), Inf),
      right = FALSE,
      include.lowest = TRUE,
      labels = c(
        "[0,10)", "[10,20)", "[20,30)", "[30,40)",
        "[40,50)", "[50,60)", "[60,70)", "[70,80)", "80+"
      )
    )
  ) |>
  dplyr::filter(
    !is.na(burden_age_group)
  ) |>
  dplyr::group_by(
    burden_age_group
  ) |>
  dplyr::summarise(
    n_total = dplyr::n(),
    n_hospitalised = sum(as.numeric(hosp_only)),
    observed_hosp_rate = n_hospitalised / n_total,
    .groups = "drop"
  )

age_levels <- c(
  "[0,10)", "[10,20)", "[20,30)", "[30,40)",
  "[40,50)", "[50,60)", "[60,70)", "[70,80)", "80+"
)

brazil_observed_hosp_by_age <-
  brazil_observed_hosp_by_age |>
  dplyr::rowwise() |>
  dplyr::mutate(
    ci = list(
      binom::binom.confint(
        x = n_hospitalised,
        n = n_total,
        methods = "wilson"
      )
    ),
    lower = ci$lower,
    upper = ci$upper
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    -ci
  ) |>
  dplyr::mutate(
    source = "Brazil SINAN observed",
    burden_age_group = factor(
      as.character(burden_age_group),
      levels = age_levels
    )
  )


p_brazil_observed_hosp_age <-
  ggplot2::ggplot(
    brazil_observed_hosp_by_age,
    ggplot2::aes(
      x = burden_age_group,
      y = observed_hosp_rate * 100
    )
  ) +
  ggplot2::geom_col(
    width = 0.72,
    fill = "steelblue"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower * 100,
      ymax = upper * 100
    ),
    width = 0.16,
    linewidth = 0.5
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      accuracy = 0.1,
      suffix = "%"
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  ggplot2::labs(
    title =
      "Observed marginal hospitalisation probability in Brazil SINAN",
    subtitle =
      "Comorbidity groups pooled; error bars show 95% Wilson confidence intervals",
    x = "Age group, years",
    y = "Hospitalisation probability"
  ) +
  theme_lancet_clean(
    base_size = 10
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

p_brazil_observed_hosp_age

saveRDS(
  brazil_observed_hosp_by_age,
  file = "01_Data/brazil_observed_hosp_by_age.rds"
)
