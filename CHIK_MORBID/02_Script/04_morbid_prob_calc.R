## 0. Output directories ######################################################

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)
library(Matrix)
bridge_comorb_cols <- c(
  "diabetes",
  "hypertension",
  "hepatopathy",
  "renal_disease",
  "hematologic",
  "peptic_ulcer",
  "autoimmune"
)

condition_labels <- c(
  diabetes      = "Diabetes",
  hypertension  = "Hypertension",
  hepatopathy   = "Liver disease",
  renal_disease = "Renal disease",
  hematologic   = "Haematologic disease",
  peptic_ulcer  = "Peptic ulcer disease",
  autoimmune    = "Autoimmune disease"
)

corr_long <- purrr::imap_dfr(corr_by_age_bridge, function(mat, age_band_name) {
  
  df <- as.data.frame(as.table(mat)) |>
    rename(
      condition1 = Var1,
      condition2 = Var2,
      correlation = Freq
    ) |>
    mutate(age_band = age_band_name)
  
  df
})

corr_plot_df <- corr_long |>
  mutate(
    condition1 = factor(condition1, levels = bridge_comorb_cols),
    condition2 = factor(condition2, levels = bridge_comorb_cols),
    i = as.integer(condition1),
    j = as.integer(condition2)
  ) |>
  filter(i >= j)   # lower triangle incl. diagonal

corr_plot_df <- corr_plot_df |>
  filter(i > j)

corr_plot_df <- corr_plot_df |>
  mutate(
    condition1_label = factor(
      condition_labels[as.character(condition1)],
      levels = condition_labels[bridge_comorb_cols]
    ),
    condition2_label = factor(
      condition_labels[as.character(condition2)],
      levels = condition_labels[bridge_comorb_cols]
    )
  )

theme_lancet_matrix <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        color = "black"
      ),
      axis.text.y = element_text(color = "black"),
      strip.text = element_text(face = "bold", color = "black"),
      strip.background = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "black"),
      plot.caption = element_text(size = 8, color = "grey30")
    )
}


p_corr_heatmap <- ggplot(
  corr_plot_df,
  aes(x = condition1, y = condition2, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~ age_band, nrow = 1) +
  scale_fill_gradient2(
    low = "#3B5B92",
    mid = "white",
    high = "#B23A48",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-0.5, 0, 0.5, 1),
    name = "Correlation"
  ) +
  coord_equal() +
  labs(
    title = "Age-specific co-occurrence patterns of comorbidities in Brazil",
    subtitle = "Observed pairwise correlations among seven comorbidities in confirmed chikungunya cases"
  ) +
  theme_lancet_matrix()

p_corr_heatmap

top_pairs <- corr_plot_df |>
  filter(i > j) |>
  group_by(age_band) |>
  slice_max(order_by = abs(correlation), n = 5, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    condition1_lab = condition_labels[as.character(condition1)],
    condition2_lab = condition_labels[as.character(condition2)],
    pair_label = paste(condition2_lab, condition1_lab, sep = " + ")
  )

p_top_pairs <- ggplot(
  top_pairs,
  aes(
    x = correlation,
    y = fct_reorder(pair_label, correlation),
    color = correlation,
    size = abs(correlation)
  )
) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey60", linewidth = 0.4) +
  geom_point(alpha = 0.9) +
  facet_wrap(~ age_band, scales = "free_y") +
  scale_color_gradient2(
    low = "#3B5B92",
    mid = "grey85",
    high = "#B23A48",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  scale_size_continuous(
    range = c(2.5, 5),
    name = "|Correlation|"
  ) +
  labs(
    x = "Pairwise correlation",
    y = NULL,
    title = "Top correlated comorbidity pairs by age band"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

p_top_pairs

p_corr_heatmap_sel <- ggplot(
  corr_plot_df,
  aes(
    x = condition1_label,
    y = condition2_label,
    fill = correlation
  )
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    data = corr_plot_df |> filter(abs(correlation) >= 0.10),
    aes(label = sprintf("%.2f", correlation)),
    size = 2.8
  ) +
  facet_wrap(~ age_band, nrow = 1) +
  scale_fill_gradient2(
    low = "#3B5B92",
    mid = "white",
    high = "#B23A48",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-0.5, 0, 0.5, 1),
    name = "Correlation"
  ) +
  coord_equal() +
  labs(
    title = "Age-specific co-occurrence patterns of comorbidities in Brazil"
  ) +
  theme_lancet_matrix()

p_corr_heatmap_sel

ggsave(
  filename = file.path(fig_dir, "fig_corr_heatmap.jpg"),
  plot = p_corr_heatmap_sel,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 300
)

##########
gbd_prev_cols <- paste0("bg_prev_", bridge_comorb_cols)

bg_prev_country |>
  dplyr::distinct(condition, age_start, age_end) |>
  dplyr::arrange(condition, age_start, age_end) |>
  tibble::as_tibble() |>
  print(n = 200)


target_age_grid <- tibble::tibble(
  target_age_start = c(
    0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
    50, 55, 60, 65, 70, 75, 80, 85, 90, 95
  ),
  target_age_end = c(
    4, 9, 14, 19, 24, 29, 34, 39, 44, 49,
    54, 59, 64, 69, 74, 79, 84, 89, 94, Inf
  )
) |>
  mutate(
    target_age_mid = ifelse(
      is.infinite(target_age_end),
      97.5,
      (target_age_start + target_age_end) / 2
    ),
    age_band_corr = case_when(
      target_age_start < 20 ~ "0-19",
      target_age_start < 40 ~ "20-39",
      target_age_start < 60 ~ "40-59",
      target_age_start < 80 ~ "60-79",
      target_age_start >= 80 ~ "80+",
      TRUE ~ NA_character_
    ),
    age_band_corr = factor(
      age_band_corr,
      levels = c("0-19", "20-39", "40-59", "60-79", "80+")
    )
  )

bg_prev_country_harmonised <- bg_prev_country_harmonised |>
  dplyr::mutate(
    country_name = dplyr::case_when(
      iso3 == "MKD" ~ "North Macedonia",
      TRUE ~ country_name
    )
  )

bg_prev_country_aligned <- bg_prev_country_harmonised |>
  dplyr::select(-age_band_corr) |>
  tidyr::crossing(target_age_grid) |>
  dplyr::filter(
    target_age_mid >= age_start,
    target_age_mid <= age_end_for_mapping
  ) |>
  dplyr::transmute(
    iso3,
    country_name,
    condition,
    age_start = target_age_start,
    age_end = target_age_end,
    age_band_corr,
    prevalence
  )

bg_prev_country_wide <- bg_prev_country_aligned |>
  dplyr::group_by(
    iso3,
    country_name,
    age_start,
    age_end,
    age_band_corr,
    condition
  ) |>
  dplyr::summarise(
    prevalence = mean(prevalence, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = condition,
    values_from = prevalence,
    names_prefix = "bg_prev_"
  )

country_lookup <- bg_prev_country_aligned |>
  dplyr::group_by(iso3) |>
  dplyr::summarise(
    country_name = dplyr::first(stats::na.omit(country_name)),
    .groups = "drop"
  )

bg_prev_country_wide <- bg_prev_country_aligned |>
  dplyr::group_by(
    iso3,
    age_start,
    age_end,
    age_band_corr,
    condition
  ) |>
  dplyr::summarise(
    prevalence = mean(prevalence, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = condition,
    values_from = prevalence,
    names_prefix = "bg_prev_"
  ) |>
  dplyr::left_join(country_lookup, by = "iso3") |>
  dplyr::select(
    iso3,
    country_name,
    age_start,
    age_end,
    age_band_corr,
    dplyr::all_of(gbd_prev_cols)
  )

bg_prev_country_complete <- bg_prev_country_wide |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(gbd_prev_cols),
      ~ !is.na(.x)
    )
  )

excluded_countries <- bg_prev_country_wide |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::all_of(gbd_prev_cols),
      is.na
    )
  ) |>
  dplyr::distinct(iso3, country_name) |>
  dplyr::arrange(iso3)

excluded_countries

### coupla func
simulate_count_distribution <- function(
    marginals,
    corr_mat,
    n_sim = 50000,
    seed = 1
) {
  
  if (any(is.na(marginals))) {
    stop("Marginal prevalence vector contains NA.")
  }
  
  if (any(!is.finite(marginals))) {
    stop("Marginal prevalence vector contains non-finite values.")
  }
  
  if (any(marginals < 0 | marginals > 1)) {
    stop("Marginal prevalence values must be between 0 and 1.")
  }
  
  vars <- names(marginals)
  
  corr_mat <- corr_mat[vars, vars]
  
  n_corr_nonfinite <- sum(!is.finite(corr_mat))
  
  corr_mat[!is.finite(corr_mat)] <- 0
  diag(corr_mat) <- 1
  
  corr_mat_pd <- Matrix::nearPD(
    corr_mat,
    corr = TRUE
  )$mat |>
    as.matrix()
  
  set.seed(seed)
  
  E <- matrix(
    rnorm(n_sim * length(vars)),
    nrow = n_sim,
    ncol = length(vars)
  )
  
  Z <- E %*% chol(corr_mat_pd)
  colnames(Z) <- vars
  
  thresholds <- qnorm(1 - marginals)
  
  X <- sweep(
    Z,
    2,
    thresholds,
    FUN = ">"
  )
  
  n_comorb <- rowSums(X)
  
  count_dist <- tibble::tibble(
    comorb_count_group = dplyr::case_when(
      n_comorb == 0 ~ "0",
      n_comorb == 1 ~ "1",
      n_comorb == 2 ~ "2",
      n_comorb >= 3 ~ "3+"
    )
  ) |>
    dplyr::count(comorb_count_group, name = "n") |>
    dplyr::mutate(
      prevalence = n / n_sim
    )
  
  count_dist <- tibble::tibble(
    comorb_count_group = c("0", "1", "2", "3+")
  ) |>
    dplyr::left_join(count_dist, by = "comorb_count_group") |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, n),
      prevalence = dplyr::if_else(is.na(prevalence), 0, prevalence)
    )
  
  sim_marginals <- colMeans(X)
  
  list(
    count_dist = count_dist,
    sim_marginals = sim_marginals,
    corr_used = corr_mat_pd,
    n_corr_nonfinite_replaced = n_corr_nonfinite
  )
}
bg_prev_country_complete <- bg_prev_country_complete |>
  dplyr::mutate(row_id = dplyr::row_number())

bg_count_dist <- purrr::map_dfr(
  seq_len(nrow(bg_prev_country_complete)),
  function(i) {
    
    row_i <- bg_prev_country_complete[i, ]
    
    marginals_i <- row_i |>
      dplyr::select(dplyr::all_of(gbd_prev_cols)) |>
      unlist() |>
      as.numeric()
    
    names(marginals_i) <- bridge_comorb_cols
    
    age_band_i <- as.character(row_i$age_band_corr)
    
    corr_i <- corr_by_age_bridge[[age_band_i]]
    corr_i <- corr_i[bridge_comorb_cols, bridge_comorb_cols]
    
    sim_i <- simulate_count_distribution(
      marginals = marginals_i,
      corr_mat = corr_i,
      n_sim = 10000,
      seed = 1000 + i
    )
    
    sim_i$count_dist |>
      dplyr::mutate(
        row_id = row_i$row_id,
        iso3 = row_i$iso3,
        country_name = row_i$country_name,
        age_start = row_i$age_start,
        age_end = row_i$age_end,
        age_band_corr = row_i$age_band_corr
      ) |>
      dplyr::select(
        row_id,
        iso3,
        country_name,
        age_start,
        age_end,
        age_band_corr,
        comorb_count_group,
        prevalence
      )
  }
)

bg_count_dist_wide <- bg_count_dist |>
  dplyr::mutate(
    comorb_count_group_clean = dplyr::case_when(
      comorb_count_group == "0" ~ "0",
      comorb_count_group == "1" ~ "1",
      comorb_count_group == "2" ~ "2",
      comorb_count_group == "3+" ~ "3plus"
    )
  ) |>
  dplyr::select(
    row_id,
    iso3,
    country_name,
    age_start,
    age_end,
    age_band_corr,
    comorb_count_group_clean,
    prevalence
  ) |>
  tidyr::pivot_wider(
    names_from = comorb_count_group_clean,
    values_from = prevalence,
    names_prefix = "prev_comorb_"
  )

bg_count_dist_wide |>
  dplyr::mutate(
    prob_sum =
      prev_comorb_0 +
      prev_comorb_1 +
      prev_comorb_2 +
      prev_comorb_3plus
  ) |>
  dplyr::summarise(
    min_prob_sum = min(prob_sum),
    max_prob_sum = max(prob_sum)
  )


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
      prev_comorb_2,
      prev_comorb_3plus
    ),
    names_to = "comorb_count_group",
    values_to = "prevalence"
  ) |>
  dplyr::mutate(
    comorb_count_group = dplyr::case_when(
      comorb_count_group == "prev_comorb_0" ~ "0",
      comorb_count_group == "prev_comorb_1" ~ "1",
      comorb_count_group == "prev_comorb_2" ~ "2",
      comorb_count_group == "prev_comorb_3plus" ~ "3+"
    ),
    comorb_count_group = factor(
      comorb_count_group,
      levels = c("0", "1", "2", "3+")
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


p_heatmap_3plus <- bg_count_dist_wide |>
  dplyr::filter(iso3 %in% selected_iso3_heatmap) |>
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
      y = forcats::fct_reorder(country_name, prev_comorb_3plus, .fun = max),
      fill = prev_comorb_3plus
    )
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(
    labels = scales::percent_format(accuracy = 1),
    name = "P(3+)"
  ) +
  ggplot2::labs(
    x = "Age group",
    y = NULL,
    title = "Estimated prevalence of 3+ comorbidities by country and age"
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

p_heatmap_3plus

ggsave(
  filename = file.path(fig_dir, "fig_heatmap_3plus.jpg"),
  plot = p_heatmap_3plus,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)

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
      pop_comorb_2,
      pop_comorb_3plus
    ),
    names_to = "comorb_count_group",
    values_to = "population_count"
  ) |>
  dplyr::mutate(
    comorb_count_group = dplyr::case_when(
      comorb_count_group == "pop_comorb_0" ~ "0",
      comorb_count_group == "pop_comorb_1" ~ "1",
      comorb_count_group == "pop_comorb_2" ~ "2",
      comorb_count_group == "pop_comorb_3plus" ~ "3+"
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
      levels = c("3+", "2", "1", "0")
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
    breaks = c("0", "1", "2", "3+"),
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
  theme_lancet_clean(base_size = 9) +
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



###
simulate_count_distribution <- function(
    marginals,
    corr_mat,
    n_sim = 50000,
    seed = 1
) {
  
  if (any(is.na(marginals))) {
    stop("Marginal prevalence vector contains NA.")
  }
  
  if (any(!is.finite(marginals))) {
    stop("Marginal prevalence vector contains non-finite values.")
  }
  
  if (any(marginals < 0 | marginals > 1)) {
    stop("Marginal prevalence values must be between 0 and 1.")
  }
  
  vars <- names(marginals)
  
  corr_mat <- corr_mat[vars, vars]
  
  n_corr_nonfinite <- sum(!is.finite(corr_mat))
  
  corr_mat[!is.finite(corr_mat)] <- 0
  diag(corr_mat) <- 1
  
  corr_mat_pd <- Matrix::nearPD(
    corr_mat,
    corr = TRUE
  )$mat |>
    as.matrix()
  
  set.seed(seed)
  
  E <- matrix(
    rnorm(n_sim * length(vars)),
    nrow = n_sim,
    ncol = length(vars)
  )
  
  Z <- E %*% chol(corr_mat_pd)
  colnames(Z) <- vars
  
  thresholds <- qnorm(1 - marginals)
  
  X <- sweep(
    Z,
    2,
    thresholds,
    FUN = ">"
  )
  
  X <- as.data.frame(X)
  
  n_comorb <- rowSums(X)
  
  ## 1) Existing count distribution: 0, 1, 2, 3+
  count_dist <- tibble::tibble(
    comorb_count_group = dplyr::case_when(
      n_comorb == 0 ~ "0",
      n_comorb == 1 ~ "1",
      n_comorb == 2 ~ "2",
      n_comorb >= 3 ~ "3+"
    )
  ) |>
    dplyr::count(comorb_count_group, name = "n") |>
    dplyr::mutate(
      prevalence = n / n_sim
    )
  
  count_dist <- tibble::tibble(
    comorb_count_group = c("0", "1", "2", "3+")
  ) |>
    dplyr::left_join(count_dist, by = "comorb_count_group") |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, n),
      prevalence = dplyr::if_else(is.na(prevalence), 0, prevalence)
    )
  
  ## 2) New mutually exclusive condition-category distribution
  exclusive_group <- dplyr::case_when(
    n_comorb == 0 ~ "No selected condition",
    n_comorb >= 2 ~ "Multimorbidity, ≥2 selected conditions",
    n_comorb == 1 & X$hypertension  ~ "Hypertension only",
    n_comorb == 1 & X$diabetes      ~ "Diabetes only",
    n_comorb == 1 & X$renal_disease ~ "Renal disease only",
    n_comorb == 1 & X$hepatopathy   ~ "Liver disease only",
    n_comorb == 1 & X$hematologic   ~ "Haematologic disease only",
    n_comorb == 1 & X$peptic_ulcer  ~ "Peptic ulcer disease only",
    n_comorb == 1 & X$autoimmune    ~ "Autoimmune disease only",
    TRUE ~ NA_character_
  )
  
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
  
  exclusive_dist <- tibble::tibble(
    exclusive_group = exclusive_group
  ) |>
    dplyr::count(exclusive_group, name = "n") |>
    dplyr::mutate(
      prevalence = n / n_sim
    )
  
  exclusive_dist <- tibble::tibble(
    exclusive_group = exclusive_levels
  ) |>
    dplyr::left_join(exclusive_dist, by = "exclusive_group") |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, n),
      prevalence = dplyr::if_else(is.na(prevalence), 0, prevalence),
      exclusive_group = factor(
        exclusive_group,
        levels = exclusive_levels
      )
    )
  
  sim_marginals <- colMeans(X)
  
  list(
    count_dist = count_dist,
    exclusive_dist = exclusive_dist,
    sim_marginals = sim_marginals,
    corr_used = corr_mat_pd,
    n_corr_nonfinite_replaced = n_corr_nonfinite
  )
}


bg_exclusive_dist <- purrr::map_dfr(
  seq_len(nrow(bg_prev_country_complete)),
  function(i) {
    
    row_i <- bg_prev_country_complete[i, ]
    
    marginals_i <- row_i |>
      dplyr::select(all_of(gbd_prev_cols)) |>
      unlist() |>
      as.numeric()
    
    names(marginals_i) <- bridge_comorb_cols
    
    age_band_i <- as.character(row_i$age_band_corr)
    
    corr_i <- corr_by_age_bridge[[age_band_i]]
    corr_i <- corr_i[bridge_comorb_cols, bridge_comorb_cols]
    
    sim_i <- simulate_count_distribution(
      marginals = marginals_i,
      corr_mat = corr_i,
      n_sim = 10000,
      seed = 1000 + i
    )
    
    sim_i$exclusive_dist |>
      mutate(
        row_id = row_i$row_id,
        iso3 = row_i$iso3,
        country_name = row_i$country_name,
        age_start = row_i$age_start,
        age_end = row_i$age_end,
        age_band_corr = row_i$age_band_corr
      ) |>
      dplyr::select(
        row_id,
        iso3,
        country_name,
        age_start,
        age_end,
        age_band_corr,
        exclusive_group,
        prevalence
      )
  }
)

selected_iso3 <- c("BRA", "IND", "KOR", "GBR", "ZMB")

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
  filter(iso3 %in% selected_iso3) |>
  mutate(
    age_group = case_when(
      is.infinite(age_end) ~ paste0(age_start, "+"),
      TRUE ~ paste0(age_start, "-", age_end)
    ),
    age_mid = case_when(
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

exclusive_plot_df <- exclusive_plot_df |>
  mutate(
    exclusive_group = factor(
      exclusive_group,
      levels = exclusive_levels
    )
  )

p_exclusive_stacked_area <- ggplot(
  exclusive_plot_df,
  aes(
    x = age_mid,
    y = prevalence,
    fill = exclusive_group,
    group = exclusive_group
  )
) +
  geom_area(
    stat = "identity",
    position = "stack",
    alpha = 1,
    colour = NA,
    linewidth = 0
  ) +
  facet_wrap(
    ~ country_name,
    ncol = 2
  ) +
  scale_fill_manual(
    values = exclusive_palette,
    breaks = exclusive_levels,
    name = "Category"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(
    x = "Age, years",
    y = "Share of population",
    title = "Age-specific distribution of selected condition categories"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.45, "cm"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30")
  ) +
  guides(
    fill = guide_legend(
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
