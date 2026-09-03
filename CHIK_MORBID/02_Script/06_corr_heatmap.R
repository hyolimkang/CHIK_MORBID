# ---------------------------------------------------------------------------
# 06_corr_heatmap.R
#
# Visualises the Brazil-derived comorbidity correlation matrix
# (corr_by_age_bridge, from 05_corr_matrix.R) as a set of heatmaps - one
# tile grid per age band, plus a "top correlated pairs" dot plot. Purely
# descriptive: does not touch GBD/background prevalence data at all, and is
# not needed by anything downstream (08_copula_simulation.R uses
# corr_by_age_bridge directly, not this script's output).
#
# Needs (from earlier scripts, already in the session):
#   corr_by_age_bridge, bridge_comorb_cols   from 05_corr_matrix.R
# ---------------------------------------------------------------------------

fig_dir <- "03_Output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

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
