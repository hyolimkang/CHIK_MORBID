# ---------------------------------------------------------------------------
# 05_corr_matrix.R
#
# Computes the correlation matrix between the 7 comorbidities, by age band,
# from the Brazil SINAN cohort (analysis_df, from 03_relative_risk.R). This
# is corr_by_age_bridge, consumed by 06_morbid_prob_calc.R's copula
# simulation. GBD/background data is NOT used for this - it is purely the
# observed co-occurrence pattern in the Brazil individual-level data.
#
# Needs (from earlier scripts, already in the session):
#   analysis_df     from 03_relative_risk.R
#   bg_prev_country  from 04_background_prevalence.R
# ---------------------------------------------------------------------------

bg_prev_brazil <- bg_prev_country |>
  filter(iso3 == "BRA") |>
  dplyr::select(condition, age_start, age_end, prevalence)

bg_prev_brazil |> count(condition)

analysis_df <- analysis_df |>
  mutate(.row_id = dplyr::row_number())

analysis_df <- analysis_df |>
  left_join(
    bg_prev_brazil,
    by = join_by(age_years >= age_start, age_years <= age_end)
  ) |>
  dplyr::select(-age_start, -age_end)

analysis_df <- analysis_df |>
  tidyr::pivot_wider(
    names_from   = condition,
    values_from  = prevalence,
    names_prefix = "bg_prev_"
  )

bridge_comorb_cols <- c(
  "diabetes",
  "hypertension",
  "hepatopathy",
  "renal_disease",
  "hematologic",
  "peptic_ulcer",
  "autoimmune"
)

gbd_prev_cols <- paste0("bg_prev_", bridge_comorb_cols)

comorb_bin_bridge <- analysis_df |>
  mutate(
    across(
      all_of(bridge_comorb_cols),
      ~ as.integer(.x == "yes")
    )
  ) |>
  dplyr::select(
    age_years,
    all_of(bridge_comorb_cols)
  ) |>
  mutate(
    age_band_corr = cut(
      age_years,
      breaks = c(0, 20, 40, 60, 80, 101),
      labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

corr_by_age_bridge <- comorb_bin_bridge |>
  group_by(age_band_corr) |>
  group_split() |>
  setNames(levels(comorb_bin_bridge$age_band_corr)) |>
  purrr::map(function(df) {
    cor(
      df |> dplyr::select(all_of(bridge_comorb_cols)),
      use = "pairwise.complete.obs"
    )
  })

corr_by_age_bridge[["60-79"]]

analysis_df <- analysis_df |>
  dplyr::select(
    -matches("^bg_prev_.*\\.x$"),
    -matches("^bg_prev_.*\\.y$")
  )
