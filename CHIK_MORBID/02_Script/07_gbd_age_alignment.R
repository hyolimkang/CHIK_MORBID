# ---------------------------------------------------------------------------
# 07_gbd_age_alignment.R
#
# Pure data-engineering step: GBD's age bands (04_background_prevalence.R's
# bg_prev_country - a mix of 5-year bands, "<5", and open-ended "95+") don't
# line up with each other consistently across conditions/sources, and need
# to be re-expressed on one shared age grid before the copula simulation in
# 08_copula_simulation.R can pull a 7-condition marginal-prevalence vector
# for a given country and age. Builds bg_prev_country_wide (one row per
# country x age-band, one column per condition) and bg_prev_country_complete
# (the subset with no missing condition, i.e. usable for simulation).
#
# Needs (from earlier scripts, already in the session):
#   bg_prev_country                          from 04_background_prevalence.R
#   bridge_comorb_cols                       from 05_corr_matrix.R
# ---------------------------------------------------------------------------

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

bg_prev_country_harmonised <- bg_prev_country |>
  dplyr::mutate(
    country_name = dplyr::case_when(
      iso3 == "MKD" ~ "North Macedonia",
      TRUE ~ country_name
    ),

    age_start = as.numeric(age_start),
    age_end = as.numeric(age_end),

    age_end_for_mapping = dplyr::if_else(
      is.na(age_end),
      Inf,
      age_end
    )
  )

# Assign each GBD age band's midpoint-matching target_age_grid rows: a GBD
# row covering e.g. age 20-64 gets expanded to the 20-24, 25-29, ..., 60-64
# rows of the shared grid, all carrying that same prevalence value.
bg_prev_country_aligned <- bg_prev_country_harmonised |>
  dplyr::select(
    -dplyr::any_of("age_band_corr")
  ) |>
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

# Countries dropped because at least one of the 7 conditions has no GBD/
# hypertension estimate for them - simulate_count_distribution() cannot run
# without a complete 7-condition marginal-prevalence vector.
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
