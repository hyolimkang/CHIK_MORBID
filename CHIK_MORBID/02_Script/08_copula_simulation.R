# ---------------------------------------------------------------------------
# 08_copula_simulation.R
#
# The core scientific computation of the background-burden pipeline. For
# every country x age-band row in bg_prev_country_complete (07), simulates
# 10,000 individuals via a Gaussian copula that:
#   - gives each of the 7 conditions its GBD/hypertension marginal
#     prevalence for that country and age band (07)
#   - reproduces the Brazil-derived correlation structure between
#     conditions for that age band (corr_by_age_bridge, 05)
# and tabulates, per simulated individual, both:
#   - comorb_count_group: "0" / "1" / "2+" (matches the scheme used
#     throughout 03_relative_risk.R)
#   - exclusive_group: which single condition (if any) they have, or
#     "Multimorbidity" if 2+, or "No selected condition" if 0
#
# NOTE ON THE 2026-09 RESTRUCTURING: this used to be two near-identical
# copies of simulate_count_distribution() and two full simulation passes -
# one computing only comorb_count_group, a second (added later) recomputing
# everything again just to also get exclusive_group. They've been merged:
# one function returns both, and the simulation runs once per row.
#
# Needs (from earlier scripts, already in the session):
#   corr_by_age_bridge, bridge_comorb_cols          from 05_corr_matrix.R
#   bg_prev_country_complete, gbd_prev_cols          from 07_gbd_age_alignment.R
#   pop_country_age                                  from 04_background_prevalence.R
#
# Output:
#   bg_count_dist_wide      country x age-band x {prev_comorb_0/1/2plus}
#   bg_exclusive_dist       country x age-band x exclusive_group x prevalence
#   bg_count_dist_pop       bg_count_dist_wide, population-weighted
#   -> saved: 01_Data/bg_count_dist_wide.RData, 01_Data/bg_exclusive_dist.RData
# ---------------------------------------------------------------------------

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

  # Restrict the correlation matrix to the conditions being simulated, and
  # replace any non-finite entries with 0 (no assumed correlation) so a
  # single missing pairwise correlation doesn't stop the whole row.
  corr_mat <- corr_mat[vars, vars]
  n_corr_nonfinite <- sum(!is.finite(corr_mat))
  corr_mat[!is.finite(corr_mat)] <- 0
  diag(corr_mat) <- 1

  # nearPD: the empirical correlation matrix isn't guaranteed to be positive
  # definite (needed for the Cholesky decomposition below), so project it
  # onto the nearest valid correlation matrix first.
  corr_mat_pd <- Matrix::nearPD(
    corr_mat,
    corr = TRUE
  )$mat |>
    as.matrix()

  set.seed(seed)

  # Standard Gaussian copula: correlated normal draws, thresholded per
  # condition at the z-score matching its target marginal prevalence.
  E <- matrix(
    stats::rnorm(n_sim * length(vars)),
    nrow = n_sim,
    ncol = length(vars)
  )
  Z <- E %*% chol(corr_mat_pd)
  colnames(Z) <- vars

  thresholds <- stats::qnorm(1 - marginals)

  X <- sweep(
    Z,
    2,
    thresholds,
    FUN = ">"
  )
  X <- as.data.frame(X)

  n_comorb <- rowSums(X)

  ## ---- 1) comorb_count_group distribution: "0" / "1" / "2+" -------------

  count_dist <- tibble::tibble(
    comorb_count_group = dplyr::case_when(
      n_comorb == 0 ~ "0",
      n_comorb == 1 ~ "1",
      n_comorb >= 2 ~ "2+",
      TRUE ~ NA_character_
    )
  ) |>
    dplyr::count(
      comorb_count_group,
      name = "n"
    ) |>
    dplyr::mutate(
      prevalence = n / n_sim
    )

  # Keep all 3 categories even if one had 0 simulated individuals
  count_dist <- tibble::tibble(
    comorb_count_group = c("0", "1", "2+")
  ) |>
    dplyr::left_join(
      count_dist,
      by = "comorb_count_group"
    ) |>
    dplyr::mutate(
      n = dplyr::coalesce(n, 0L),
      prevalence = dplyr::coalesce(prevalence, 0)
    )

  ## ---- 2) mutually exclusive condition-category distribution -----------

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
      n = dplyr::coalesce(n, 0L),
      prevalence = dplyr::coalesce(prevalence, 0),
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

# ---- Run the simulation once per country x age-band row -------------------

bg_prev_country_complete <- bg_prev_country_complete |>
  dplyr::mutate(row_id = dplyr::row_number())

bg_sim_results <- purrr::map(
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

    meta_cols <- function(df) {
      df |>
        dplyr::mutate(
          row_id = row_i$row_id,
          iso3 = row_i$iso3,
          country_name = row_i$country_name,
          age_start = row_i$age_start,
          age_end = row_i$age_end,
          age_band_corr = row_i$age_band_corr
        )
    }

    list(
      count_dist = sim_i$count_dist |>
        meta_cols() |>
        dplyr::select(
          row_id, iso3, country_name, age_start, age_end, age_band_corr,
          comorb_count_group, prevalence
        ),
      exclusive_dist = sim_i$exclusive_dist |>
        meta_cols() |>
        dplyr::select(
          row_id, iso3, country_name, age_start, age_end, age_band_corr,
          exclusive_group, prevalence
        )
    )
  }
)

bg_count_dist <- purrr::map_dfr(bg_sim_results, "count_dist")
bg_exclusive_dist <- purrr::map_dfr(bg_sim_results, "exclusive_dist")

# ---- Reshape comorb_count_group results to one row per country x age-band -

bg_count_dist_wide <- bg_count_dist |>
  dplyr::mutate(
    comorb_count_group_column = dplyr::case_when(
      comorb_count_group == "0" ~ "0",
      comorb_count_group == "1" ~ "1",
      comorb_count_group == "2+" ~ "2plus",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(
    row_id,
    iso3,
    country_name,
    age_start,
    age_end,
    age_band_corr,
    comorb_count_group_column,
    prevalence
  ) |>
  tidyr::pivot_wider(
    names_from = comorb_count_group_column,
    values_from = prevalence,
    names_prefix = "prev_comorb_",
    values_fill = 0
  )

# Sanity check: the 3 comorbidity-count probabilities should sum to ~1 for
# every row.
bg_count_dist_wide |>
  dplyr::mutate(
    prob_sum =
      prev_comorb_0 +
      prev_comorb_1 +
      prev_comorb_2plus
  ) |>
  dplyr::summarise(
    min_prob_sum = min(prob_sum),
    max_prob_sum = max(prob_sum)
  )

bg_count_dist_wide <- bg_count_dist_wide |>
  dplyr::mutate(
    burden_age_group = dplyr::case_when(
      age_start < 10 ~ "[0,10)",
      age_start < 20 ~ "[10,20)",
      age_start < 30 ~ "[20,30)",
      age_start < 40 ~ "[30,40)",
      age_start < 50 ~ "[40,50)",
      age_start < 60 ~ "[50,60)",
      age_start < 70 ~ "[60,70)",
      age_start < 80 ~ "[70,80)",
      age_start >= 80 ~ "[80,+)",
      TRUE ~ NA_character_
    ),
    burden_age_group = factor(
      burden_age_group,
      levels = c(
        "[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)",
        "[50,60)", "[60,70)", "[70,80)", "[80,+)"
      )
    )
  )

# ---- Population-weighted version -------------------------------------------

bg_count_dist_pop <- bg_count_dist_wide |>
  dplyr::left_join(
    pop_country_age,
    by = c("iso3", "age_start", "age_end"),
    suffix = c("", "_pop")
  ) |>
  dplyr::mutate(
    country_name = dplyr::coalesce(country_name, country_name_pop)
  ) |>
  dplyr::select(
    -dplyr::any_of("country_name_pop")
  ) |>
  dplyr::mutate(
    pop_comorb_0     = population * prev_comorb_0,
    pop_comorb_1     = population * prev_comorb_1,
    pop_comorb_2plus = population * prev_comorb_2plus
  )

save(bg_count_dist_wide, file = "01_Data/bg_count_dist_wide.RData")
save(bg_exclusive_dist, file = "01_Data/bg_exclusive_dist.RData")

message(sprintf(
  "[08] simulated %s country x age-band rows (%d countries)",
  format(nrow(bg_prev_country_complete), big.mark = ","),
  dplyr::n_distinct(bg_prev_country_complete$iso3)
))
