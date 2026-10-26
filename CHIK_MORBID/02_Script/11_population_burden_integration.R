# ---------------------------------------------------------------------------
# 11_population_burden_integration.R
#
# The final step the rest of the pipeline builds toward: instead of the
# comorb_count_group RR (03) or single-condition RR (03, section 3-3) -
# both of which compare crude "condition present" vs "condition absent"
# groups that are themselves contaminated by co-occurring conditions
# (05's correlation matrix shows these 7 conditions cluster in the same
# people) - this fits ONE multivariable model with all 7 conditions as
# simultaneous covariates, so each condition's coefficient is adjusted for
# the other 6. That model is then evaluated directly on the *simulated*
# population from 08_copula_simulation.R (which already carries each
# simulated individual's full 7-condition profile, not just a summary
# category), giving an expected hospitalisation/death probability that
# properly integrates over the population's joint comorbidity structure -
# no need to force a match between "RR groups" and "population groups".
#
# Needs (from earlier scripts, already in the session):
#   analysis_df, comorb_cols                        from 03_relative_risk.R
#   corr_by_age_bridge, bridge_comorb_cols           from 05_corr_matrix.R
#   bg_prev_country_complete, gbd_prev_cols          from 07_gbd_age_alignment.R
#   target_age_grid                                  from 07_gbd_age_alignment.R
#
# Output:
#   fit_hosp_full, fit_death_full        the fitted multivariable GAMs
#   burden_by_country_age                expected P(hosp)/P(death) per
#                                         country x age-band, plus the
#                                         ratio against a zero-comorbidity
#                                         baseline at the same age
#   -> saved: 01_Data/burden_by_country_age.RData
# ---------------------------------------------------------------------------

## ============================================================
## 1. Cohort additions: region, calendar year, multimorbidity flag
## ============================================================

analysis_df <- analysis_df |>
  mutate(
    year_cat = factor(lubridate::year(event_date)),
    region = dplyr::case_when(
      substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 1) == "1" ~ "Norte",
      substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 1) == "2" ~ "Nordeste",
      substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 1) == "3" ~ "Sudeste",
      substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 1) == "4" ~ "Sul",
      substr(dplyr::coalesce(muni_residence6, muni_notif6), 1, 1) == "5" ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    # Sudeste is the modal region (largest N) - natural reference level
    region = factor(region, levels = c("Sudeste", "Nordeste", "Norte", "Centro-Oeste", "Sul")),
    multimorbid_2plus = n_comorbid_model >= 2
  )

stopifnot(!anyNA(analysis_df$region))

## ============================================================
## 2. Full multivariable model: all 7 conditions as simultaneous covariates
## ============================================================
## logit P(H) = s(age) + sex + region + year + sum(beta_j * C_j) +
##              gamma * I(sum(C_j) >= 2)
## Main effects only to start; age x condition interactions are added
## below and kept only if AIC improves (matches the model-selection
## approach already used for the death GAM in 03_relative_risk.R).

full_main_rhs <- c(
  "s(age_years, bs = 'ts', k = 8)",
  "sex", "region", "year_cat",
  comorb_cols,
  "multimorbid_2plus"
)

fit_hosp_full_main <- mgcv::gam(
  reformulate(full_main_rhs, response = "hosp_only"),
  data = analysis_df,
  family = binomial(link = "logit"),
  method = "REML"
)

fit_death_full_main <- mgcv::gam(
  reformulate(full_main_rhs, response = "death_only"),
  data = analysis_df,
  family = binomial(link = "logit"),
  method = "REML",
  na.action = na.exclude
)

## Candidate extension: let hypertension (the most prevalent condition,
## n ~ 79,000) have its own age smooth, rather than assuming its effect
## is constant across age like every other condition.
full_ageint_rhs <- c(
  "s(age_years, bs = 'ts', k = 8)",
  "s(age_years, by = hypertension, bs = 'ts', k = 6)",
  "sex", "region", "year_cat",
  comorb_cols,
  "multimorbid_2plus"
)

fit_hosp_full_ageint <- mgcv::gam(
  reformulate(full_ageint_rhs, response = "hosp_only"),
  data = analysis_df,
  family = binomial(link = "logit"),
  method = "REML"
)

fit_death_full_ageint <- mgcv::gam(
  reformulate(full_ageint_rhs, response = "death_only"),
  data = analysis_df,
  family = binomial(link = "logit"),
  method = "REML",
  na.action = na.exclude
)

aic_hosp <- AIC(fit_hosp_full_main, fit_hosp_full_ageint)
aic_death <- AIC(fit_death_full_main, fit_death_full_ageint)

print(aic_hosp)
print(aic_death)

fit_hosp_full <- if (aic_hosp["fit_hosp_full_ageint", "AIC"] < aic_hosp["fit_hosp_full_main", "AIC"]) {
  message("[11] hosp: age x hypertension interaction improves AIC, keeping it")
  fit_hosp_full_ageint
} else {
  message("[11] hosp: main-effects-only model preferred by AIC")
  fit_hosp_full_main
}

fit_death_full <- if (aic_death["fit_death_full_ageint", "AIC"] < aic_death["fit_death_full_main", "AIC"]) {
  message("[11] death: age x hypertension interaction improves AIC, keeping it")
  fit_death_full_ageint
} else {
  message("[11] death: main-effects-only model preferred by AIC")
  fit_death_full_main
}

summary(fit_hosp_full)
summary(fit_death_full)

## ============================================================
## 3. Region/year/sex standardisation weights
## ============================================================
## Predictions below are standardised (weighted-averaged) over the
## observed Brazil-wide distribution of sex x region x year, the same
## way 03_relative_risk.R standardises over sex alone. Point estimates
## only (no bootstrap CI) - this feeds a large simulation loop below
## where a full parametric bootstrap per draw would be intractable.

strata_weights <- analysis_df |>
  dplyr::count(sex, region, year_cat, name = "n") |>
  dplyr::mutate(weight = n / sum(n)) |>
  dplyr::select(sex, region, year_cat, weight)

## ============================================================
## 4. Standardised prediction for an arbitrary comorbidity profile
## ============================================================
## comorb_status: named logical vector, e.g. c(diabetes = TRUE, ...),
## missing conditions default to FALSE ("no"). Returns one probability
## per age in `ages`, standardised over strata_weights.

predict_standardised <- function(fit_model, ages, comorb_status = NULL) {

  status <- setNames(rep("no", length(comorb_cols)), comorb_cols)
  if (!is.null(comorb_status)) {
    status[names(comorb_status)] <- ifelse(comorb_status, "yes", "no")
  }
  n_comorb <- sum(status == "yes")

  pred_df <- tidyr::expand_grid(
    age_years = ages,
    strata_weights
  )

  for (v in comorb_cols) {
    pred_df[[v]] <- factor(status[[v]], levels = c("no", "yes"))
  }
  pred_df$multimorbid_2plus <- n_comorb >= 2

  pred_df$p <- predict(fit_model, newdata = pred_df, type = "response")

  pred_df |>
    dplyr::group_by(age_years) |>
    dplyr::summarise(p = sum(p * weight) / sum(weight), .groups = "drop") |>
    dplyr::pull(p, name = age_years)
}

## Reference (zero-comorbidity) risk by single year of age, 0-100
target_ages <- 0:100
ref_hosp_by_age <- predict_standardised(fit_hosp_full, target_ages)
ref_death_by_age <- predict_standardised(fit_death_full, target_ages)

## ============================================================
## 5. Precompute predicted probability for all 128 comorbidity
##    combinations, at each of the 20 GBD age-band midpoints
## ============================================================

combo_grid <- expand.grid(
  setNames(rep(list(c(FALSE, TRUE)), length(bridge_comorb_cols)), bridge_comorb_cols)
)

target_age_mids <- sort(unique(target_age_grid$target_age_mid))
target_age_mids <- target_age_mids[is.finite(target_age_mids)]

message(sprintf(
  "[11] precomputing predictions for %d combinations x %d ages x 2 outcomes",
  nrow(combo_grid), length(target_age_mids)
))

combo_lookup <- purrr::map_dfr(seq_len(nrow(combo_grid)), function(i) {

  status_i <- as.logical(combo_grid[i, ])
  names(status_i) <- names(combo_grid)

  p_hosp <- predict_standardised(fit_hosp_full, target_age_mids, status_i)
  p_death <- predict_standardised(fit_death_full, target_age_mids, status_i)

  tibble::tibble(
    combo_id = i,
    age_mid = target_age_mids,
    p_hosp = as.numeric(p_hosp),
    p_death = as.numeric(p_death)
  )
})

## bridge_comorb_cols order must match the copula simulation's X columns
combo_key <- combo_grid |>
  dplyr::mutate(combo_id = dplyr::row_number())

## ============================================================
## 6. Re-simulate the population (same copula approach as
##    08_copula_simulation.R) and integrate the risk model over it
## ============================================================

integrate_burden_row <- function(row_i, n_sim = 10000, seed = 1) {

  marginals_i <- row_i |>
    dplyr::select(dplyr::all_of(gbd_prev_cols)) |>
    unlist() |>
    as.numeric()
  names(marginals_i) <- bridge_comorb_cols

  age_band_i <- as.character(row_i$age_band_corr)
  corr_i <- corr_by_age_bridge[[age_band_i]]
  corr_i <- corr_i[bridge_comorb_cols, bridge_comorb_cols]
  corr_i[!is.finite(corr_i)] <- 0
  diag(corr_i) <- 1
  corr_pd <- Matrix::nearPD(corr_i, corr = TRUE)$mat |> as.matrix()

  set.seed(seed)
  E <- matrix(stats::rnorm(n_sim * length(bridge_comorb_cols)),
              nrow = n_sim, ncol = length(bridge_comorb_cols))
  Z <- E %*% chol(corr_pd)
  colnames(Z) <- bridge_comorb_cols
  thresholds <- stats::qnorm(1 - marginals_i)
  X <- sweep(Z, 2, thresholds, FUN = ">")
  X <- as.data.frame(X)

  ## Map each simulated individual to one of the 128 pre-computed
  ## combinations (same column order as combo_key)
  combo_str <- do.call(paste, c(X[bridge_comorb_cols], sep = "_"))
  key_str <- do.call(paste, c(combo_key[bridge_comorb_cols], sep = "_"))
  combo_id_i <- combo_key$combo_id[match(combo_str, key_str)]

  age_mid_i <- row_i$age_start + (row_i$age_end - row_i$age_start) / 2
  if (is.infinite(row_i$age_end)) age_mid_i <- 97.5
  nearest_age_mid <- target_age_mids[which.min(abs(target_age_mids - age_mid_i))]

  cell <- combo_lookup[combo_lookup$age_mid == nearest_age_mid, ]
  p_hosp_i <- cell$p_hosp[match(combo_id_i, cell$combo_id)]
  p_death_i <- cell$p_death[match(combo_id_i, cell$combo_id)]

  tibble::tibble(
    iso3 = row_i$iso3,
    country_name = row_i$country_name,
    age_start = row_i$age_start,
    age_end = row_i$age_end,
    age_mid = nearest_age_mid,
    expected_p_hosp = mean(p_hosp_i, na.rm = TRUE),
    expected_p_death = mean(p_death_i, na.rm = TRUE)
  )
}

bg_prev_country_complete <- bg_prev_country_complete |>
  dplyr::mutate(row_id_11 = dplyr::row_number())

burden_by_country_age <- purrr::map_dfr(
  seq_len(nrow(bg_prev_country_complete)),
  function(i) integrate_burden_row(bg_prev_country_complete[i, ], seed = 2000 + i)
)

## Ratio against a hypothetical zero-comorbidity population at the same
## age: how much higher is expected burden because of this country's
## actual comorbidity profile?
burden_by_country_age <- burden_by_country_age |>
  dplyr::mutate(
    ref_p_hosp = ref_hosp_by_age[as.character(round(age_mid))],
    ref_p_death = ref_death_by_age[as.character(round(age_mid))],
    excess_ratio_hosp = expected_p_hosp / ref_p_hosp,
    excess_ratio_death = expected_p_death / ref_p_death
  )

save(burden_by_country_age, file = "01_Data/burden_by_country_age.RData")

message(sprintf(
  "[11] integrated burden for %s country x age-band rows (%d countries)",
  format(nrow(burden_by_country_age), big.mark = ","),
  dplyr::n_distinct(burden_by_country_age$iso3)
))
