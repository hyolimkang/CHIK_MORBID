# -----------------------------------------------------------------------------
# Refresh standardised risk and RR confidence intervals for joint core-3
# profiles.
#
# This lightweight refresh reproduces only the pooled joint-profile models in
# 12_core3_condition_specific_rr.R. It avoids rerunning the age-stratified
# analyses while adding 95% CIs for each profile's standardised risk to the
# two cached profile summary objects.
# -----------------------------------------------------------------------------

required_packages <- c("dplyr", "lubridate", "mgcv", "MASS", "tibble")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# ---- 1. User-editable settings ----------------------------------------------

individual_input <- file.path("01_Data", "chik_sinan_individual_2015_2025.rds")
hosp_output <- file.path("01_Data", "core_profile_rr_hosp.RData")
death_output <- file.path("01_Data", "core_profile_rr_death.RData")
bootstrap_draws <- 2000L
bootstrap_block_size <- 200L

core_cols <- c("diabetes", "hypertension", "renal_disease")
profile_levels <- c(
  "None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD"
)

if (!file.exists(individual_input)) stop("Data file not found: ", individual_input)

# ---- 2. Recreate the two outcome-specific cohorts exactly as in script 12 --

add_core3 <- function(data) {
  data |>
    filter(
      is_confirmed_chik,
      year(event_date) >= 2017,
      !is.na(age_years), age_years >= 0, age_years <= 100,
      sex %in% c("male", "female"),
      if_all(all_of(core_cols), ~ .x %in% c("no", "yes"))
    ) |>
    mutate(
      year = factor(year(event_date)),
      sex = factor(sex, levels = c("female", "male")),
      uf_residence = factor(uf_residence),
      dm = as.integer(diabetes == "yes"),
      htn = as.integer(hypertension == "yes"),
      ckd = as.integer(renal_disease == "yes"),
      core_profile = case_when(
        dm == 0 & htn == 0 & ckd == 0 ~ "None",
        dm == 1 & htn == 0 & ckd == 0 ~ "DM",
        dm == 0 & htn == 1 & ckd == 0 ~ "HTN",
        dm == 0 & htn == 0 & ckd == 1 ~ "CKD",
        dm == 1 & htn == 1 & ckd == 0 ~ "DM+HTN",
        dm == 1 & htn == 0 & ckd == 1 ~ "DM+CKD",
        dm == 0 & htn == 1 & ckd == 1 ~ "HTN+CKD",
        dm == 1 & htn == 1 & ckd == 1 ~ "DM+HTN+CKD"
      ),
      core_profile = factor(core_profile, levels = profile_levels)
    )
}

individual_data <- readRDS(individual_input)
hosp_cohort <- individual_data |>
  add_core3() |>
  filter(hospitalised %in% c("no", "yes")) |>
  mutate(hosp_only = hospitalised == "yes") |>
  select(age_years, sex, year, uf_residence, core_profile, hosp_only)

death_cohort <- individual_data |>
  add_core3() |>
  filter(!is.na(died_from_chik)) |>
  mutate(death_only = died_from_chik) |>
  select(age_years, sex, year, uf_residence, core_profile, death_only)

rm(individual_data)
gc()

message("Hospitalisation cohort: ", format(nrow(hosp_cohort), big.mark = ","))
message("Mortality cohort: ", format(nrow(death_cohort), big.mark = ","))

# ---- 3. Fit pooled joint-profile models -------------------------------------

fit_hosp_profile <- mgcv::gam(
  hosp_only ~ core_profile + sex + year +
    s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts"),
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "REML"
)

fit_death_profile <- mgcv::gam(
  death_only ~ core_profile + sex + year +
    s(uf_residence, bs = "re") + s(age_years, k = 10, bs = "ts"),
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "REML",
  na.action = na.exclude
)

# ---- 4. Population standardisation and paired bootstrap --------------------

weighted_risk_draws <- function(X, beta_draws, weights, draw_block_size) {
  draw_count <- nrow(beta_draws)
  output <- numeric(draw_count)
  for (first_draw in seq.int(1L, draw_count, by = draw_block_size)) {
    draw_index <- first_draw:min(first_draw + draw_block_size - 1L, draw_count)
    probability_block <- plogis(X %*% t(beta_draws[draw_index, , drop = FALSE]))
    output[draw_index] <- colSums(probability_block * weights) / sum(weights)
  }
  output
}

standardise_profile_rr <- function(fit, data, seed) {
  standardisation_cells <- data |>
    count(age_years, sex, year, uf_residence, name = "n")
  weights <- standardisation_cells$n
  beta_hat <- stats::coef(fit)

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = bootstrap_draws,
    mu = beta_hat,
    Sigma = stats::vcov(fit, unconditional = TRUE)
  )

  risk_point <- setNames(numeric(length(profile_levels)), profile_levels)
  risk_draws <- matrix(
    NA_real_, nrow = bootstrap_draws, ncol = length(profile_levels),
    dimnames = list(NULL, profile_levels)
  )

  for (profile in profile_levels) {
    prediction_data <- standardisation_cells
    prediction_data$core_profile <- factor(profile, levels = profile_levels)
    X <- stats::predict(fit, newdata = prediction_data, type = "lpmatrix")

    point_predictions <- plogis(as.numeric(X %*% beta_hat))
    risk_point[profile] <- sum(point_predictions * weights) / sum(weights)
    risk_draws[, profile] <- weighted_risk_draws(
      X, beta_draws, weights, draw_block_size = bootstrap_block_size
    )
  }

  rr_point <- risk_point / risk_point[["None"]]
  rr_draws <- sweep(risk_draws, 1, risk_draws[, "None"], FUN = "/")

  tibble(
    core_profile = factor(profile_levels, levels = profile_levels),
    standardised_risk = as.numeric(risk_point),
    standardised_risk_lower = apply(
      risk_draws, 2, stats::quantile, probs = 0.025, names = FALSE
    ),
    standardised_risk_upper = apply(
      risk_draws, 2, stats::quantile, probs = 0.975, names = FALSE
    ),
    rr = as.numeric(rr_point),
    rr_lower = apply(rr_draws, 2, stats::quantile, probs = 0.025, names = FALSE),
    rr_upper = apply(rr_draws, 2, stats::quantile, probs = 0.975, names = FALSE)
  )
}

get_crude_profile_counts <- function(data, outcome_var) {
  data |>
    group_by(core_profile) |>
    summarise(
      n_profile = n(),
      n_events_profile = sum(.data[[outcome_var]]),
      .groups = "drop"
    )
}

core_profile_rr_hosp <- standardise_profile_rr(
  fit_hosp_profile, hosp_cohort, seed = 1L
) |>
  left_join(get_crude_profile_counts(hosp_cohort, "hosp_only"), by = "core_profile")

core_profile_rr_death <- standardise_profile_rr(
  fit_death_profile, death_cohort, seed = 1L
) |>
  left_join(get_crude_profile_counts(death_cohort, "death_only"), by = "core_profile")

# ---- 5. Save enhanced cached summaries --------------------------------------

save(core_profile_rr_hosp, file = hosp_output)
save(core_profile_rr_death, file = death_output)

message("Updated profile summaries with standardised-risk and RR CIs:\n  ",
        hosp_output, "\n  ", death_output)
print(core_profile_rr_hosp, n = Inf)
print(core_profile_rr_death, n = Inf)
