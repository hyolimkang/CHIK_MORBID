# -----------------------------------------------------------------------------
# Refresh standardised risk and risk-difference CIs for the core-3 analyses
#
# This focused script mirrors sections 1-6 of
# 12_core3_condition_specific_rr.R, but stops after refreshing the pooled
# core-3 summaries. It adds 95% CIs for standardised risks and risk differences when a full
# rerun of the later profile/figure sections is unnecessary.
#
# It overwrites only:
#   01_Data/core3_rr_hosp.RData
#   01_Data/core3_rr_death.RData
#   03_Output/tables/core3_rr_tables_with_risk_cis.xlsx
#
# The standardised risks and RRs use the same mutually adjusted models and
# the same 2,000-draw parametric bootstrap. Bootstrap predictions are handled
# in blocks to keep peak memory practical.
# -----------------------------------------------------------------------------

required_packages <- c("dplyr", "lubridate", "mgcv", "MASS", "tibble", "openxlsx")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    ". Run 02_Script/00_setup.R or install the missing packages first."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(mgcv)
  library(MASS)
  library(tibble)
  library(openxlsx)
})

# ---- 1. Inputs and analysis settings ----------------------------------------

individual_input <- file.path("01_Data", "chik_sinan_individual_2015_2025.rds")
hosp_output <- file.path("01_Data", "core3_rr_hosp.RData")
death_output <- file.path("01_Data", "core3_rr_death.RData")
table_dir <- file.path("03_Output", "tables")
workbook_output <- file.path(table_dir, "core3_rr_tables_with_risk_cis.xlsx")

bootstrap_draws <- 2000L
bootstrap_seed <- 1L
draw_block_size <- 100L
core_columns <- c("diabetes", "hypertension", "renal_disease")
exposure_variables <- c("dm", "htn", "ckd")

if (!file.exists(individual_input)) {
  stop("Missing cleaned individual data: ", individual_input)
}

# ---- 2. Build the same independent core-3 cohorts as script 12 --------------

ind <- readRDS(individual_input)

add_core3 <- function(data) {
  data |>
    dplyr::filter(
      is_confirmed_chik,
      lubridate::year(event_date) >= 2017,
      !is.na(age_years),
      age_years >= 0,
      age_years <= 100,
      sex %in% c("male", "female"),
      dplyr::if_all(dplyr::all_of(core_columns), ~ .x %in% c("no", "yes"))
    ) |>
    dplyr::mutate(
      year = factor(lubridate::year(event_date)),
      sex = factor(sex, levels = c("female", "male")),
      uf_residence = factor(uf_residence),
      dm = as.integer(diabetes == "yes"),
      htn = as.integer(hypertension == "yes"),
      ckd = as.integer(renal_disease == "yes")
    )
}

hosp_cohort <- ind |>
  add_core3() |>
  dplyr::filter(hospitalised %in% c("no", "yes")) |>
  dplyr::transmute(
    age_years, sex, year, uf_residence, dm, htn, ckd,
    hosp_only = hospitalised == "yes"
  )

death_cohort <- ind |>
  add_core3() |>
  dplyr::filter(!is.na(died_from_chik)) |>
  dplyr::transmute(
    age_years, sex, year, uf_residence, dm, htn, ckd,
    death_only = died_from_chik
  )

rm(ind)
gc()

message(sprintf(
  "[19] hospitalisation cohort: %s records; %s events",
  format(nrow(hosp_cohort), big.mark = ","),
  format(sum(hosp_cohort$hosp_only), big.mark = ",")
))
message(sprintf(
  "[19] mortality cohort: %s records; %s events",
  format(nrow(death_cohort), big.mark = ","),
  format(sum(death_cohort$death_only), big.mark = ",")
))

# ---- 3. Refit the mutually adjusted models ----------------------------------

fit_hosp_core3 <- mgcv::bam(
  hosp_only ~
    s(age_years, k = 10, bs = "ts") + sex + year +
    s(uf_residence, bs = "re") + dm + htn + ckd,
  data = hosp_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE
)

fit_death_core3 <- mgcv::bam(
  death_only ~
    s(age_years, k = 10, bs = "ts") + sex + year +
    s(uf_residence, bs = "re") + dm + htn + ckd,
  data = death_cohort,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  na.action = na.exclude
)

# ---- 4. Standardised risks, RR, and all 95% CIs -----------------------------

get_adjusted_or <- function(fit, exposure_var) {
  estimate <- stats::coef(fit)[[exposure_var]]
  se <- sqrt(diag(stats::vcov(fit, unconditional = TRUE)))[[exposure_var]]
  tibble::tibble(
    condition = exposure_var,
    or = exp(estimate),
    or_lower = exp(estimate - 1.96 * se),
    or_upper = exp(estimate + 1.96 * se)
  )
}

standardised_rr_with_ci <- function(
  fit, data, exposure_var,
  B = bootstrap_draws, seed = bootstrap_seed,
  block_size = draw_block_size
) {
  combo <- data |>
    dplyr::count(age_years, sex, year, uf_residence, dm, htn, ckd, name = "n")

  combo_exposed <- combo
  combo_unexposed <- combo
  combo_exposed[[exposure_var]] <- 1L
  combo_unexposed[[exposure_var]] <- 0L

  X1 <- stats::predict(fit, newdata = combo_exposed, type = "lpmatrix")
  X0 <- stats::predict(fit, newdata = combo_unexposed, type = "lpmatrix")
  weights <- combo$n
  weight_sum <- sum(weights)
  beta_hat <- stats::coef(fit)

  risk_exposed <- sum(stats::plogis(as.numeric(X1 %*% beta_hat)) * weights) / weight_sum
  risk_unexposed <- sum(stats::plogis(as.numeric(X0 %*% beta_hat)) * weights) / weight_sum

  set.seed(seed)
  beta_draws <- MASS::mvrnorm(
    n = B, mu = beta_hat, Sigma = stats::vcov(fit, unconditional = TRUE)
  )
  risk_exposed_draws <- numeric(B)
  risk_unexposed_draws <- numeric(B)

  for (first_draw in seq.int(1, B, by = block_size)) {
    draw_index <- first_draw:min(first_draw + block_size - 1L, B)
    beta_block <- t(beta_draws[draw_index, , drop = FALSE])

    exposed_block <- stats::plogis(X1 %*% beta_block)
    risk_exposed_draws[draw_index] <- as.numeric(crossprod(weights, exposed_block)) / weight_sum
    rm(exposed_block)

    unexposed_block <- stats::plogis(X0 %*% beta_block)
    risk_unexposed_draws[draw_index] <- as.numeric(crossprod(weights, unexposed_block)) / weight_sum
    rm(unexposed_block)
  }

  rr_draws <- risk_exposed_draws / risk_unexposed_draws
  risk_difference_draws <- risk_exposed_draws - risk_unexposed_draws
  tibble::tibble(
    condition = exposure_var,
    rr = risk_exposed / risk_unexposed,
    rr_lower = stats::quantile(rr_draws, 0.025, names = FALSE),
    rr_upper = stats::quantile(rr_draws, 0.975, names = FALSE),
    standardised_risk_exposed = risk_exposed,
    standardised_risk_exposed_lower = stats::quantile(
      risk_exposed_draws, 0.025, names = FALSE
    ),
    standardised_risk_exposed_upper = stats::quantile(
      risk_exposed_draws, 0.975, names = FALSE
    ),
    standardised_risk_unexposed = risk_unexposed,
    standardised_risk_unexposed_lower = stats::quantile(
      risk_unexposed_draws, 0.025, names = FALSE
    ),
    standardised_risk_unexposed_upper = stats::quantile(
      risk_unexposed_draws, 0.975, names = FALSE
    ),
    standardised_risk_difference = risk_exposed - risk_unexposed,
    standardised_risk_difference_lower = stats::quantile(
      risk_difference_draws, 0.025, names = FALSE
    ),
    standardised_risk_difference_upper = stats::quantile(
      risk_difference_draws, 0.975, names = FALSE
    )
  )
}

get_crude_counts <- function(data, exposure_var, outcome_var) {
  exposed <- data[[exposure_var]] == 1L
  outcome <- data[[outcome_var]]
  tibble::tibble(
    condition = exposure_var,
    n_exposed = sum(exposed),
    n_events_exposed = sum(outcome[exposed]),
    n_unexposed = sum(!exposed),
    n_events_unexposed = sum(outcome[!exposed])
  )
}

build_core3_summary <- function(fit, data, outcome_var) {
  dplyr::bind_rows(lapply(exposure_variables, function(exposure_var) {
    get_adjusted_or(fit, exposure_var) |>
      dplyr::left_join(
        standardised_rr_with_ci(fit, data, exposure_var), by = "condition"
      ) |>
      dplyr::left_join(
        get_crude_counts(data, exposure_var, outcome_var), by = "condition"
      )
  })) |>
    dplyr::mutate(
      condition = dplyr::recode(
        condition,
        dm = "Diabetes",
        htn = "Hypertension",
        ckd = "Chronic kidney disease"
      ),
      condition = factor(condition, levels = c(
        "Diabetes", "Hypertension", "Chronic kidney disease"
      ))
    )
}

core3_rr_hosp <- build_core3_summary(fit_hosp_core3, hosp_cohort, "hosp_only")
core3_rr_death <- build_core3_summary(fit_death_core3, death_cohort, "death_only")

# ---- 5. Update the cached summaries and spreadsheet -------------------------

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
save(core3_rr_hosp, file = hosp_output)
save(core3_rr_death, file = death_output)

workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(workbook, "hosp")
openxlsx::writeDataTable(workbook, "hosp", core3_rr_hosp, withFilter = TRUE)
openxlsx::addWorksheet(workbook, "death")
openxlsx::writeDataTable(workbook, "death", core3_rr_death, withFilter = TRUE)
openxlsx::saveWorkbook(workbook, workbook_output, overwrite = TRUE)

message("Updated standardised risk CIs written to:")
message("  ", hosp_output)
message("  ", death_output)
message("  ", workbook_output)
print(core3_rr_hosp)
print(core3_rr_death)
