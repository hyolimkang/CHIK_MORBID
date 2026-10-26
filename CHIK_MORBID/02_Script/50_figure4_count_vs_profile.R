# ---------------------------------------------------------------------------
# 50_figure4_count_vs_profile.R
#
# Figure 4: do specific DM/HTN/CKD profiles add information beyond
# comorbidity count?
#
# Three representations, each with the manuscript's adjustment set
# (12_core3_condition_specific_rr.R section 8): sex + year + state random
# effect + penalised age smooth, no profile-by-age interaction.
#   Model A: any of DM/HTN/CKD (2 levels)
#   Model B: number of conditions (0/1/2/3)
#   Model C: eight mutually exclusive profiles
# A is nested in B, and B in C (each is a linear constraint on the
# exposure coefficients of the next), so C-vs-B tests whether WHICH
# conditions matter once HOW MANY is known.
#
# For hospitalisation (N = 776,072) and chikungunya-attributed death
# (N = 983,947), separately:
#   - full-data fits: AIC, edf, approximate LRT, Wald tests of within-count
#     homogeneity (penalisation-aware, using Vp)
#   - 5-fold cross-validation, folds stratified by outcome x state x year and
#     shared by all three models: log loss, Brier, AUROC, AUPRC, calibration
#     intercept/slope; pooled out-of-fold paired differences (per-person
#     delta method for log loss/Brier, stratified bootstrap for AUROC/AUPRC)
#   - standardised risks by g-computation over the outcome cohort's observed
#     age x sex x year x state distribution (2,000 coefficient draws), for any
#     (A), count (B) and profile (C); within-count contrasts from model C draws
#   - sensitivity: CV repeated excluding all CKD-containing records
# The figure is drawn by 51_figure4_count_vs_profile_plot.R.
#
# Models are fitted with mgcv::bam(fREML, discrete) - the scalable form of
# the same penalised GAM as script 12's gam(REML) section 8 fit, needed for
# 36 refits. Full-data model C estimates are checked against script 12's
# cached gam() estimates.
#
# Output: 03_Output/figure_revision/ (main CSVs) and figure4/ (detail CSVs);
#         figure drawn by script 51.
# Runtime: ~20-30 min.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(MASS)
  library(mgcv)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

source("02_Script/figure_revision_theme.R")

out_dir <- file.path(fig_rev_dir, "figure4")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

options(warn = 1)  # print warnings as they occur, attributed to the step
n_threads <- max(1, min(8, parallel::detectCores() - 2))
n_folds <- 5
B_draws <- 2000
B_boot <- 200
fold_seed <- 50

log_msg <- function(...) message(sprintf("[50 %s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...))

## ============================================================
## 1. Cohorts (identical rules to script 12)
## ============================================================
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")

base_cohort <- ind |>
  dplyr::filter(
    is_confirmed_chik,
    lubridate::year(event_date) >= 2017,
    !is.na(age_years), age_years >= 0, age_years <= 100,
    sex %in% c("male", "female"),
    dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                  ~ .x %in% c("no", "yes"))
  ) |>
  dplyr::mutate(
    year = factor(lubridate::year(event_date)),
    sex = factor(sex, levels = c("female", "male")),
    uf_residence = factor(uf_residence),
    dm = as.integer(diabetes == "yes"),
    htn = as.integer(hypertension == "yes"),
    ckd = as.integer(renal_disease == "yes"),
    core_profile = factor(dplyr::case_when(
      dm == 0 & htn == 0 & ckd == 0 ~ "None",
      dm == 1 & htn == 0 & ckd == 0 ~ "DM",
      dm == 0 & htn == 1 & ckd == 0 ~ "HTN",
      dm == 0 & htn == 0 & ckd == 1 ~ "CKD",
      dm == 1 & htn == 1 & ckd == 0 ~ "DM+HTN",
      dm == 1 & htn == 0 & ckd == 1 ~ "DM+CKD",
      dm == 0 & htn == 1 & ckd == 1 ~ "HTN+CKD",
      TRUE ~ "DM+HTN+CKD"
    ), levels = core_profile_levels),
    n_conditions = factor(dm + htn + ckd, levels = 0:3),
    any_condition = factor(ifelse(dm + htn + ckd > 0, "yes", "no"), levels = c("no", "yes"))
  )

cohorts <- list(
  hosp = base_cohort |>
    dplyr::filter(hospitalised %in% c("no", "yes")) |>
    dplyr::mutate(y = as.integer(hospitalised == "yes")),
  death = base_cohort |>
    dplyr::filter(!is.na(died_from_chik)) |>
    dplyr::mutate(y = as.integer(died_from_chik))
)
cohorts <- purrr::map(cohorts, ~ dplyr::select(.x, y, age_years, sex, year, uf_residence,
                                               core_profile, n_conditions, any_condition))
rm(ind, base_cohort)
invisible(gc())

stopifnot(nrow(cohorts$hosp) == 776072, sum(cohorts$hosp$y) == 25363,
          nrow(cohorts$death) == 983947, sum(cohorts$death$y) == 1050)

outcome_labels <- c(hosp = "Hospitalisation", death = "Chikungunya-attributed death")

## ============================================================
## 2. Models
## ============================================================
adjust <- "+ sex + year + s(uf_residence, bs = 're') + s(age_years, k = 10, bs = 'ts')"
model_formulas <- list(
  A = stats::as.formula(paste("y ~ any_condition", adjust)),
  B = stats::as.formula(paste("y ~ n_conditions", adjust)),
  C = stats::as.formula(paste("y ~ core_profile", adjust))
)
model_labels <- c(A = "A: any condition", B = "B: condition count", C = "C: specific profile")

fit_model <- function(formula, data) {
  mgcv::bam(formula, data = data, family = binomial(link = "logit"),
            method = "fREML", discrete = TRUE, nthreads = n_threads)
}

## ------------------------------------------------------------
## 2a. Full-data fits, information criteria, nested tests
## ------------------------------------------------------------
full_fits <- list()
for (o in names(cohorts)) for (m in names(model_formulas)) {
  log_msg("full fit %s / %s", o, m)
  full_fits[[o]][[m]] <- fit_model(model_formulas[[m]], cohorts[[o]])
}

## Wald test of linear constraints on the exposure coefficients of model C
## (and B), using the Bayesian covariance Vp.
wald_constraint <- function(fit, L) {
  b <- coef(fit)
  d <- as.numeric(L %*% b)
  V <- L %*% fit$Vp %*% t(L)
  stat <- as.numeric(t(d) %*% solve(V, d))
  c(chisq = stat, df = nrow(L), p = stats::pchisq(stat, nrow(L), lower.tail = FALSE))
}
coef_row <- function(fit, name) {
  r <- numeric(length(coef(fit)))
  r[match(name, names(coef(fit)))] <- 1
  r
}
contrast_rows <- function(fit, prefix, a, b_list) {
  ## rows: coef(a) - coef(b) for each b in b_list (reference level has coef 0)
  t(sapply(b_list, function(b) {
    ra <- if (is.na(a)) 0 else coef_row(fit, paste0(prefix, a))
    rb <- if (is.na(b)) 0 else coef_row(fit, paste0(prefix, b))
    ra - rb
  }))
}

nested_tests <- purrr::map_dfr(names(cohorts), function(o) {
  fC <- full_fits[[o]]$C
  fB <- full_fits[[o]]$B
  ## One-condition profiles equal: DM = HTN = CKD
  L1 <- contrast_rows(fC, "core_profile", "DM", c("HTN", "CKD"))
  ## Two-condition profiles equal: DM+HTN = DM+CKD = HTN+CKD
  L2 <- contrast_rows(fC, "core_profile", "DM+HTN", c("DM+CKD", "HTN+CKD"))
  ## Count model: 1 = 2 = 3 (i.e. any-condition is enough)
  LB <- contrast_rows(fB, "n_conditions", "1", c("2", "3"))
  w1 <- wald_constraint(fC, L1); w2 <- wald_constraint(fC, L2)
  wj <- wald_constraint(fC, rbind(L1, L2)); wb <- wald_constraint(fB, LB)
  aov_BA <- tryCatch(mgcv::anova.gam(full_fits[[o]]$A, fB, test = "Chisq"), error = function(e) NULL)
  aov_CB <- tryCatch(mgcv::anova.gam(fB, fC, test = "Chisq"), error = function(e) NULL)
  tibble::tibble(
    outcome = o,
    test = c("B vs A: count levels 1 = 2 = 3 (Wald, Vp)",
             "C vs B: one-condition profiles equal, DM = HTN = CKD (Wald, Vp)",
             "C vs B: two-condition profiles equal, DM+HTN = DM+CKD = HTN+CKD (Wald, Vp)",
             "C vs B: joint within-count homogeneity (Wald, Vp)",
             "B vs A: approximate LRT (anova.gam)",
             "C vs B: approximate LRT (anova.gam)"),
    statistic = c(wb[["chisq"]], w1[["chisq"]], w2[["chisq"]], wj[["chisq"]],
                  if (is.null(aov_BA)) NA else aov_BA$Deviance[2],
                  if (is.null(aov_CB)) NA else aov_CB$Deviance[2]),
    df = c(wb[["df"]], w1[["df"]], w2[["df"]], wj[["df"]],
           if (is.null(aov_BA)) NA else aov_BA$Df[2],
           if (is.null(aov_CB)) NA else aov_CB$Df[2]),
    p_value = c(wb[["p"]], w1[["p"]], w2[["p"]], wj[["p"]],
                if (is.null(aov_BA)) NA else aov_BA$`Pr(>Chi)`[2],
                if (is.null(aov_CB)) NA else aov_CB$`Pr(>Chi)`[2])
  )
})

info_criteria <- purrr::map_dfr(names(cohorts), function(o) {
  purrr::map_dfr(names(model_formulas), function(m) {
    f <- full_fits[[o]][[m]]
    tibble::tibble(outcome = o, model = m, aic = stats::AIC(f), total_edf = sum(f$edf),
                   exposure_df = length(grep("^(any_condition|n_conditions|core_profile)",
                                             names(coef(f)))),
                   deviance = stats::deviance(f),
                   deviance_explained_pct = 100 * summary(f)$dev.expl,
                   converged = if (is.null(f$converged)) NA else f$converged)
  }) |>
    dplyr::mutate(delta_aic_vs_previous = aic - dplyr::lag(aic))
})
print(info_criteria)
print(nested_tests)

## ============================================================
## 3. Cross-validation
## ============================================================
## Folds: within every outcome x state x year stratum, records are randomly
## dealt across the 5 folds, so each fold has the same event rate and the
## same state/year mix. One fold vector per cohort, shared by A, B and C.
make_folds <- function(d, seed) {
  set.seed(seed)
  strata <- interaction(d$y, d$uf_residence, d$year, drop = TRUE)
  fold <- integer(nrow(d))
  for (idx in split(seq_len(nrow(d)), strata)) {
    ## sample.int, not sample(): sample(k) on a length-1 vector permutes 1:k.
    fold[idx] <- rep_len(sample.int(n_folds), length(idx))[sample.int(length(idx))]
  }
  fold
}

metric_set <- function(y, p) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  ## as.numeric: n1 * n0 exceeds the integer range (~2e10 for hospitalisation)
  n1 <- as.numeric(sum(y)); n0 <- as.numeric(length(y)) - n1
  r <- rank(p)
  auroc <- (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  o <- order(p, decreasing = TRUE)
  tp <- cumsum(y[o])
  auprc <- sum((tp / seq_along(o))[y[o] == 1]) / n1
  lp <- stats::qlogis(p)
  slope <- unname(coef(stats::glm.fit(cbind(1, lp), y, family = binomial()))[2])
  citl <- unname(coef(stats::glm(y ~ 1, offset = lp, family = binomial()))[1])
  c(log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    brier = mean((y - p)^2), auroc = auroc, auprc = auprc,
    calibration_intercept = citl, calibration_slope = slope,
    observed_rate = mean(y), mean_predicted = mean(p))
}

run_cv <- function(d, folds, label) {
  oof <- matrix(NA_real_, nrow(d), length(model_formulas),
                dimnames = list(NULL, names(model_formulas)))
  fold_metrics <- list()
  for (k in seq_len(n_folds)) {
    tr <- folds != k; te <- folds == k
    for (m in names(model_formulas)) {
      ## Model C on a subset may lack profile levels (sensitivity analysis):
      ## drop unused levels in training and test consistently.
      dtr <- droplevels(d[tr, ])
      f <- fit_model(model_formulas[[m]], dtr)
      dte <- d[te, ]
      dte$core_profile <- factor(as.character(dte$core_profile), levels = levels(dtr$core_profile))
      dte$n_conditions <- factor(as.character(dte$n_conditions), levels = levels(dtr$n_conditions))
      dte$any_condition <- factor(as.character(dte$any_condition), levels = levels(dtr$any_condition))
      oof[te, m] <- as.numeric(stats::predict(f, newdata = dte, type = "response"))
      fold_metrics[[length(fold_metrics) + 1]] <- tibble::as_tibble_row(
        c(list(analysis = label, fold = k, model = m, n = sum(te), events = sum(d$y[te])),
          as.list(metric_set(d$y[te], oof[te, m]))))
    }
    log_msg("  %s fold %d done", label, k)
  }
  list(oof = oof, fold_metrics = dplyr::bind_rows(fold_metrics))
}

## Paired differences on pooled out-of-fold predictions. Improvement is
## defined so that POSITIVE = the more detailed model is better.
paired_differences <- function(y, oof, comparisons, label, seed) {
  p <- pmin(pmax(oof, 1e-12), 1 - 1e-12)
  ll <- -(y * log(p) + (1 - y) * log(1 - p))
  br <- (y - p)^2
  set.seed(seed)
  i1 <- which(y == 1); i0 <- which(y == 0)
  boot_idx <- replicate(B_boot, c(sample(i1, replace = TRUE), sample(i0, replace = TRUE)),
                        simplify = FALSE)
  auc_fun <- function(yy, pp) {
    n1 <- as.numeric(sum(yy)); n0 <- as.numeric(length(yy)) - n1
    (sum(rank(pp)[yy == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  ap_fun <- function(yy, pp) {
    o <- order(pp, decreasing = TRUE); tp <- cumsum(yy[o])
    sum((tp / seq_along(o))[yy[o] == 1]) / sum(yy)
  }
  purrr::map_dfr(comparisons, function(cmp) {
    new <- cmp[1]; ref <- cmp[2]
    d_ll <- ll[, ref] - ll[, new]
    d_br <- br[, ref] - br[, new]
    se <- function(x) stats::sd(x) / sqrt(length(x))
    auc_d <- sapply(boot_idx, function(ix) auc_fun(y[ix], p[ix, new]) - auc_fun(y[ix], p[ix, ref]))
    ap_d <- sapply(boot_idx, function(ix) ap_fun(y[ix], p[ix, new]) - ap_fun(y[ix], p[ix, ref]))
    auc_pt <- auc_fun(y, p[, new]) - auc_fun(y, p[, ref])
    ap_pt <- ap_fun(y, p[, new]) - ap_fun(y, p[, ref])
    mll <- mean(ll[, ref]); mbr <- mean(br[, ref])
    tibble::tibble(
      analysis = label,
      comparison = paste0(new, " vs ", ref),
      metric = c("log_loss", "brier", "log_loss_relative_pct", "brier_relative_pct",
                 "auroc", "auprc"),
      improvement = c(mean(d_ll), mean(d_br), 100 * mean(d_ll) / mll, 100 * mean(d_br) / mbr,
                      auc_pt, ap_pt),
      lower = c(mean(d_ll) - 1.96 * se(d_ll), mean(d_br) - 1.96 * se(d_br),
                100 * (mean(d_ll) - 1.96 * se(d_ll)) / mll, 100 * (mean(d_br) - 1.96 * se(d_br)) / mbr,
                stats::quantile(auc_d, 0.025, names = FALSE), stats::quantile(ap_d, 0.025, names = FALSE)),
      upper = c(mean(d_ll) + 1.96 * se(d_ll), mean(d_br) + 1.96 * se(d_br),
                100 * (mean(d_ll) + 1.96 * se(d_ll)) / mll, 100 * (mean(d_br) + 1.96 * se(d_br)) / mbr,
                stats::quantile(auc_d, 0.975, names = FALSE), stats::quantile(ap_d, 0.975, names = FALSE)),
      uncertainty_method = c(rep("per-person paired delta (normal)", 4),
                             rep("stratified bootstrap, 200 reps", 2))
    )
  })
}

comparisons <- list(c("B", "A"), c("C", "B"), c("C", "A"))

cv_results <- list()
pooled_metrics <- list()
fold_metrics <- list()
paired <- list()
contribution <- list()

for (o in names(cohorts)) {
  d <- cohorts[[o]]
  folds <- make_folds(d, fold_seed)
  for (analysis in c("primary", "excluding_ckd")) {
    keep <- if (analysis == "primary") rep(TRUE, nrow(d)) else
      !grepl("CKD", as.character(d$core_profile), fixed = TRUE)
    label <- paste(o, analysis, sep = ":")
    log_msg("CV %s (n = %d)", label, sum(keep))
    cv <- run_cv(d[keep, ], folds[keep], label)
    cv_results[[label]] <- cv
    fold_metrics[[label]] <- cv$fold_metrics
    pooled_metrics[[label]] <- purrr::map_dfr(names(model_formulas), function(m) {
      tibble::as_tibble_row(c(list(analysis = label, model = m),
                              as.list(metric_set(d$y[keep], cv$oof[, m]))))
    })
    paired[[label]] <- paired_differences(d$y[keep], cv$oof, comparisons, label,
                                          seed = fold_seed + nchar(label))
    if (analysis == "primary") {
      ## Where does the C-vs-B log-loss improvement come from?
      p <- pmin(pmax(cv$oof, 1e-12), 1 - 1e-12)
      yy <- d$y
      ll <- -(yy * log(p) + (1 - yy) * log(1 - p))
      contribution[[o]] <- tibble::tibble(core_profile = d$core_profile,
                                          d_ll = ll[, "B"] - ll[, "C"]) |>
        dplyr::group_by(core_profile) |>
        dplyr::summarise(n = dplyr::n(), total_improvement = sum(d_ll), .groups = "drop") |>
        dplyr::mutate(outcome = o,
                      share_of_total_pct = 100 * total_improvement / sum(total_improvement),
                      share_of_cohort_pct = 100 * n / sum(n), .before = 1)
    }
  }
}

pooled_metrics <- dplyr::bind_rows(pooled_metrics)
fold_metrics <- dplyr::bind_rows(fold_metrics)
paired <- dplyr::bind_rows(paired)
contribution <- dplyr::bind_rows(contribution)

## Fold-level consistency of the paired log-loss / Brier differences.
fold_consistency <- fold_metrics |>
  dplyr::select(analysis, fold, model, log_loss, brier) |>
  tidyr::pivot_wider(names_from = model, values_from = c(log_loss, brier)) |>
  dplyr::mutate(ll_B_vs_A = log_loss_A - log_loss_B, ll_C_vs_B = log_loss_B - log_loss_C,
                br_B_vs_A = brier_A - brier_B, br_C_vs_B = brier_B - brier_C) |>
  dplyr::group_by(analysis) |>
  dplyr::summarise(dplyr::across(c(ll_B_vs_A, ll_C_vs_B, br_B_vs_A, br_C_vs_B),
                                 list(folds_improved = ~ sum(.x > 0), mean = mean, min = min, max = max)),
                   .groups = "drop")

model_comparison_metrics <- dplyr::bind_rows(
  pooled_metrics |>
    tidyr::pivot_longer(-c(analysis, model), names_to = "metric", values_to = "value") |>
    dplyr::mutate(type = "pooled out-of-fold metric", comparison = model) |>
    dplyr::select(type, analysis, comparison, metric, value),
  paired |>
    dplyr::transmute(type = "paired improvement (positive = first model better)",
                     analysis, comparison, metric, value = improvement, lower, upper,
                     uncertainty_method)
)

readr::write_csv(model_comparison_metrics, file.path(fig_rev_dir, "model_comparison_metrics.csv"), na = "")
readr::write_csv(fold_metrics, file.path(fig_rev_dir, "model_comparison_fold_metrics.csv"))
readr::write_csv(fold_consistency, file.path(out_dir, "fold_consistency.csv"))
readr::write_csv(contribution, file.path(out_dir, "profile_contribution_to_C_vs_B_logloss.csv"))
readr::write_csv(info_criteria, file.path(out_dir, "information_criteria.csv"))
readr::write_csv(nested_tests, file.path(out_dir, "nested_model_tests.csv"))
print(as.data.frame(paired), digits = 3)

## ============================================================
## 4. Standardised risks (g-computation) and within-count contrasts
## ============================================================
weighted_risk_draws <- function(X, beta_draws, w, block = 250) {
  out <- numeric(nrow(beta_draws))
  for (i in seq.int(1, nrow(beta_draws), by = block)) {
    idx <- i:min(i + block - 1, nrow(beta_draws))
    out[idx] <- colSums(stats::plogis(X %*% t(beta_draws[idx, , drop = FALSE])) * w) / sum(w)
  }
  out
}

standardise_levels <- function(fit, d, var, levels, seed) {
  combo <- dplyr::count(d, age_years, sex, year, uf_residence, name = "n")
  beta_hat <- coef(fit)
  set.seed(seed)
  bd <- MASS::mvrnorm(B_draws, beta_hat, stats::vcov(fit, unconditional = TRUE))
  pt <- setNames(numeric(length(levels)), levels)
  dr <- matrix(NA_real_, B_draws, length(levels), dimnames = list(NULL, levels))
  for (l in levels) {
    nd <- combo
    nd[[var]] <- factor(l, levels = levels(d[[var]]))
    X <- stats::predict(fit, newdata = nd, type = "lpmatrix")
    pt[l] <- sum(stats::plogis(as.numeric(X %*% beta_hat)) * combo$n) / sum(combo$n)
    dr[, l] <- weighted_risk_draws(X, bd, combo$n)
  }
  list(point = pt, draws = dr)
}

q <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE))

std_results <- list()
contrasts <- list()
for (o in names(cohorts)) {
  d <- cohorts[[o]]
  log_msg("standardising %s", o)
  sA <- standardise_levels(full_fits[[o]]$A, d, "any_condition", c("no", "yes"), seed = 401)
  sB <- standardise_levels(full_fits[[o]]$B, d, "n_conditions", as.character(0:3), seed = 402)
  sC <- standardise_levels(full_fits[[o]]$C, d, "core_profile", core_profile_levels, seed = 403)

  summarise_std <- function(s, model, ref) {
    tibble::tibble(
      outcome = o, model = model, level = names(s$point),
      std_risk_pct = 100 * s$point,
      std_risk_lower_pct = 100 * apply(s$draws, 2, q, 0.025),
      std_risk_upper_pct = 100 * apply(s$draws, 2, q, 0.975),
      rr = s$point / s$point[[ref]],
      rr_lower = apply(s$draws / s$draws[, ref], 2, q, 0.025),
      rr_upper = apply(s$draws / s$draws[, ref], 2, q, 0.975)
    )
  }
  counts_C <- d |> dplyr::count(core_profile, name = "n_cases") |>
    dplyr::left_join(d |> dplyr::group_by(core_profile) |>
                       dplyr::summarise(n_events = sum(y), .groups = "drop"), by = "core_profile")
  counts_B <- d |> dplyr::group_by(n_conditions) |>
    dplyr::summarise(n_cases = dplyr::n(), n_events = sum(y), .groups = "drop")
  counts_A <- d |> dplyr::group_by(any_condition) |>
    dplyr::summarise(n_cases = dplyr::n(), n_events = sum(y), .groups = "drop")

  std_results[[o]] <- dplyr::bind_rows(
    summarise_std(sA, "A", "no") |>
      dplyr::left_join(dplyr::rename(counts_A, level = any_condition) |>
                         dplyr::mutate(level = as.character(level)), by = "level") |>
      dplyr::mutate(n_conditions = ifelse(level == "no", "0", "1-3")),
    summarise_std(sB, "B", "0") |>
      dplyr::left_join(dplyr::rename(counts_B, level = n_conditions) |>
                         dplyr::mutate(level = as.character(level)), by = "level") |>
      dplyr::mutate(n_conditions = level),
    summarise_std(sC, "C", "None") |>
      dplyr::left_join(dplyr::rename(counts_C, level = core_profile) |>
                         dplyr::mutate(level = as.character(level)), by = "level") |>
      dplyr::mutate(n_conditions = as.character(profile_count[level]))
  )

  ## Within-count contrasts from model C (same draw): pairwise RRs among the
  ## one- and two-condition profiles, and each profile vs the case-mix-weighted
  ## average of its own count group (observed profile frequencies as weights).
  pair_rr <- function(a, b) {
    r <- sC$draws[, a] / sC$draws[, b]
    tibble::tibble(outcome = o, contrast_type = "pairwise within count",
                   n_conditions = as.character(profile_count[a]),
                   contrast = paste(a, "vs", b), rr = sC$point[[a]] / sC$point[[b]],
                   rr_lower = q(r, 0.025), rr_upper = q(r, 0.975),
                   events_a = counts_C$n_events[counts_C$core_profile == a],
                   events_b = counts_C$n_events[counts_C$core_profile == b])
  }
  pw <- dplyr::bind_rows(
    pair_rr("DM", "HTN"), pair_rr("CKD", "HTN"), pair_rr("CKD", "DM"),
    pair_rr("DM+CKD", "DM+HTN"), pair_rr("HTN+CKD", "DM+HTN"), pair_rr("HTN+CKD", "DM+CKD")
  )
  vs_avg <- purrr::map_dfr(c("1", "2"), function(k) {
    profs <- names(profile_count)[profile_count == as.numeric(k)]
    wts <- counts_C$n_cases[match(profs, counts_C$core_profile)]
    wts <- wts / sum(wts)
    avg_d <- as.numeric(sC$draws[, profs] %*% wts)
    avg_p <- sum(sC$point[profs] * wts)
    purrr::map_dfr(profs, function(a) {
      r <- sC$draws[, a] / avg_d
      tibble::tibble(outcome = o, contrast_type = "profile vs its count-group average (model C)",
                     n_conditions = k, contrast = paste(a, "vs average of", k, "-condition profiles"),
                     rr = sC$point[[a]] / avg_p, rr_lower = q(r, 0.025), rr_upper = q(r, 0.975),
                     events_a = counts_C$n_events[counts_C$core_profile == a],
                     events_b = sum(counts_C$n_events[match(profs, counts_C$core_profile)]))
    })
  })
  ## Three-condition profile is its own count group (no contrast possible).
  contrasts[[o]] <- dplyr::bind_rows(pw, vs_avg)
}

std_results <- dplyr::bind_rows(std_results)
contrasts <- dplyr::bind_rows(contrasts)
readr::write_csv(std_results, file.path(fig_rev_dir, "model_standardised_risks.csv"))
readr::write_csv(contrasts, file.path(fig_rev_dir, "within_count_profile_contrasts.csv"))

## Agreement of bam model C with script 12's cached gam() estimates.
load("01_Data/core_profile_rr_hosp.RData"); load("01_Data/core_profile_rr_death.RData")
cache_check <- dplyr::bind_rows(
  dplyr::mutate(core_profile_rr_hosp, outcome = "hosp"),
  dplyr::mutate(core_profile_rr_death, outcome = "death")
) |>
  dplyr::transmute(outcome, level = as.character(core_profile), cached_rr = rr,
                   cached_risk_pct = 100 * standardised_risk) |>
  dplyr::inner_join(dplyr::filter(std_results, model == "C"), by = c("outcome", "level")) |>
  dplyr::group_by(outcome) |>
  dplyr::summarise(max_abs_rel_diff_rr = max(abs(rr / cached_rr - 1)),
                   max_abs_rel_diff_risk = max(abs(std_risk_pct / cached_risk_pct - 1)))
print(cache_check)
readr::write_csv(cache_check, file.path(out_dir, "model_C_vs_script12_cache_check.csv"))

saveRDS(list(cv_oof = purrr::map(cv_results, "oof"), sessionInfo = utils::sessionInfo()),
        file.path(out_dir, "cv_out_of_fold_predictions.rds"))

## Figure 4 itself is drawn by 51_figure4_count_vs_profile_plot.R from the
## CSVs written above, so figure tweaks never require rerunning the models.

log_msg("done")
