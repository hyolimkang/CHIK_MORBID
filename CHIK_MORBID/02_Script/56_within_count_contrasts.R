# ---------------------------------------------------------------------------
# 56_within_count_contrasts.R
#
# Within-count comorbidity-profile contrasts: among cases with the same
# number of core conditions, do specific DM/HTN/CKD combinations differ in
# standardised risk of hospitalisation and chikungunya-attributed death?
#
# Models: the manuscript's overall profile model (script 12 section 8 /
# Figure 2): outcome ~ core_profile + sex + year + s(state, re) +
# s(age, k = 10, ts), and the matching comorbidity-count model (script 50,
# model B), both refitted with mgcv::bam(fREML, discrete) and checked
# against the cached Figure 2 estimates.
#
# Standardisation: one common target per outcome - the observed joint
# distribution of exact age x sex x year x state in the whole outcome cohort
# (identical to Figure 2 and to script 50's count/profile risks).
#
# Contrasts (pre-specified, all reported): two-condition DM+CKD vs DM+HTN,
# HTN+CKD vs DM+HTN, HTN+CKD vs DM+CKD; one-condition DM vs HTN, CKD vs HTN,
# CKD vs DM. RD (percentage points) and RR from shared coefficient draws;
# Holm-adjusted Wald p-values and Bonferroni-simultaneous 95% intervals;
# a nonparametric case-bootstrap (model refitted) as a rare-profile check.
#
# Output: 03_Output/figure_revision/within_count/ (figures: script 57)
# Runtime: ~1 h (bootstrap cached in bootstrap_contrasts.rds).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(MASS)
  library(mgcv)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
})
options(warn = 1)

out_dir <- "03_Output/figure_revision/within_count"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

n_threads <- max(1, min(8, parallel::detectCores() - 2))
B_draws <- 4000      # enough draws for stable Bonferroni (alpha/6) tail quantiles
B_boot <- 200
seed <- 56

core_profile_levels <- c("None", "DM", "HTN", "CKD", "DM+HTN", "DM+CKD", "HTN+CKD", "DM+HTN+CKD")
profile_count <- c(None = 0, DM = 1, HTN = 1, CKD = 1, `DM+HTN` = 2, `DM+CKD` = 2,
                   `HTN+CKD` = 2, `DM+HTN+CKD` = 3)
contrast_defs <- tibble::tribble(
  ~group,           ~priority,   ~a,        ~b,
  "Two conditions", "primary",   "DM+CKD",  "DM+HTN",
  "Two conditions", "primary",   "HTN+CKD", "DM+HTN",
  "Two conditions", "primary",   "HTN+CKD", "DM+CKD",
  "One condition",  "secondary", "DM",      "HTN",
  "One condition",  "secondary", "CKD",     "HTN",
  "One condition",  "secondary", "CKD",     "DM"
)
log_msg <- function(...) message(sprintf("[56 %s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...))

## ============================================================
## 1. Cohorts (script 12 rules) and checks
## ============================================================
ind <- readRDS("01_Data/chik_sinan_individual_2015_2025.rds")
base <- ind |>
  dplyr::filter(is_confirmed_chik, lubridate::year(event_date) >= 2017,
                !is.na(age_years), age_years >= 0, age_years <= 100,
                sex %in% c("male", "female"),
                dplyr::if_all(dplyr::all_of(c("diabetes", "hypertension", "renal_disease")),
                              ~ .x %in% c("no", "yes"))) |>
  dplyr::mutate(
    year = factor(lubridate::year(event_date)),
    sex = factor(sex, levels = c("female", "male")),
    uf_residence = factor(uf_residence),
    dm = diabetes == "yes", htn = hypertension == "yes", ckd = renal_disease == "yes",
    core_profile = factor(dplyr::case_when(
      !dm & !htn & !ckd ~ "None", dm & !htn & !ckd ~ "DM", !dm & htn & !ckd ~ "HTN",
      !dm & !htn & ckd ~ "CKD", dm & htn & !ckd ~ "DM+HTN", dm & !htn & ckd ~ "DM+CKD",
      !dm & htn & ckd ~ "HTN+CKD", TRUE ~ "DM+HTN+CKD"), levels = core_profile_levels),
    n_conditions = factor(dm + htn + ckd, levels = 0:3)
  )
cohorts <- list(
  hosp = base |> dplyr::filter(hospitalised %in% c("no", "yes")) |>
    dplyr::mutate(y = as.integer(hospitalised == "yes")),
  death = base |> dplyr::filter(!is.na(died_from_chik)) |>
    dplyr::mutate(y = as.integer(died_from_chik))
)
cohorts <- purrr::map(cohorts, ~ dplyr::select(.x, y, age_years, sex, year, uf_residence,
                                               core_profile, n_conditions))
rm(ind, base); invisible(gc())
stopifnot(nrow(cohorts$hosp) == 776072, sum(cohorts$hosp$y) == 25363,
          nrow(cohorts$death) == 983947, sum(cohorts$death$y) == 1050)

adj <- "+ sex + year + s(uf_residence, bs = 're') + s(age_years, k = 10, bs = 'ts')"
f_C <- stats::as.formula(paste("y ~ core_profile", adj))
f_B <- stats::as.formula(paste("y ~ n_conditions", adj))
fit_bam <- function(f, d) mgcv::bam(f, data = d, family = binomial(), method = "fREML",
                                    discrete = TRUE, nthreads = n_threads)

counts <- purrr::imap_dfr(cohorts, function(d, o) d |> dplyr::group_by(core_profile) |>
  dplyr::summarise(n_cases = dplyr::n(), n_events = sum(y), .groups = "drop") |>
  dplyr::mutate(outcome = o, core_profile = as.character(core_profile)))

## ============================================================
## 2. Fits, nesting check, standardisation
## ============================================================
fits <- list()
for (o in names(cohorts)) {
  log_msg("fitting %s", o)
  fits[[o]] <- list(C = fit_bam(f_C, cohorts[[o]]), B = fit_bam(f_B, cohorts[[o]]))
}

## B is nested in C iff the count indicator columns lie in the span of the
## profile indicator columns (smooth/random-effect terms are identical).
nesting <- purrr::imap_dfr(cohorts, function(d, o) {
  XC <- stats::model.matrix(~ core_profile, d)
  XB <- stats::model.matrix(~ n_conditions, d)
  res <- stats::lm.fit(XC, XB)$residuals
  tibble::tibble(outcome = o, max_abs_residual_B_on_C = max(abs(res)),
                 nested = max(abs(res)) < 1e-6)  # floating-point tolerance on 0/1 columns
})
print(nesting)

std_target <- function(d) dplyr::count(d, age_years, sex, year, uf_residence, name = "n")

risk_matrix <- function(fit, d, var, lvls, beta) {
  ## Returns a list of lpmatrices (one per level) on the common target.
  tgt <- std_target(d)
  purrr::map(setNames(lvls, lvls), function(l) {
    nd <- tgt; nd[[var]] <- factor(l, levels = levels(d[[var]]))
    stats::predict(fit, newdata = nd, type = "lpmatrix")
  })
}
std_risks <- function(Xs, beta_mat, w) {
  ## beta_mat: draws x p. Returns draws x levels matrix of weighted mean risks.
  out_m <- sapply(Xs, function(X) {
    out <- numeric(nrow(beta_mat))
    for (i in seq.int(1, nrow(beta_mat), by = 250)) {
      idx <- i:min(i + 249, nrow(beta_mat))
      out[idx] <- colSums(stats::plogis(X %*% t(beta_mat[idx, , drop = FALSE])) * w) / sum(w)
    }
    out
  })
  ## sapply drops to a vector when there is a single draw (the point estimate).
  matrix(out_m, nrow = nrow(beta_mat), dimnames = list(NULL, names(Xs)))
}

q <- function(x, p) as.numeric(stats::quantile(x, p, names = FALSE))
alpha_bonf <- 0.05 / nrow(contrast_defs)

risks_out <- list(); contrasts_out <- list(); draws_keep <- list()
for (o in names(cohorts)) {
  d <- cohorts[[o]]; w <- std_target(d)$n
  log_msg("standardising %s", o)
  for (m in c("C", "B")) {
    fit <- fits[[o]][[m]]
    var <- if (m == "C") "core_profile" else "n_conditions"
    lv <- if (m == "C") core_profile_levels else as.character(0:3)
    Xs <- risk_matrix(fit, d, var, lv)
    set.seed(seed + nchar(o) + (m == "B"))
    bd <- MASS::mvrnorm(B_draws, coef(fit), stats::vcov(fit, unconditional = TRUE))
    pt <- std_risks(Xs, matrix(coef(fit), nrow = 1), w)[1, ]
    dr <- std_risks(Xs, bd, w)
    if (m == "C") draws_keep[[o]] <- list(point = pt, draws = dr)
    risks_out[[paste(o, m)]] <- tibble::tibble(
      outcome = o, model = ifelse(m == "C", "profile", "count"), level = lv,
      n_conditions = if (m == "C") as.character(profile_count[lv]) else lv,
      std_risk_pct = 100 * pt, lower_pct = 100 * apply(dr, 2, q, 0.025),
      upper_pct = 100 * apply(dr, 2, q, 0.975))
  }
  ## Pairwise contrasts within the profile model (shared draws).
  pt <- draws_keep[[o]]$point; dr <- draws_keep[[o]]$draws
  fC <- fits[[o]]$C; b <- coef(fC); V <- fC$Vp
  cn <- function(p) if (p == "None") NA_character_ else paste0("core_profile", p)
  contrasts_out[[o]] <- purrr::pmap_dfr(contrast_defs, function(group, priority, a, b_) {
    ## Draw vectors get distinct names: inside tibble() a column called rr
    ## would mask a draw vector called rr.
    rd_d <- 100 * (dr[, a] - dr[, b_]); rr_d <- dr[, a] / dr[, b_]
    L <- numeric(length(b)); L[match(cn(a), names(b))] <- 1; L[match(cn(b_), names(b))] <- -1
    z <- sum(L * b) / sqrt(as.numeric(t(L) %*% V %*% L))
    ca <- counts[counts$outcome == o & counts$core_profile == a, ]
    cb <- counts[counts$outcome == o & counts$core_profile == b_, ]
    tibble::tibble(
      outcome = o, group = group, priority = priority, contrast = paste(a, "vs", b_),
      profile_a = a, profile_b = b_,
      risk_a_pct = 100 * pt[[a]], risk_b_pct = 100 * pt[[b_]],
      rd_pp = 100 * (pt[[a]] - pt[[b_]]), rd_lower_pp = q(rd_d, 0.025), rd_upper_pp = q(rd_d, 0.975),
      rd_lower_simult_pp = q(rd_d, alpha_bonf / 2), rd_upper_simult_pp = q(rd_d, 1 - alpha_bonf / 2),
      rr = pt[[a]] / pt[[b_]], rr_lower = q(rr_d, 0.025), rr_upper = q(rr_d, 0.975),
      rr_lower_simult = q(rr_d, alpha_bonf / 2), rr_upper_simult = q(rr_d, 1 - alpha_bonf / 2),
      log_or_diff = sum(L * b), wald_z = z, p_wald = 2 * stats::pnorm(-abs(z)),
      n_cases_a = ca$n_cases, n_events_a = ca$n_events,
      n_cases_b = cb$n_cases, n_events_b = cb$n_events,
      sparse = min(ca$n_events, cb$n_events) < 10)
  }) |>
    dplyr::mutate(p_holm = stats::p.adjust(p_wald, "holm"))
}

risks <- dplyr::bind_rows(risks_out) |>
  dplyr::left_join(dplyr::rename(counts, level = core_profile), by = c("outcome", "level"))
contrasts <- dplyr::bind_rows(contrasts_out)

## Consistency with Figure 2 (script 12 cached gam REML estimates).
load("01_Data/core_profile_rr_hosp.RData"); load("01_Data/core_profile_rr_death.RData")
fig2 <- dplyr::bind_rows(dplyr::mutate(core_profile_rr_hosp, outcome = "hosp"),
                         dplyr::mutate(core_profile_rr_death, outcome = "death")) |>
  dplyr::transmute(outcome, level = as.character(core_profile),
                   fig2_risk_pct = 100 * standardised_risk,
                   fig2_lower = 100 * standardised_risk_lower, fig2_upper = 100 * standardised_risk_upper)
consistency <- risks |> dplyr::filter(model == "profile") |>
  dplyr::inner_join(fig2, by = c("outcome", "level")) |>
  dplyr::mutate(rel_diff_point = std_risk_pct / fig2_risk_pct - 1,
                rel_diff_lower = lower_pct / fig2_lower - 1, rel_diff_upper = upper_pct / fig2_upper - 1)
readr::write_csv(consistency, file.path(out_dir, "consistency_with_figure2.csv"))
print(consistency |> dplyr::group_by(outcome) |>
        dplyr::summarise(max_point = max(abs(rel_diff_point)), max_ci = max(abs(c(rel_diff_lower, rel_diff_upper)))))

## ============================================================
## 3. Omnibus within-count tests
## ============================================================
omnibus <- purrr::map_dfr(names(cohorts), function(o) {
  fC <- fits[[o]]$C; b <- coef(fC); V <- fC$Vp
  row <- function(a, bb) { L <- numeric(length(b)); L[match(paste0("core_profile", a), names(b))] <- 1
    L[match(paste0("core_profile", bb), names(b))] <- -1; L }
  wald <- function(L) { d <- as.numeric(L %*% b); s <- as.numeric(t(d) %*% solve(L %*% V %*% t(L), d))
    c(s, nrow(L), stats::pchisq(s, nrow(L), lower.tail = FALSE)) }
  L1 <- rbind(row("DM", "HTN"), row("CKD", "HTN"))
  L2 <- rbind(row("DM+CKD", "DM+HTN"), row("HTN+CKD", "DM+HTN"))
  w1 <- wald(L1); w2 <- wald(L2); wj <- wald(rbind(L1, L2))
  a <- mgcv::anova.gam(fits[[o]]$B, fC, test = "Chisq")
  tibble::tibble(
    outcome = o,
    test = c("A. One-condition heterogeneity: DM = HTN = CKD",
             "B. Two-condition heterogeneity: DM+HTN = DM+CKD = HTN+CKD",
             "C. Profile beyond count, joint (A + B)",
             "C. Profile beyond count, approximate LRT count vs profile model",
             "C. Profile beyond count, AIC difference (profile minus count)"),
    method = c(rep("Wald test on adjusted log-odds, Bayesian covariance Vp", 3),
               "mgcv anova.gam (models nested in parametric terms; smoothing parameters re-estimated, so approximate)",
               "AIC"),
    statistic = c(w1[1], w2[1], wj[1], a$Deviance[2], stats::AIC(fC) - stats::AIC(fits[[o]]$B)),
    df = c(w1[2], w2[2], wj[2], a$Df[2], NA),
    p_value = c(w1[3], w2[3], wj[3], a$`Pr(>Chi)`[2], NA))
}) |> dplyr::left_join(nesting, by = "outcome")

## ============================================================
## 4. Rare-profile check: nonparametric case bootstrap (model refitted)
## ============================================================
boot_cache <- file.path(out_dir, "bootstrap_contrasts.rds")
if (file.exists(boot_cache)) {
  boot <- readRDS(boot_cache); log_msg("reusing bootstrap cache")
} else {
  boot <- list()
  for (o in names(cohorts)) {
    d <- cohorts[[o]]; tgt <- std_target(d)
    set.seed(seed + 100 + nchar(o))
    ## Resample within profile: profile sizes are fixed, but the number of
    ## events in each profile varies, as it must for valid uncertainty.
    strata_idx <- split(seq_len(nrow(d)), d$core_profile)
    res <- matrix(NA_real_, B_boot, 8, dimnames = list(NULL, core_profile_levels))
    zero_dmckd <- logical(B_boot)
    for (r in seq_len(B_boot)) {
      idx <- unlist(lapply(strata_idx, function(s) s[sample.int(length(s), replace = TRUE)]), use.names = FALSE)
      db <- d[idx, ]
      zero_dmckd[r] <- sum(db$y[db$core_profile == "DM+CKD"]) == 0
      fb <- tryCatch(fit_bam(f_C, db), error = function(e) NULL)
      if (is.null(fb)) next
      for (p in core_profile_levels) {
        nd <- tgt; nd$core_profile <- factor(p, levels = core_profile_levels)
        res[r, p] <- sum(stats::predict(fb, newdata = nd, type = "response") * tgt$n) / sum(tgt$n)
      }
      if (r %% 25 == 0) log_msg("  %s bootstrap %d/%d", o, r, B_boot)
    }
    attr(res, "zero_dmckd_events") <- sum(zero_dmckd)
    boot[[o]] <- res
  }
  saveRDS(boot, boot_cache)
}
boot_cmp <- purrr::map_dfr(names(cohorts), function(o) {
  res <- boot[[o]][stats::complete.cases(boot[[o]]), , drop = FALSE]
  purrr::pmap_dfr(contrast_defs, function(group, priority, a, b_) {
    rd <- 100 * (res[, a] - res[, b_]); rr <- res[, a] / res[, b_]
    tibble::tibble(outcome = o, contrast = paste(a, "vs", b_), n_boot = nrow(res),
                   boot_reps_zero_dmckd_events = attr(boot[[o]], "zero_dmckd_events"),
                   boot_rd_lower_pp = q(rd, 0.025), boot_rd_upper_pp = q(rd, 0.975),
                   boot_rr_lower = q(rr, 0.025), boot_rr_upper = q(rr, 0.975))
  })
})
contrasts <- dplyr::left_join(contrasts, boot_cmp, by = c("outcome", "contrast"))

## ============================================================
## 5. Outputs
## ============================================================
readr::write_csv(risks, file.path(out_dir, "within_count_standardised_risks.csv"))
readr::write_csv(contrasts, file.path(out_dir, "within_count_direct_contrasts.csv"))
readr::write_csv(omnibus, file.path(out_dir, "within_count_omnibus_tests.csv"))
print(as.data.frame(contrasts[, c("outcome", "contrast", "rd_pp", "rd_lower_pp", "rd_upper_pp",
                                  "rd_lower_simult_pp", "rd_upper_simult_pp", "rr", "rr_lower", "rr_upper",
                                  "p_holm", "boot_rd_lower_pp", "boot_rd_upper_pp", "sparse")]), digits = 3)
print(as.data.frame(omnibus[, c("outcome", "test", "statistic", "df", "p_value")]), digits = 3)
log_msg("done")
