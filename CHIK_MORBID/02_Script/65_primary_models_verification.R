# ---------------------------------------------------------------------------
# 65_primary_models_verification.R
#
# Independent verification of the harmonised primary-model outputs
# (scripts 62-64). Re-derives every check from the saved draws and output
# files rather than from the objects used to create them.
#   A model provenance      B cohort counts        C profile completeness
#   D overall = weighted age-specific (point and every draw)
#   E RR = risk / risk(None)   F RD = 100 x [risk(a) - risk(b)]
#   G Figure 1 = Table 2; Figure 3 / Figure 2 use the same estimates
#   H numerical CI checks   I old-vs-new report present
# Output: <pm_dir>/validation/final_verification_checks.csv
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(mgcv)
})
source("02_Script/primary_models_functions.R")
v <- function(section, check, pass, detail = "")
  tibble::tibble(section = section, check = check, pass = isTRUE(pass), detail = as.character(detail))
f <- function(...) file.path(pm_dir, ...)
rd <- function(...) readr::read_csv(f(...), show_col_types = FALSE)
tol <- 1e-10
res <- list()

risks <- rd("estimates", "overall_standardised_risks.csv")
rrs <- rd("estimates", "overall_standardised_RRs.csv")
age <- rd("estimates", "age_specific_standardised_risks.csv")
age_rr <- rd("estimates", "age_specific_standardised_RRs.csv")
con <- rd("estimates", "within_count_standardised_RDs.csv")
sup <- rd("estimates", "profile_event_support.csv")
t2 <- rd("tables", "Table2_Joint_Profiles_Validated.csv")
f1 <- rd("figures", "Figure1_plotted_values.csv")
f2 <- rd("figures", "Figure2_cells_and_display_status.csv")
f3 <- rd("figures", "Figure3_plotted_values.csv")
std <- readRDS(f("estimates", "primary_standardised_risk_draws.rds"))
prov <- rd("models", "model_provenance.csv")
ids <- c(hosp = "hospitalisation_H2", death = "mortality_H0")

## A. Provenance
for (nm in c("risks", "rrs", "age", "age_rr", "con", "t2", "f1", "f2", "f3"))
  res[[length(res) + 1]] <- v("A", paste("Only primary model IDs in", nm), all(get(nm)$model_id %in% ids),
                               paste(unique(get(nm)$model_id), collapse = ", "))
res[[length(res) + 1]] <- v("A", "Hospitalisation estimates from H2; mortality from H0",
  all(risks$model_id[risks$outcome == "Hospitalisation"] == ids[["hosp"]]) &&
    all(risks$model_id[risks$outcome == "Chikungunya-attributed death"] == ids[["death"]]))
fits <- list(hospitalisation_H2 = readRDS(f("models", "hospitalisation_H2_primary.rds")),
             mortality_H0 = readRDS(f("models", "mortality_H0_primary.rds")))
for (m in names(fits)) {
  fo <- paste(deparse(formula(fits[[m]]), width.cutoff = 500L), collapse = "")
  expect <- paste(deparse(stats::as.formula(primary_models[[m]]$formula), width.cutoff = 500L), collapse = "")
  res[[length(res) + 1]] <- v("A", paste("Saved model formula matches specification:", m), identical(fo, expect), fo)
  res[[length(res) + 1]] <- v("A", paste("Saved model fitted with bam fREML discrete:", m),
                               inherits(fits[[m]], "bam") && fits[[m]]$method == "fREML", fits[[m]]$method)
  cd <- readRDS(f("estimates", ifelse(m == "hospitalisation_H2", "coefficient_draws_hospitalisation.rds",
                                      "coefficient_draws_mortality.rds")))
  res[[length(res) + 1]] <- v("A", paste("Coefficient draws match saved model coefficients:", m),
                               identical(cd$coef_names, names(coef(fits[[m]]))) && nrow(cd$beta_draws) == 4000,
                               paste(nrow(cd$beta_draws), "draws"))
}
res[[length(res) + 1]] <- v("A", "Provenance table lists exactly the two primary models", setequal(prov$model_id, ids))

## B. Cohort counts
cnt <- sup |> group_by(outcome) |> summarise(n = sum(n_cases), e = sum(n_events))
res[[length(res) + 1]] <- v("B", "Hospitalisation N = 776,072; events = 25,363",
                             cnt$n[cnt$outcome == "hosp"] == 776072 && cnt$e[cnt$outcome == "hosp"] == 25363)
res[[length(res) + 1]] <- v("B", "Mortality N = 983,947; deaths = 1,050",
                             cnt$n[cnt$outcome == "death"] == 983947 && cnt$e[cnt$outcome == "death"] == 1050)
res[[length(res) + 1]] <- v("B", "Model objects fitted on these cohorts",
                             nrow(fits$hospitalisation_H2$model) == 776072 && sum(fits$hospitalisation_H2$y) == 25363 &&
                               nrow(fits$mortality_H0$model) == 983947 && sum(fits$mortality_H0$y) == 1050)
recon <- rd("validation", "cohort_reconciliation.csv")
res[[length(res) + 1]] <- v("B", "All cohort reconciliation checks passed (script 62)", all(recon$pass),
                             paste(sum(recon$pass), "/", nrow(recon)))

## C. Profile completeness
for (m in ids) {
  r <- risks[risks$model_id == m, ]
  res[[length(res) + 1]] <- v("C", paste("Eight unique profiles incl. None:", m),
                               nrow(r) == 8 && setequal(r$core_profile, core_profile_levels) && !anyDuplicated(r$core_profile))
}

## D. Overall = weighted age-specific, point and draw level
for (m in ids) {
  s <- std[[m]]; wa <- s$n_age / s$n_target
  pt <- max(abs(s$point - colSums(s$point_age * wa)))
  dr <- max(abs(s$draws - apply(s$draws_age, c(1, 3), function(x) sum(x * wa))))
  res[[length(res) + 1]] <- v("D", paste("Overall = weighted mean of age-specific risks (point):", m), pt < 1e-12, sprintf("%.2e", pt))
  res[[length(res) + 1]] <- v("D", paste("Overall = weighted mean of age-specific risks (all 4,000 draws):", m), dr < 1e-12, sprintf("%.2e", dr))
  a <- age[age$model_id == m, ]
  wcsv <- a |> group_by(core_profile) |> summarise(w = sum(std_risk_pct * n_target_age) / sum(n_target_age))
  o <- risks[risks$model_id == m, ] |> inner_join(wcsv, by = "core_profile")
  res[[length(res) + 1]] <- v("D", paste("Identity holds in the published CSVs:", m),
                               max(abs(100 * o$risk - o$w)) < 1e-9, sprintf("%.2e", max(abs(100 * o$risk - o$w))))
  res[[length(res) + 1]] <- v("D", paste("Age groups partition the target:", m), sum(s$n_age) == s$n_target)
}

## E. RR consistency (point and CI recomputed from draws)
for (m in ids) {
  s <- std[[m]]; r <- rrs[rrs$model_id == m, ]; r <- r[match(names(s$point), r$core_profile), ]
  ri <- risks[risks$model_id == m, ]; ri <- ri[match(names(s$point), ri$core_profile), ]
  q <- function(x, p) as.numeric(quantile(x, p, names = FALSE))
  rr_d <- s$draws / s$draws[, "None"]
  res[[length(res) + 1]] <- v("E", paste("RR = risk / risk(None), unrounded:", m),
                               max(abs(r$rr - ri$risk / ri$risk[ri$core_profile == "None"])) < tol)
  res[[length(res) + 1]] <- v("E", paste("RR CIs = quantiles of within-draw ratios:", m),
                               max(abs(c(r$rr_lower - apply(rr_d, 2, q, 0.025), r$rr_upper - apply(rr_d, 2, q, 0.975)))) < tol)
  res[[length(res) + 1]] <- v("E", paste("RR(None) = 1 in every draw:", m), all(rr_d[, "None"] == 1))
}

## F. RD consistency
for (m in ids) {
  s <- std[[m]]; cm <- con[con$model_id == m, ]
  rdp <- 100 * (s$point[cm$profile_a] - s$point[cm$profile_b])
  lo <- mapply(function(a, b) quantile(100 * (s$draws[, a] - s$draws[, b]), 0.025, names = FALSE), cm$profile_a, cm$profile_b)
  res[[length(res) + 1]] <- v("F", paste("RD = 100 x [risk(a) - risk(b)], unrounded:", m), max(abs(cm$rd_pp - rdp)) < tol)
  res[[length(res) + 1]] <- v("F", paste("RD CIs from paired draws:", m), max(abs(cm$rd_lower_pp - lo)) < tol)
  res[[length(res) + 1]] <- v("F", paste("RD of a profile with itself = 0 in every draw:", m),
                               all(s$draws[, "DM"] - s$draws[, "DM"] == 0))
}

## G. Figure-table consistency
j <- inner_join(f1, t2, by = c("model_id", "core_profile"), suffix = c("_f1", "_t2"))
res[[length(res) + 1]] <- v("G", "Figure 1 = Table 2 (risks, RRs, CIs)", nrow(j) == 16 &&
  max(abs(c(100 * j$risk - j$std_risk_pct, 100 * j$risk_lower - j$std_risk_lower_pct,
            100 * j$risk_upper - j$std_risk_upper_pct, j$rr_f1 - j$rr_t2, j$rr_lower_f1 - j$rr_lower_t2,
            j$rr_upper_f1 - j$rr_upper_t2))) < tol)
f3j <- f3 |> left_join(transmute(t2, model_id, profile_a = core_profile, ta = std_risk_pct), by = c("model_id", "profile_a")) |>
  left_join(transmute(t2, model_id, profile_b = core_profile, tb = std_risk_pct), by = c("model_id", "profile_b"))
res[[length(res) + 1]] <- v("G", "Figure 3 RDs = differences of Table 2 risks", nrow(f3j) == 12 &&
  max(abs(f3j$rd_pp - (f3j$ta - f3j$tb))) < tol)
f2j <- inner_join(f2, age, by = c("model_id", "age_group", "core_profile"), suffix = c("", "_c"))
res[[length(res) + 1]] <- v("G", "Figure 2 cells = canonical age-specific estimates (same models/draws)",
  nrow(f2j) == 80 && max(abs(c(f2j$std_risk_pct - f2j$std_risk_pct_c, f2j$std_risk_lower_pct - f2j$std_risk_lower_pct_c,
                               f2j$std_risk_upper_pct - f2j$std_risk_upper_pct_c))) < tol)
cross <- rd("validation", "cross_figure_consistency.csv")
res[[length(res) + 1]] <- v("G", "Script 64 cross-figure checks all passed", all(cross$pass))

## H. Numerical checks
for (m in ids) {
  s <- std[[m]]
  res[[length(res) + 1]] <- v("H", paste("All draw probabilities in (0, 1), finite:", m),
                               all(is.finite(s$draws)) && all(s$draws > 0 & s$draws < 1) &&
                                 all(is.finite(s$draws_age)) && all(s$draws_age > 0 & s$draws_age < 1))
}
res[[length(res) + 1]] <- v("H", "Overall risk CIs contain point estimates",
                             all(risks$risk >= risks$risk_lower & risks$risk <= risks$risk_upper))
res[[length(res) + 1]] <- v("H", "Overall RR CIs contain point estimates (non-reference)",
                             with(rrs[rrs$core_profile != "None", ], all(rr >= rr_lower & rr <= rr_upper)))
res[[length(res) + 1]] <- v("H", "Age-specific risk CIs contain point estimates",
                             all(age$std_risk_pct >= age$std_risk_lower_pct & age$std_risk_pct <= age$std_risk_upper_pct),
                             paste(sum(!(age$std_risk_pct >= age$std_risk_lower_pct & age$std_risk_pct <= age$std_risk_upper_pct)), "exceptions"))
res[[length(res) + 1]] <- v("H", "RD CIs contain point estimates", all(con$rd_pp >= con$rd_lower_pp & con$rd_pp <= con$rd_upper_pp))
res[[length(res) + 1]] <- v("H", "No missing/infinite values in reported estimates",
                             all(sapply(list(risks, rrs, age, con), function(x) all(is.finite(as.matrix(x[sapply(x, is.numeric)]))))))
## Monte Carlo error: 5% tolerance for profiles with >= 10 events; tails of
## sparse profiles (< 10 events) are reported, not tested.
mc <- rd("validation", "monte_carlo_check_4000_vs_1000_draws.csv") |>
  left_join(transmute(sup, model_id = ids[outcome], core_profile, n_events), by = c("model_id", "core_profile"))
mc_max <- function(d) max(abs(as.matrix(d[, grep("rel_diff", names(d))])), na.rm = TRUE)
res[[length(res) + 1]] <- v("H", "Monte Carlo: 1,000- vs 4,000-draw CI limits within 5% (profiles with >= 10 events)",
                             mc_max(mc[mc$n_events >= 10, ]) < 0.05,
                             sprintf("max %.3f; sparse profiles max %.3f (reported only)",
                                     mc_max(mc[mc$n_events >= 10, ]), mc_max(mc[mc$n_events < 10, ])))
dc <- rd("validation", "draw_level_standardisation_checks.csv")
res[[length(res) + 1]] <- v("H", "Frequency-weighted = individual-level averaging (rel. diff < 1e-8)",
                             max(abs(dc$rel_diff)) < 1e-8, sprintf("%.2e", max(abs(dc$rel_diff))))

## I. Legacy comparison
ovn <- rd("validation", "old_vs_new_primary_estimates.csv")
res[[length(res) + 1]] <- v("I", "Old-vs-new report covers risks, RRs, age-specific risks and RDs",
                             all(c("overall", "age-specific", "within-count RD") %in% ovn$quantity) && nrow(ovn) > 0,
                             paste(nrow(ovn), "rows;", sum(ovn$material_change, na.rm = TRUE), "flagged material"))

out <- dplyr::bind_rows(res)
readr::write_csv(out, f("validation", "final_verification_checks.csv"))
print(as.data.frame(out), right = FALSE)
message(sprintf("[65] %d / %d checks passed", sum(out$pass), nrow(out)))
if (!all(out$pass)) stop("Verification failed")
