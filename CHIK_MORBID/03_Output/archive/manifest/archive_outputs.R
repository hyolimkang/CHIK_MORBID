# -----------------------------------------------------------------------------
# archive_outputs.R
#
# Inventory, classify and (optionally) archive 03_Output files so that only
# the selected manuscript figures/tables and supplementary materials remain in
# the active output folders. Nothing is deleted or regenerated; moves use
# `git mv` (staged, not committed) and every move is checksum-verified.
#
# Run from the CHIK_MORBID project folder:
#   Rscript 03_Output/archive/manifest/archive_outputs.R          # dry run
#   Rscript 03_Output/archive/manifest/archive_outputs.R --apply  # move files
#
# Writes 03_Output/archive/manifest/output_inventory.csv (every file) and,
# with --apply, archive_manifest.csv (every moved file).
# -----------------------------------------------------------------------------

apply_moves <- "--apply" %in% commandArgs(trailingOnly = TRUE)
out_root <- "03_Output"
arch_root <- file.path(out_root, "archive")
man_dir <- file.path(arch_root, "manifest")

files <- list.files(out_root, recursive = TRUE, all.files = FALSE)
files <- files[!startsWith(files, "archive/")]
rel <- files
ext <- tolower(tools::file_ext(rel))
base <- basename(rel)
stem <- tools::file_path_sans_ext(base)

## ---- 1. Retention lists (exact confirmed paths) ------------------------------
fmt3 <- function(p) paste0(p, c(".pdf", ".png", ".svg"))
main_files <- c(
  fmt3("figure_revision/Figure2_profile_RR_refined"),                            # Main Figure 1
  fmt3("figure_revision/figure3_final/panels/Figure3_combined_AB_final"),        # Main Figure 2
  "figure_revision/figure3_final/panels/Figure3_title_and_legend.docx",
  fmt3("figure_revision/figure4_final/Figure4_within_count_RD_FINAL"),           # Main Figure 3
  "figure_revision/figure4_final/Figure4_Title_and_Legend.docx",
  "tables/table1_updated/Table1_Study_Population_FINAL.docx",                    # Main Table 1
  "tables/table1_updated/Table1_Study_Population_FINAL.png",
  "tables/tables_2_3_reconciled/Table2_Joint_Profiles_FINAL.docx",               # Main Table 2
  "tables/tables_2_3_reconciled/Table2_Joint_Profiles_FINAL.png"
)
main_assets <- c(  # production assets for Main Figure 2 (not separate figures)
  fmt3("figure_revision/figure3_final/panels/Figure3_panelA_hospitalisation"),
  fmt3("figure_revision/figure3_final/panels/Figure3_panelB_death")
)
supp_files <- c(
  fmt3("figures/figure_cohort_flow"),                                            # Supp Figure S1
  paste0("figure_revision/figure3_final/Supp_hospital_age_profile_support", c(".pdf", ".png")),
  paste0("figure_revision/figure3_final/Supp_mortality_age_profile_support", c(".pdf", ".png")),
  paste0("figure_revision/figure3_final/Supp_hospital_age_specific_RR", c(".pdf", ".png")),
  paste0("figure_revision/figure3_final/Supp_mortality_age_specific_RR", c(".pdf", ".png")),
  "tables/tables_2_3_reconciled/Supplementary_Table_Individual_Conditions_FINAL.docx",
  "tables/tables_2_3_reconciled/Supplementary_Table_Individual_Conditions_FINAL.png"
)
stopifnot(all(c(main_files, main_assets, supp_files) %in% rel))

## Undecided supplementary candidates: left in place, flagged for review.
review_candidates <- c(
  paste0("figure_revision/figure3_final/Supp_hospital_age_interaction_robustness", c(".pdf", ".png")),
  grep("^figures/fig_condition_recording_by_", rel, value = TRUE),
  grep("^figures/figure_evidence_", rel, value = TRUE),
  "tables/clinical_outcomes_by_underlying_condition.docx",
  "tables/clinical_outcomes_by_underlying_condition_2017_2025_updated.docx",
  "tables/table_core3_confirmed_hosp_death_by_condition.docx",
  "tables/table_core3_risk_differences.docx"
)

## ---- 2. Archive rules (presentation outputs only) ----------------------------
presentation_ext <- c("pdf", "png", "svg", "jpg", "jpeg", "pptx", "docx", "xlsx")
archive_rule <- function(p) {
  b <- basename(p)
  if (grepl("PROTOTYPE", b)) return("prototype: population burden / clustering / background prevalence")
  if (startsWith(p, "figures/")) {
    if (grepl("^figure1_(flowchart|study_architecture)", b)) return("superseded: alternative Figure 1 / flowchart layout (final flowchart = figure_cohort_flow, Supp Figure S1)")
    if (grepl("^fig_core_profile_rr", b)) return("superseded: earlier overall/age-band profile RR figure (final = Figure2_profile_RR_refined)")
    if (grepl("^fig3_", b)) return("superseded: earlier Figure 3 condition/profile age curves and forest")
    if (grepl("^fig_(comorbidity_age_by_country|corr_heatmap|global_stack|heatmap_2plus|region_comorb)", b))
      return("prototype: GBD/copula background-prevalence graphics")
    if (grepl("^fig_", b)) return("superseded/exploratory: earlier RR, absolute-risk and model graphics")
  }
  if (startsWith(p, "figure_revision/")) {
    if (grepl("^Figure3_age_specific_absolute_risk_main", b)) return("superseded: mortality-only Figure 3 (final = Figure3_combined_AB_final)")
    if (grepl("^Figure4_count_vs_profile_main", b)) return("superseded: count-vs-profile model-comparison figure (final = Figure4_within_count_RD_FINAL)")
    if (grepl("^Supp_Figure_age_(specific_mortality_RR|support_heatmap)", b)) return("superseded: earlier mortality supplementary age figure (final = figure3_final/Supp_mortality_*)")
    if (grepl("^Supp_Figure_age_RR_curve_validation", b)) return("model development: continuous age-specific RR curve validation")
    if (grepl("^Figure3_candidate[12]_", b)) return("rejected layout candidate for four-panel Figure 3")
    if (grepl("^Figure3_combined_absolute_risks_FINAL", b)) return("superseded: four-panel Figure 3 (final = two-panel Figure3_combined_AB_final)")
    if (grepl("^hospital_age_profile_support_heatmap", b)) return("duplicate: identical to Supp_hospital_age_profile_support")
    if (grepl("^Supp_hospital_RR_curve_validation", b)) return("model development: continuous hospitalisation RR curves (H1 vs H2)")
    if (grepl("^Figure3_panelsAB_preview", b)) return("intermediate proof: side-by-side preview of panels")
    if (grepl("^Figure4_candidate_[AB]_", b)) return("superseded: within-count figure candidates (final = Figure4_within_count_RD_FINAL)")
    if (grepl("_lowres_proof", b)) return("intermediate proof: low-resolution legibility check")
  }
  if (startsWith(p, "tables/") && !grepl("/", sub("^tables/", "", p))) {
    if (b == "table1_analytic_cohorts.docx") return("superseded: previous Table 1 (final = table1_updated/Table1_Study_Population_FINAL)")
    if (b == "table_core3_standardised_risks.docx") return("superseded: previous Table 2, individual conditions (final = Supplementary_Table_Individual_Conditions_FINAL)")
    if (grepl("^table_joint_profile_standardised_risks\\.(docx|xlsx)$", b)) return("superseded: previous Table 3, joint profiles (final = Table2_Joint_Profiles_FINAL)")
    if (b == "descriptive_comorbid.docx") return("superseded: early descriptive table (July)")
    if (b == "chik_sinan_descriptive_tables.xlsx") return("superseded: side-analysis descriptive workbook (7-condition cohort)")
  }
  NA_character_
}

dest_for <- function(p, reason) {
  sub_path <- if (grepl("^prototype", reason)) {
    file.path("prototypes", p)            # keeps figures/ or tables/ prefix
  } else {
    p                                     # figures/..., figure_revision/..., tables/...
  }
  file.path(arch_root, sub_path)
}

## ---- 3. Code-reference check -------------------------------------------------
scripts <- list.files("02_Script", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
code <- do.call(rbind, lapply(scripts, function(s) {
  l <- readLines(s, warn = FALSE, encoding = "UTF-8")
  data.frame(script = s, line = l, stringsAsFactors = FALSE)
}))
read_pat <- "read|load\\(|readRDS|fread|getSheetNames|read_docx|readLines"
ref_info <- function(b, s) {
  hit <- grepl(b, code$line, fixed = TRUE)
  if (!any(hit)) hit <- grepl(paste0('"', s, '"'), code$line, fixed = TRUE)
  if (!any(hit)) return(c(referenced_by = "", read_by = ""))
  h <- code[hit, ]
  c(referenced_by = paste(unique(basename(h$script)), collapse = "; "),
    read_by = paste(unique(basename(h$script[grepl(read_pat, h$line)])), collapse = "; "))
}
refs <- t(mapply(ref_info, base, stem))

## ---- 4. Classification ----------------------------------------------------------
inv <- data.frame(original_path = file.path(out_root, rel), filename = base, extension = ext,
                  size_bytes = file.size(file.path(out_root, rel)),
                  referenced_by = refs[, "referenced_by"], read_by = refs[, "read_by"],
                  stringsAsFactors = FALSE)
inv$category <- NA_character_; inv$related_item <- ""; inv$reason <- ""; inv$proposed_destination <- ""
for (i in seq_len(nrow(inv))) {
  p <- rel[i]
  if (p %in% main_files) {
    inv$category[i] <- "MAIN"; inv$reason[i] <- "final main manuscript figure/table"
  } else if (p %in% main_assets) {
    inv$category[i] <- "MAIN"; inv$reason[i] <- "production asset for Main Figure 2 (individual panel)"
  } else if (p %in% supp_files) {
    inv$category[i] <- "SUPPLEMENTARY"; inv$reason[i] <- "selected supplementary figure/table"
  } else if (p %in% review_candidates) {
    inv$category[i] <- "DEPENDENCY_OR_REVIEW"; inv$reason[i] <- "supplementary candidate: inclusion not yet decided"
  } else if (startsWith(p, "validation/")) {
    inv$category[i] <- "DEPENDENCY_OR_REVIEW"
    inv$reason[i] <- "internal analytical validation record (kept with its validation report)"
  } else {
    r <- if (ext[i] %in% presentation_ext) archive_rule(p) else NA_character_
    if (!is.na(r) && nchar(inv$read_by[i]) == 0) {
      inv$category[i] <- "ARCHIVE"; inv$reason[i] <- r; inv$proposed_destination[i] <- dest_for(p, r)
    } else if (!is.na(r)) {
      inv$category[i] <- "DEPENDENCY_OR_REVIEW"; inv$reason[i] <- paste0("would archive (", r, ") but read by ", inv$read_by[i])
    } else {
      inv$category[i] <- "DEPENDENCY_OR_REVIEW"
      inv$reason[i] <- if (nchar(inv$read_by[i]) > 0) paste("read by", inv$read_by[i]) else
        if (ext[i] %in% c("csv", "rds", "rdata", "xlsx", "md", "docx")) "source data / verification record retained for reproducibility" else
          "not covered by an archive rule; retained for review"
    }
  }
}
## Related manuscript item.
inv$related_item[grepl("Figure2_profile_RR_refined|figure2_plotted", rel)] <- "Main Figure 1 (orig. Figure 2)"
inv$related_item[grepl("figure3_final", rel)] <- "Main Figure 2 (orig. Figure 3) / age supplements"
inv$related_item[grepl("figure4_final|within_count", rel)] <- "Main Figure 3 (orig. Figure 4)"
inv$related_item[grepl("table1_updated|figure_cohort_flow", rel)] <- "Main Table 1 / Supp Figure S1"
inv$related_item[grepl("tables_2_3_reconciled", rel)] <- "Main Table 2 / individual-condition supplement"

## ---- 5. Dry-run checks -----------------------------------------------------------------
stopifnot(!any(inv$category[match(file.path(out_root, c(main_files, main_assets, supp_files)), inv$original_path)] == "ARCHIVE"))
dest <- inv$proposed_destination[inv$category == "ARCHIVE"]
stopifnot(!any(duplicated(dest)), !any(file.exists(dest)))   # no collisions / overwrites
write.csv(inv, file.path(man_dir, "output_inventory.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(table(inv$category))
cat("Archive candidates read by code (left in place):",
    sum(grepl("^would archive", inv$reason)), "\n")

if (!apply_moves) { message("Dry run only. Re-run with --apply to move files."); quit(save = "no") }

## ---- 6. Apply moves -------------------------------------------------------------
md5 <- function(f) unname(tools::md5sum(f))
mv <- inv[inv$category == "ARCHIVE", ]
mv$original_md5 <- md5(mv$original_path); mv$archived_md5 <- NA_character_; mv$move_status <- NA_character_
tracked <- system2("git", c("ls-files", "--", out_root), stdout = TRUE)
for (i in seq_len(nrow(mv))) {
  from <- mv$original_path[i]; to <- mv$proposed_destination[i]
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- if (from %in% tracked) {
    system2("git", c("mv", "-k", shQuote(from), shQuote(to))) == 0 && file.exists(to) && !file.exists(from)
  } else {
    file.rename(from, to)
  }
  if (isTRUE(ok) && file.exists(to)) {
    mv$archived_md5[i] <- md5(to)
    mv$move_status[i] <- if (identical(mv$archived_md5[i], mv$original_md5[i])) "moved; checksum verified" else "moved; CHECKSUM MISMATCH"
  } else {
    mv$move_status[i] <- "NOT MOVED (file locked or git mv failed); left at original path"
  }
}
write.csv(data.frame(original_path = mv$original_path, archived_path = mv$proposed_destination,
                     filename = mv$filename, file_type = mv$extension, reason = mv$reason,
                     original_checksum_md5 = mv$original_md5, archived_checksum_md5 = mv$archived_md5,
                     move_status = mv$move_status),
          file.path(man_dir, "archive_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(table(mv$move_status))
