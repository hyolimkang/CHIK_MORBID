# Output cleanup report

**Scope:** all files under `CHIK_MORBID/03_Output/` at the start of the cleanup.
**Procedure:** `archive/manifest/archive_outputs.R` (dry run, then `--apply`).

Nothing under `01_Data/` or `02_Script/` was modified or moved. No file was deleted. No model was refitted and no figure or table was regenerated. Moves are staged with `git mv` but **not committed**.

## Counts

| | Files |
|---|---:|
| Inspected | **376** |
| MAIN: 15 final files (3 figures × 3 formats + 2 legend `.docx` + 2 tables × `.docx`/`.png`) and 6 Panel A/B production assets | **21** |
| SUPPLEMENTARY: flowchart ×3, four age supplements ×2 formats, individual-condition table `.docx` + `.png` | **13** |
| ARCHIVE: moved | **121** |
| DEPENDENCY_OR_REVIEW: left in place | **221** |

Of the 121 archived files:

| Destination | Files |
|---|---:|
| `figures/` | 38 |
| `prototypes/figures/` | 40 |
| `prototypes/tables/` | 6 |
| `figure_revision/` (top level) | 15 |
| `figure_revision/figure3_final/` | 9 |
| `figure_revision/figure3_final/panels/` | 2 |
| `figure_revision/figure4_final/` | 1 |
| `figure_revision/within_count/` | 6 |
| `tables/` | 4 |

The 221 files left in place are:
- source data and estimates (CSV/RDS/XLSX);
- verification reports and reconciliation files;
- all 111 files under `validation/`, kept with their reports;
- the flagged supplementary candidates listed in `OUTPUT_MANIFEST.md`;
- two superseded tables that a script still reads (below).

## Verification
- **Final files:** all 21 MAIN and 13 SUPPLEMENTARY files exist at their unchanged paths. None was classified as ARCHIVE; the script asserts this before moving anything.
- **Archived files:** all 121 exist at their archive paths and no longer exist at their original paths. Original and archived MD5 checksums match for **121/121** (`archive/manifest/archive_manifest.csv`, column `move_status` = "moved; checksum verified").
- **No collisions or overwrites:** destinations were unique and did not exist beforehand (asserted).
- **Git:** `git status` shows 121 renames (`R`) plus the new `archive/` documentation files. History is preserved and nothing is committed.
- **No broken code references:** for every file, all R scripts under `02_Script/` were searched for its filename (or quoted stem). **No archived file is read by any script.** References to archived files are writes (e.g. `ggsave`, `print(doc)`) or comments.

## Unresolved dependency (left unmoved)
- `tables/table_core3_standardised_risks.docx` (old Table 2) and `tables/table_joint_profile_standardised_risks.docx` (old Table 3) are superseded, but `02_Script/60_tables_results_reconciliation.R` reads them to document the old-vs-new comparison. They stay at their current paths.
- A safe later change would be to point script 60 at the archive paths once the reconciliation is final. This has **not** been done, because `02_Script/` was out of scope.

## Scripts that would regenerate archived outputs in the active folders
If re-run, these scripts write files that are now archived. Their output paths were intentionally not changed:

| Script | Outputs it would recreate |
|---|---|
| `12_core3_condition_specific_rr.R` | `fig_core_profile_rr_*`, `fig_core3_rr_*`, `fig3_*` |
| `03_relative_risk.R` | `fig_rr_*`, `fig_absolute_risk_*`, `fig_predicted_*` |
| `06_`, `09_`, `10_` | GBD/copula graphics |
| `15_`, `21_` | Alternative Figure 1 layouts |
| `18_`, `27_` | Old Table 1 and Table 3 Word/Excel files |
| `29_`–`37_`, `42_` | `*_PROTOTYPE` figures and workbooks |
| `49_` | Mortality-only Figure 3 and the older `Supp_Figure_age_*` |
| `51_` | `Figure4_count_vs_profile_main` |
| `53_` | Four-panel Figure 3, layout candidates, duplicate support heatmap, curve validation |
| `54_` | Panels preview |
| `57_` | Within-count candidates A/B |
| `58_` | Low-resolution proof |
| `side_analyses/table_basic.R` | `chik_sinan_descriptive_tables.xlsx` |

Suggested follow-ups, not applied:
- (a) re-run `archive_outputs.R --apply` after any such regeneration; or
- (b) later, add an `archive_obsolete = TRUE` switch, or redirect the obsolete outputs in those scripts, once the manuscript set is frozen.

## Numerical results
**Unchanged.** No estimates, confidence intervals or event counts were edited or regenerated, and every moved file is byte-identical (checksum-verified). All source estimates for Figures 1–3 and Tables 1–2 remain at their original paths, together with every validation and reconciliation record.

## Other notes
- `Table2_Joint_Profiles_FINAL.docx` was open in Word during the earlier sync. It is retained (not moved), and its content is identical to the committed version.
- The `archive/validation_visuals/` folder is empty by design; see `archive/README.md`.
