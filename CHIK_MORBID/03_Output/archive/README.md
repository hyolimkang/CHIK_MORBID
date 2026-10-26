# 03_Output/archive

This folder holds output files that are **not** part of the final manuscript or the selected supplementary set: superseded figure and table versions, rejected layout candidates, intermediate proofs, duplicates, and prototype population-burden graphics. Nothing here was edited or regenerated. Files were moved with `git mv`, and each move was checksum-verified (MD5).

## Organisation

| Folder | Contents |
|---|---|
| `figures/` | Former `03_Output/figures/` graphics: earlier RR and absolute-risk figures, old Figure 3 age curves, alternative Figure 1 / flowchart layouts, and GBD/copula background-prevalence graphics |
| `figure_revision/` | Former `03_Output/figure_revision/` candidates and proofs, with the original sub-folders kept (`figure3_final/`, `figure3_final/panels/`, `figure4_final/`, `within_count/`) |
| `tables/` | Superseded Table 1 / Table 3 drafts and early descriptive tables |
| `prototypes/` | `*_PROTOTYPE` population-burden, clustering and background-prevalence figures (`prototypes/figures/`) and workbooks (`prototypes/tables/`) |
| `validation_visuals/` | Intentionally empty. Validation graphics were left beside their validation reports in `03_Output/validation/`, which cite them by filename. |
| `manifest/` | `archive_manifest.csv` (every moved file), `output_inventory.csv` (every file inspected, with classification and code references) and `archive_outputs.R` (the reproducible procedure) |

Original filenames are unchanged, and the path inside `archive/` mirrors the original path below `03_Output/`, except that prototypes are grouped under `prototypes/`.

## Recovering a file
1. Look up the file in `manifest/archive_manifest.csv` (`original_path` → `archived_path`).
2. Move it back: `git mv "<archived_path>" "<original_path>"`.
3. If you need the copy as it was before archiving, use git history (`git log --follow -- "<archived_path>"`).

## Regeneration note
Several scripts still write these filenames to the active folders if re-run, for example script 12 (`fig_core_profile_rr_*`, `fig3_*`), scripts 49, 51, 53, 54, 57 and 58 (figure candidates and proofs), and scripts 29–37 (prototypes). Script output paths were deliberately **not** changed. Re-running such a script will recreate the obsolete file in its original location; it can be re-archived with `archive_outputs.R --apply`. See `../OUTPUT_CLEANUP_REPORT.md`.
