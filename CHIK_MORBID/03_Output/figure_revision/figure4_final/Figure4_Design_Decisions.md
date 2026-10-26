# Figure 4: design decisions

**Final figure:** `Figure4_within_count_RD_FINAL.pdf/.svg/.png` (250 × 84 mm landscape, 600 dpi PNG)
**Low-resolution proof:** `Figure4_within_count_RD_FINAL_lowres_proof.png`
**Script:** `02_Script/58_figure4_final.R`

## Layout
- **Three columns.** Profile comparison | Hospitalisation | Chikungunya-attributed death, built as three plots that share one numeric row grid (identical y-limits and zero expansion). Every comparison therefore sits on exactly the same row in all three columns.
- **Labels once.** Comparison labels appear only in the left column. Group headings ("One condition", "Two conditions") are horizontal, bold and muted, on their own rows, with an extra blank row before the second group. There are no rotated strips.
- **Numbers outside the plot.** Each outcome's RD (95% CI) column is drawn in a reserved right margin of that outcome's plot (85 pt), outside the plotting area, using coordinate limits (not scale limits) with clipping off. The numbers align with their rows, and each outcome heading spans its forest plus its numbers, so headings are never clipped.
- **Column widths.** Widths are fixed by patchwork (0.62 : 1 : 1), not left to automatic margins. This removes the overlaps and clipping of the previous Candidate B.
- **Not included in the image** (all moved to `Figure4_Title_and_Legend.docx`): overall title, panel letters, direction arrows, "first/second higher" text, model names, p-values, significance stars, notes.

## Axes
- **Linear risk-difference axes** with separate ranges per outcome, chosen to contain every interval without truncation:
  - hospitalisation −4 to 10 pp, ticks every 2;
  - death −1 to 1.5 pp, ticks every 0.5.
- Forcing a common range would flatten the mortality estimates and falsely imply equal precision.
- **Reference line:** the zero line is a mid-grey 0.4 pt line, clearly heavier than the faint vertical gridlines (0.2 pt, #EBEBEB). There are no horizontal gridlines.
- **Axis title:** "Standardised risk difference (percentage points)", set on two lines.

## Marks and the sparse convention
- **Adequately supported:** filled dark-blue circle (#1F4E89) with a solid blue 0.5 pt interval.
- **Sparse:** open grey circle with a solid grey interval (#9A9A9A) and grey numbers that are still legible. There are no dashed intervals and no end caps.
- **Sparse means data support only:** either profile has <10 outcome events, the rule coded in script 56. It flags only the two mortality contrasts involving DM+CKD (4 deaths). Grey is **not** applied because an interval crosses zero: hospitalisation HTN+CKD vs DM+CKD crosses zero but stays blue, because both profiles have ≥27 events.

## Typography
- Arial throughout, whose digits are tabular, so the numeric columns align.
- Unicode minus signs for negative values.
- Hierarchy:
  - column and outcome headings: 8.6 pt bold;
  - group headings: 7.7 pt bold, muted;
  - comparison labels and numbers: ~7.4 pt;
  - ticks: 7 pt.

## Visual verification
- **Full resolution and low-resolution proof** both inspected: no clipped or overlapping text, all intervals inside their axes, rows aligned across columns, both outcome headings fully visible.
- **Two earlier iterations were rejected:**
  1. The RD text was dropped because it sat outside the scale limits; switching to coordinate limits fixed this.
  2. The RD column was clipped by a 40 pt margin; it was widened to 85 pt.
