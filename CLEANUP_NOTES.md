# Repository Cleanup Notes

_Date: 2026-07-24 · Branch: `dev`_

This document records the cleanup performed on `aru_sound_analysis` and lists
files that are unnecessary or worth revisiting. **No files were deleted.** Clutter
was moved into a new top-level `archive/` directory so it stays in version history
but is out of the way of the active analysis code.

## Important context: why so little was moved

Nearly every script in `src/` references other files by **hardcoded paths relative
to the project root**, e.g.:

```r
source("src/functions.R")
source("src/aridity_gradient_functions/aridity_gradient_mas_functions.R")
load("data_clean/aridity_data_clean.Rdata")
```

Because of this, physically reorganizing the `src/` scripts or the `data_clean/`
files would silently break `source()` and `load()`/`save()` calls across dozens of
scripts. The active analysis data is **already organized** under `data_clean/`
(with themed subfolders) and `src/` is already split into themed function
subdirectories. So cleanup was limited to relocating **duplicates, mislabeled
files, and junk that nothing references.** Deeper reorganization is left as a
documented recommendation below rather than done blindly.

---

## What was moved to `archive/`

### `archive/duplicate_data/`
| Archived file | Was at | Reason |
|---|---|---|
| `water_mesonet_weather.Rdata` | repo root | **Exact duplicate** (md5 match) of `data_clean/water_mesonet_weather.Rdata`. Scripts load the `data_clean/mesonet_data/` copy, not the root one. |
| `acoustic_and_birdnet_data.Rdata` | `src/acoustic_and_birdnet_data.R` | **Mislabeled binary** — it was an `.Rdata` file with a `.R` extension (2.1 MB of binary, not R code). **Exact duplicate** (md5 match) of `data_clean/acoustic_and_birdnet_data.Rdata`, which is what scripts actually load. Renamed to the correct extension in the archive. |
| `aridity_data_clean_OLD.Rdata` | `src/aridity_data_clean.Rdata` | An **older/different** version (md5 differs, smaller) of the canonical `data_clean/aridity_data_clean.Rdata` that scripts reference. Kept as a `_OLD` copy in case it is needed. |

### `archive/superseded_scripts/`
| Archived file | Was at | Reason |
|---|---|---|
| `aru_stats_analysis-copy.R` | `src/` | Older manual copy of `src/aru_stats_analysis.R` (1149 vs 1402 lines). Not `source()`d anywhere. |
| `acoustic_analysis-LAPTOP-M0CQMPUE.R` | `src/broad_scale_analysis/` | OneDrive/device **sync-conflict copy** of `acoustic_analysis.R`. Not `source()`d anywhere. |

### `archive/zip_archives/`
| Archived file | Was at | Reason |
|---|---|---|
| `evap_check.zip` | `src/` | 4.8 MB archive not referenced by any script. |
| `broad_scale_analysis.zip` | `src/misc_files/` | Zipped snapshot of scripts that already exist unzipped in `src/broad_scale_analysis/`. |

### `archive/libreoffice_lock_files/`
LibreOffice/office temporary lock files (`.~lock.*#`) that were accidentally
committed. These are pure junk and regenerate whenever a file is open in an office
app. **Recommend deleting these entirely** once confirmed unneeded.

- `.~lock.cbma_mesonet.csv#` (was `data_clean/mesonet_data/`)
- `.~lock.broad_acoustic_metrics_day_mas_bin.xlsx#` (was `results/`)
- `.~lock.casp_day_bin_results.xlsx#` (was `results/`)
- `.~lock.lasp_day_bin_results.xlsx#` (was `results/`)

`.gitignore` was updated to ignore these patterns going forward (see below).

---

## `.gitignore` additions

Added rules so this class of clutter is not re-committed:

```gitignore
*.WAV                 # uppercase audio (the lowercase *.wav was already ignored)
.~lock.*#             # LibreOffice/office lock files
*-LAPTOP-*            # OneDrive/device sync conflict copies
*-DESKTOP-*
*-copy.R              # manual editor copies
*.Rdata~
```

---

## Files NOT moved — flagged for your review

These are candidates for cleanup but were **left in place** because they may still
be wanted, are referenced by scripts, or moving them risks breaking paths. Decide
per-file.

### `src/misc_files/` — appears to be a mixed "junk drawer"
None of these are `source()`d by the analysis pipeline. Several look like they
belong to **other projects / tutorials** rather than this dissertation chapter:

- **Likely unrelated to this chapter** (song-clustering / tutorial material):
  `Budgie Clustering large.R`, `Budgie Clustering small.R`,
  `LBH Clustering large.R`, `LBH Clustering small.R`, `LBH field songs analysis.R`,
  `ohun_tutorial.R`, `warbleR_song_analysis.R`, `calculate acoustic space.R`,
  `acoustic_spaces.R`, `plot synthetic data results.R`,
  `run analysis on your data.R`, `Read me.docx`
- **Second, stray project file:** `machine_learning_analysis.Rproj` — a second
  `.Rproj` inside `src/misc_files/` (the real project file is
  `aru_sound_analysis.Rproj` at the root).
- **Large data file in a source folder:** `aci_all_water_sswma.csv` (2.5 MB) —
  data does not belong under `src/`; consider moving to `data_clean/` if still used.
- Remaining `.R` files here look like superseded one-off drafts of tables/contrasts
  that have "official" versions under `src/aridity_gradient_functions/`,
  `src/ece_functions/`, etc. Worth diffing before keeping.

> Suggested action: if you confirm `misc_files/` is scratch/other-project code,
> move the whole folder into `archive/` or a separate repo. Left untouched for now
> to avoid discarding anything you may still reference.

### Oddly-named result files in `results/`
- `casp_masbin_results.jpg.xlsx` — double extension; likely an `.xlsx` saved with a
  stray `.jpg` in the name. Rename to `casp_masbin_results.xlsx`.
- `noca_day_binn_results.xlsx` — likely typo ("binn" → "bin").

### `src/graveyard.R`
Explicitly dead code by name (contains a hardcoded Windows `setwd()` path). Kept
because it may be a personal snippet stash; consider moving to `archive/` if truly
unused.

### `data/20210518_110000.WAV` (11.5 MB)
A sample audio file. **Kept in place** — it is referenced by
`src/spectrogram_example.R`. Note it is a large binary in git; consider Git LFS if
the repo needs to stay lean. (`.gitignore` now ignores new `*.WAV`, but this one is
already tracked and was left tracked so the example keeps working.)

### Hardcoded absolute / Windows paths in scripts
Several scripts contain machine-specific paths (e.g.
`C:/Users/meely/OneDrive - University of Oklahoma/...` in
`src/abm_scripts/current_extreme.R` and `src/graveyard.R`, and commented
`setwd()` lines elsewhere). These are not a file-organization issue but will break
on other machines. Consider replacing with project-root-relative paths (e.g. via
the `here` package) in a future pass.

---

## Recommended (optional) future reorganization

If you later want a cleaner `src/` layout, do it as a deliberate refactor, because
paths are hardcoded:

1. Group the top-level analysis scripts (`aru_stats_analysis.R`,
   `extreme_stats_analysis.R`, `combine_audio_weather.R`, `species_per_site.R`,
   `bird_morphometrics.R`, `pca_plot.R`, etc.) under `src/analysis/`.
2. Keep the sourced **function** files where they are (or centralize under
   `src/functions/`) — but update every `source("src/...")` call accordingly.
3. Adopt the `here` package so paths resolve from the project root regardless of
   working directory.

Doing 1–3 requires editing the `source()`/`load()`/`save()` paths throughout the
scripts and re-running to verify — a code change, not just a file move, so it was
intentionally left out of this cleanup.
