# Code Redundancy Notes — `src/` R files

_Date: 2026-07-24 · Branch: `dev`_

Audit of the ~75 `.R` files under `src/` (~24,000 lines) for duplicated files,
repeated function definitions, and copy-pasted code blocks. **Nothing was
changed** — this is documentation only. Companion to `CLEANUP_NOTES.md`.

## TL;DR

- **`src/misc_files/` is largely superseded** — it holds older, standalone copies
  of functions that were later consolidated into the themed dirs
  (`water_supp_functions/`, `ece_functions/`, `aridity_gradient_functions/`).
  Each themed file is a strict superset of its misc counterpart.
- **The dangerous cases are DIVERGED duplicates**, not identical ones: several
  misc-files copies use *different model formulas* and *omit the bonferroni
  adjustment*, so running them produces **different statistical results** than the
  themed versions. See the ⚠️ rows below.
- **Utility functions are defined in 3–5 places each** (`assump`, `att_coef`,
  `aud_range`, `max_sat`) with subtle signature/behavior differences.
- **Boilerplate is copy-pasted**: a 10-line `source()` header appears verbatim in
  3 scripts; the same ~8-library stack is re-declared in 15–20 files.
- **Within-file repetition**: filename-parsing blocks and dotplot builders are
  pasted multiple times inside single files.

Legend: **IDENTICAL** = byte/whitespace-only diff · **SUPERSEDED** = older but
functionally similar; themed copy is newer/more complete · **⚠️ DIVERGED** =
meaningfully different logic (results can differ).

---

## 1. Duplicate files (same basename, multiple locations)

| File | Copies | Status |
|---|---|---|
| `Atmospheric_sound_attenuation.R` | `abm_scripts/`, `misc_files/` | **IDENTICAL** (only CRLF vs LF line endings). Both define `att_coef` + `aud_range`. |
| `regression_diagnostics.R` | `abm_scripts/`, `birdnet_analysis/`, `broad_scale_analysis/` | Three copies of the same `assump()` diagnostic, **⚠️ DIVERGED** in `par()` handling (see §2). |

Both function pairs are *also* defined in `src/functions.R`, so the standalone
files are fully redundant with the central utility file.

---

## 2. Utility functions defined in multiple files

| Function | Locations | Verdict |
|---|---|---|
| `att_coef`, `aud_range` | `functions.R:17/44`, `abm_scripts/Atmospheric_sound_attenuation.R:3/28`, `misc_files/Atmospheric_sound_attenuation.R:3/28` | **⚠️ DIVERGED signatures** — math bodies identical, but `functions.R`'s `att_coef` has **no default for `Pa`** (commented out) so a bare call errors, and its `aud_range` has defaults `dbOri=85, dbMin=30` while the other two have none. Same call can succeed in one, error in another. |
| `assump` | `functions.R:132`, `abm_scripts/regression_diagnostics.R:12`, `broad_scale_analysis/regression_diagnostics.R:12`, `birdnet_analysis/regression_diagnostics.R:12`, `broad_scale_analysis/acoustic_analysis.R:567` | **⚠️ DIVERGED — 3 behavioral variants.** (A) no `par()` grid, resets nothing: `functions.R`, `broad_scale_analysis/regression_diagnostics.R`, `birdnet_analysis/regression_diagnostics.R`. (B) sets `par(mfrow=c(2,2))` **and resets** it: `abm_scripts/regression_diagnostics.R`. (C) param renamed `x`→`model`, sets grid but **never resets** (leaks `par` state to caller): `acoustic_analysis.R:567`. |
| `max_sat` | `functions.R:116`, `weather_calcs/mesonet_weather_calc.R:26` | **IDENTICAL** (`0.0039*exp(0.0656*temp)`). |

---

## 3. `misc_files/` vs themed function dirs (older standalone copies)

Every function below has a newer home in a themed dir. The themed file also
contains *many additional* consolidated functions the misc file lacks.

### 3a. Aridity gradient
| Function | Old copy | Canonical | Verdict |
|---|---|---|---|
| `aridity_contrasts_mas` | `misc_files/aridity_contrasts_mas.R` | `aridity_gradient_functions/aridity_gradient_mas_functions.R:2` | **SUPERSEDED** — identical through L63; themed adds `adjust="bonferroni"`, a confint table, and `summary(emm)` return. |

### 3b. Water supplementation — CBMA
| Function | Old copy | Canonical (`water_supp_functions/cbma_water_functions.R`) | Verdict |
|---|---|---|---|
| `cbma_water_contrasts` | `misc_files/cbma_water_contrasts.R` | `:3` | **⚠️ DIVERGED** — model `pc ~ ws_site*water*gh*mas_bin + date` (old) vs `... *gh + mas_bin + date` (new); old has no bonferroni + raw contrast labels. |
| `cbma_water_table` | `misc_files/cbma_water_table.R` | `:66` | **SUPERSEDED** — old hardcodes a recode + `select(-arid_within)`. |
| `cbma_water_table2` | `misc_files/cbma_water_table.R` | `:110` | **⚠️ DIVERGED** — new subsets rows `[c(1:8,10),]` and re-groups. |

### 3c. Water supplementation — SSWMA
| Function | Old copy | Canonical (`water_supp_functions/sswma_water_functions.R`) | Verdict |
|---|---|---|---|
| `sswma_water_contrasts` | `misc_files/sswma_water_contrasts.R` | `:6` | **⚠️ DIVERGED** — old `emmeans(...)` on `pc`; new switches to `emtrends(..., var="xvar")`, new formula, adds bonferroni. Signature changed `(data,pc)`→`(data,yvar,xvar)`. |
| `sswma_water_table` | `misc_files/sswma_water_table.R` (+ inline in `sswma_water_contrasts.R`) | `:41` | **SUPERSEDED** — two older label variants; new is the cleaned one. |
| `sswma_water_table2` | `misc_files/sswma_water_table.R` | `:84` | **⚠️ DIVERGED** — new subsets `[1:16,]`, drops a row group. |

### 3d. Extreme climate events (ECE)
| Function | Old copy | Canonical (`ece_functions/ece_functions.R`) | Verdict |
|---|---|---|---|
| `ece_contrast_mas` | `misc_files/ece_contrast_mas.R` | `:5` | **⚠️ DIVERGED** — old `lm(pc ~ site + scale(date))` / `emmeans`; new `lm(yvar ~ site*xvar + mas_bin + scale(date))` / `emtrends` + bonferroni. |
| `ece_table` | `misc_files/ece_table.R` | `:163` | **IDENTICAL** (verbatim). |
| `ece_tables_combined` | `misc_files/ece_tables_combined.R` | `:209` | **SUPERSEDED** — differs only in `select(-mas_bin,...)` to account for the added column. |
| `ece_tables_combined2` | `misc_files/ece_tables_combined.R` | `:270` | **⚠️ DIVERGED** — new adds padding rows + `gt(groupname_col=...)`. |

### 3e. SSWMA water ECE
| Function | Old copy | Canonical (`ece_functions/sswma_water_ece_functions.R`) | Verdict |
|---|---|---|---|
| `sswma_water_impact` | `misc_files/sswma_water_impact.R` | `:122` | **⚠️ DIVERGED** — old hardcodes `data=sswmawl_thres`, `emmeans`; new is parameterized `(data,yvar,xvar)`, `emtrends` + bonferroni. |
| `sswma_water_impact_table` | `misc_files/sswma_water_impact.R` | `:152` | **IDENTICAL** (verbatim). |
| `sswma_ece_table` | `misc_files/sswma_water_impact.R` | `:249` | **⚠️ DIVERGED** — rewritten from explicit `rbind` per PC to a `for` loop over a `pcs` list. |

> **Recommendation:** treat the themed dirs as canonical and retire the
> `misc_files/` copies (move to `archive/` alongside the other superseded
> material — see `CLEANUP_NOTES.md`). Because several are ⚠️ DIVERGED, do **not**
> assume they are interchangeable; confirm the analysis scripts `source()` only
> the themed versions (they do — the top-level scripts source the themed dirs, not
> `misc_files/`).

---

## 4. Repeated boilerplate across scripts

**4a. Identical 10-line `source()` header** — verbatim in:
- `aru_stats_analysis.R:36-45`
- `extreme_stats_analysis.R:26-35`
- `species_per_site.R:39-48`

(loads `functions.R` + the four themed dirs + `evap_rate_function.R` +
`inflection_points.R`). Candidate for a single `src/_setup.R` that these scripts
source once.

**4b. Re-declared library stacks** — the same core packages are `library()`-ed in
15–20 separate files:

| package | # files | | package | # files |
|---|---|---|---|---|
| zoo | 20 | | car | 18 |
| lubridate | 20 | | lmerTest | 17 |
| tidyverse | 18 | | ggplot2 | 15 |
| lme4 | 18 | | dplyr | 15 |

`aru_stats_analysis.R` alone has 30 `library()` calls. A shared
`src/_libraries.R` (or `pacman::p_load(...)`) sourced at the top would remove most
of this.

---

## 5. Large-script overlap

| Pair | Overlap | Notes |
|---|---|---|
| `broad_scale_analysis/acoustic_analysis.R` vs `acoustic_analysis_apply.R` | **Low (~40–50 verbatim lines)** | Evolved rewrite, not a copy. The `_apply` version generalizes the per-index processing. |
| `aru_stats_analysis.R` vs `extreme_stats_analysis.R` | **High (~100–150 lines copy-pasted)** | `extreme_stats_analysis.R` is a trimmed offshoot sharing the entire preprocessing/PCA front-end verbatim: the water-window `mutate(water = ifelse(date >= "2021-06-04" ...))` logic, the `dplyr::filter` date chains, and the PCA sign-flip + 4× `ggbiplot(...)` block. Models then diverge. |

**Within-file repetition:** in both `acoustic_analysis*.R` the filename→date
parser block is pasted ~4× per file (one per index aci/adi/aei/bio):
```r
adi_results$day    = as.factor(substr(adi_results$filename, 7, 8))
adi_results$hour   = ...; $min; $month; $second; $year; $date_time = ...
```
A single helper `parse_aru_filename(df)` would replace all copies.

---

## 6. Plot-function duplication

| Function | Locations | Verdict |
|---|---|---|
| `dotplot` | `misc_files/dot_plot_func.R:8` (fuller: adds `ggtitle`, `geom_line`, `base_size=10`) vs `misc_files/multi_plot_func.R:2` (simpler, `base_size=20`) | **⚠️ DIVERGED duplicate (~70% identical).** If both are sourced, the last one wins silently. |
| `sswma_dotplot` vs `cbma_dotplot` | both in `water_supp_functions/water_supp_plots.R:62` and `:144` | **Near-duplicate pair (~90% identical)** — differ only in `hide.ns`, `tip.length`, `guides()`/`legend.key.size`. Could be one function with a site/param argument. |

---

## 7. `misc_files/` one-off "save-as-and-tweak" families

Within each family, 60–90% of lines are shared verbatim; siblings differ by a
single parameter, level count, or output format.

- **Aridity table builders:** `aridity_table.R` ≈ `aridity_table_mas.R` (~90%
  identical; differ in `select(-arid_within)` vs `-mas_bin` and row-group labels).
  `aridity_table_csv.R` and `aridity_table_mas_horizontal.R` share the same four
  predawn/early/mid/late sub-table blocks (~40 verbatim lines); the `_csv` one
  stops at `cbind`, the other adds `gt()`.
- **Aridity contrast runners:** `aridity_contrasts.R` (lm) ≈
  `aridity_contrasts_lmer.R` (lmer) — only the model line differs.
  `aridity_contrasts_mas.R` ≈ `aridity_contrasts_mas2.R` — differ only in scale
  (5 levels / 30 contrasts vs 3 levels / 18 contrasts).
- **ECE contrast runners:** `ece_contrast_mas.R` ≈ `ece_contrast_mas2.R` — `_mas2`
  just drops the LWMA reference level and its 3 contrasts.

These are prime candidates for a single parameterized function (e.g. pass the
model formula, level count, and output format as arguments).

---

## 8. Dead code (already flagged in CLEANUP_NOTES.md)

- `src/graveyard.R` (1,052 lines) — explicitly a code graveyard; contains a
  hardcoded Windows `setwd()` and loose plotting/analysis snippets that duplicate
  fragments living in the maintained scripts.

---

## Suggested remediation order (lowest risk → highest value)

1. **Archive `misc_files/`** superseded copies (§3, §7) — none are sourced by the
   pipeline, so this is safe once confirmed. Keeps history via `archive/`.
2. **Consolidate utility functions** (§2) into `src/functions.R` as the single
   source of truth; delete the standalone `Atmospheric_sound_attenuation.R` /
   `regression_diagnostics.R` copies. Resolve the ⚠️ signature/`par()` divergences
   deliberately (pick one behavior).
3. **Extract shared setup** (§4): one `src/_libraries.R` + `src/_setup.R` sourced
   by the top-level scripts.
4. **Factor out repeated blocks** (§5, §6): `parse_aru_filename()`, a single
   parameterized `dotplot()`, and one water-dotplot with a site argument.

All of the above are code changes (they edit `source()`/`library()`/function
bodies), so they belong in a dedicated refactor with re-run verification — not a
mechanical file move.
