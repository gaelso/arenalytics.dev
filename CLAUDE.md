# CLAUDE.md — arenalytics.dev

Context file for AI assistants collaborating on this repository.

---

## What this project is

**arenalytics** is an R package that ships a Shiny dashboard. Its purpose is to let
National Forest Inventory (NFI) managers derive weighted statistics from field
measurement data collected in **Open Foris Arena**, without needing to write code.

Users upload a pre-processed ZIP file exported by the Arena OLAP processing chain,
choose which entities and combinations of reporting dimensions to analyse, and the app
computes survey-weighted means and totals on the fly using the `{survey}` / `{srvyr}`
packages.

The live app is deployed at:
`https://openforis-shiny.shinyapps.io/arenalytics/`

This repo (`arenalytics.dev`) is the development fork. The canonical package lives at
`https://github.com/openforis/arenalytics`.

---

## Domain concepts

These terms appear throughout the code and must be understood to reason about it correctly.

| Term | Meaning |
|------|---------|
| **NFI** | National Forest Inventory — statistical survey of a country's forests. |
| **Open Foris Arena** | FAO's open-source survey platform. Arenalytics consumes its output. |
| **MAU** | Minimum Area Unit — the spatial unit at which field data is aggregated (e.g. sample plot). The ZIP contains one `MAU_<entity>.csv` per entity. |
| **Entity** | A measured object type: `tree`, `plot`, `shrub`, etc. Each has its own MAU table. |
| **Base unit** | The primary sampling unit (`baseUnit` in `chain_summary`, typically `plot` or `cluster`). Its UUID column is `<baseUnit>_uuid`. |
| **Base-unit dimension** | A dimension attribute recorded at base-unit level (e.g. `plot_forest_type`). Stratification is only meaningful over these. |
| **Sub-unit dimension** | A dimension attribute recorded below base-unit level (e.g. `tree_species` within a plot). Triggers a `tidyr::complete()` expansion step. |
| **PSU / SSU** | Primary / Secondary Sampling Unit — survey design terminology. |
| **Lonely PSU** | A stratum with only one PSU — causes variance estimation issues. Handled via `survey.lonely.psu` option (`"adjust"` or `"remove"`). |
| **Expansion factor** (`exp_factor_`) | Area represented by each MAU row (hectares). Used as the survey weight and to convert per-ha means to totals. |
| **Chain summary** | `chain_summary.json` — the metadata file in every ZIP. Contains sampling strategy, base unit, clustering entity, result variables, language, and analysis parameters. |
| **Sampling strategy** | Integer code in `chain$samplingStrategy`: 3 or 4 = stratified; others = simple random. |
| **OLAP prefix** | Older ZIPs used `OLAP_` as the entity table prefix; current convention is `MAU_`. Backward-compat code in `fct_arenalyse.R` and `build_area_result()` renames the old `OLAP_baseunit_total` column. |

---

## Package structure

```
arenalytics.dev/
├── R/
│   ├── fct_arenalyse.R        # Core survey estimation engine (~600 lines)
│   ├── fct_checkzip.R         # ZIP integrity validation
│   ├── fct_readzip.R          # Legacy ZIP reader (superseded, kept for compat)
│   ├── fct_readzip2.R         # Active ZIP reader — fault-tolerant, returns errors
│   ├── fct_varinfo.R          # Column metadata extractor for one entity
│   ├── fct_find_label.R       # Code → label lookup utility
│   ├── fct_mean.R             # Trivial weighted mean helper
│   ├── mod_home_UI.R          # Home page UI module
│   ├── mod_home_server.R      # Home page server (navigation events only)
│   ├── mod_tool_UI2.R         # Main tool UI (~515 lines)
│   ├── mod_tool_server2.R     # Main tool server (~1305 lines, largest file)
│   ├── mod_about_UI.R         # About page UI (download example ZIP buttons)
│   ├── mod_about_server.R     # About page server (download handlers)
│   ├── shiny_run_arenalytics_dev.R  # App entry point, UI + server + theme
│   ├── utils.R                # Almost entirely commented dead code — ignore
│   ├── utils-tr.R             # Translation key constants for i18n
│   └── zzz.R                  # .onLoad / .onUnload hooks (resource paths)
├── inst/
│   ├── assets/                # CSS, JS, images, favicons, translations.json
│   ├── extdata/               # Demo ZIPs (good + broken) for testing
│   └── quarto/                # analysis-report.qmd report template
├── tests/
│   ├── dev-fct/               # Manual dev scripts — not a formal test suite
│   └── *.R                    # Ad-hoc test scripts (check-data, tuto-pkg, etc.)
└── app.R                      # shinyapps.io deployment entry point
```

---

## Data flow

```
User uploads ZIP
      │
      ▼
fct_checkzip()          Validates required files are present
      │
      ▼
fct_readzip2()          Reads all files from ZIP into a named list (rv$inputs)
      │                 On success, also calls fct_varinfo() for each entity
      │                 → returns list(data = ..., errors = ..., var_meta = ...)
      │
      ▼
fct_varinfo()           Merges schema_summary + chain resultVariables
      │                 Classifies each column: dimension vs measure,
      │                 base-unit vs sub-unit, stratum flag, category type
      │                 → returns a tibble (rv$inputs$var_meta[[entity]])
      │
      ▼
fct_arenalyse()         Survey estimation for selected entity + dims
      │                 1. Filter rows to the relevant aggregation level
      │                 2. If sub-unit dims: expand with tidyr::complete()
      │                 3. Build srvyr survey design
      │                 4. Compute survey_mean() per group
      │                 5. Join expansion areas, compute totals
      │                 → returns list(MEANS = tibble, TOTALS = tibble)
      │
      ▼
replace_dim_labels()    Replace category codes with human-readable labels
      │
      ▼
UI: table (DT) + bar plots (ggplot2) + downloadable report (Quarto)
```

---

## Reactive values structure (`rv`)

Passed between all modules. Defined in `shiny_run_arenalytics_dev.R`.

```r
rv$inputs$path_zip      # Path to uploaded ZIP
rv$inputs$check_zip     # Result of fct_checkzip()
rv$inputs$data          # Named list from fct_readzip2()$data
rv$inputs$errors        # Character vector from fct_readzip2()$errors
rv$inputs$var_meta      # Named list of fct_varinfo() tibbles, one per entity
rv$inputs$data_ok       # TRUE when data loaded with no errors

rv$insights$entities        # Entity names (e.g. c("tree", "plot"))
rv$insights$entities_labs   # Human-readable labels
rv$insights$entities_named  # Named vector: label = name

rv$analysis$result          # list(MEANS, TOTALS) from fct_arenalyse()
rv$analysis$dim_meta        # fct_varinfo() result for the active entity
rv$analysis$measures_meta   # tibble of measure names + labels
rv$analysis$dims            # Character vector of selected dimension names
rv$analysis$entity          # Active entity name
rv$analysis$mode            # "area" or anything else (other measures)
rv$analysis$strat_label     # Label of the stratification attribute, or NULL

rv$actions$to_tool          # Triggers navigation to Tool tab
rv$actions$to_about         # Triggers navigation to About tab
```

---

## Key functions — quick reference

### `fct_readzip2(.path, .pb_ss, .pb_id, .entity_prefix = "MAU_")`
Reads every file in the ZIP inside `tryCatch()`. Returns
`list(data, errors, var_meta)`. `data` is a named list keyed by snake_case
filenames. File names are converted from camelCase (`SchemaSummary` →
`schema_summary`) but the entity prefix is preserved (`MAU_tree`, not
`mau_tree`).

### `fct_varinfo(.zip, .entity, .entity_prefix = "MAU_")`
Produces the column metadata tibble for one entity. Columns of interest:
`name`, `label`, `report_type` (`"dimension"` | `"measure"` | `NA`),
`dimension_baseunit` (`TRUE` | `FALSE` | `NA`), `categoryName`, `categoryType`
(`"F"` flat | `"H"` hierarchical), `stratum` (logical).

### `fct_arenalyse(.zip, .entity, .dim, .cm, .lonely, .pb_ss, .pb_id)`
`.cm = "fast"` — single `summarise()` call across all measures (faster, fails all if one errors).  
`.cm = "safe"` — `purrr::imap()` over measures, `tryCatch()` per measure (partial results on error).  
`.lonely = "adjust"` (default) — grand-mean substitution for lonely PSUs (conservative, recommended).  
`.lonely = "remove"` — drops lonely strata from variance (may underestimate SE).

### `fct_checkzip(.path, .entity_prefix)`
Returns a list with `has_chain`, `has_schema`, `has_dimensions`, `has_categories`,
`has_taxonomies`, `has_OLAPentities`, `all_ok`, `missing`.

---

## Shiny module conventions

- All modules follow the `mod_<name>_UI()` / `mod_<name>_server()` pattern.
- `id` is always namespaced via `ns <- NS(id)` (UI) and `session$ns` (server).
- `rv` (reactive values) is the shared communication object, passed as an argument.
- UI functions also receive `i18n` (translator object) and `.tr` (key list from `utils-tr.R`).
- The `"2"` suffix in `mod_tool_UI2` and `mod_tool_server2` is historical — these are
  the active versions; no `mod_tool_UI.R` or `mod_tool_server.R` exist.

---

## i18n

- Translation strings live in `inst/assets/translations.json`.
- Keys are defined as a list in `R/utils-tr.R` via `.tr_keys()`.
- All translatable strings are called with `i18n$t(.tr$<key>)`.
- Supported languages: `en`, `fr`, `sp` (Spanish).
- Many UI strings in `mod_tool_server2.R` (analysis panel, filter labels, plot
  axis titles) are **not yet i18n-translated** — they use hardcoded English strings.

---

## Entity table prefix

The ZIP entity tables are named `MAU_<entity>.csv`. The prefix `"MAU_"` is:
- The default value of `.entity_prefix` in `fct_readzip2`, `fct_varinfo`, `fct_checkzip`, `fct_arenalyse`.
- Hardcoded as `.ep <- "MAU_"` in `mod_tool_server2.R` (line 13).
- Older ZIPs used `OLAP_` — backward-compat code handles the renamed
  `OLAP_baseunit_total` column in `fct_arenalyse.R` and `build_area_result()`.

---

## Coding conventions

- **Tidyverse throughout**: prefer `dplyr`, `tidyr`, `purrr`, `stringr`. Use
  `purrr::map()` family instead of `apply()` / `lapply()` / `sapply()`.
- **NSE**: use `rlang::sym()`, `!!`, `!!!`, `:=`, `.data$` (imported via
  `@importFrom rlang .data :=`). `rlang::.data` is the standard pronoun throughout.
- **Pipe**: `|>` (base R native pipe, R ≥ 4.1). No `%>%` from magrittr.
- **`%||%`**: null-coalescing operator from `rlang`, used throughout.
- **Parameter naming**: function parameters use the `.param` convention (leading dot)
  to distinguish them from local variables.
- **Section markers**: `## ++ ##` marks actively maintained code blocks.
  `## $$$ ##` or `## $$$` marks recently added or changed blocks. `## !!! ... !!!`
  marks temporary testing code that should be removed before merging.
- **Progress bar**: both `fct_readzip2` and `fct_arenalyse` accept `.pb_ss` (Shiny
  session) and `.pb_id` (widget ID) to update a `shinyWidgets::progressBar` during
  computation. Pass `NULL` (defaults) when running outside Shiny.
- **`Sys.sleep(0.1)`**: appears intentionally in `fct_readzip2` (inside the file loop)
  and in `fct_arenalyse` (before heavy computation). These yield control so Shiny can
  flush the progress bar and console div before blocking. Do not remove them.

---

## Known tech debt / open issues

| Issue | Location | Notes |
|-------|----------|-------|
| `utils.R` is dead code | `R/utils.R` | 100% commented-out `fct_get_dim_meta()` — near-duplicate of `fct_varinfo.R`. Can be deleted. |
| `fct_readzip.R` is legacy | `R/fct_readzip.R` | Superseded by `fct_readzip2.R`. Still exported. Should be deprecated or removed from NAMESPACE. |
| `DESCRIPTION` is placeholder | `DESCRIPTION` | Title, description, and author fields contain generic boilerplate. |
| `app.R` / footer text | `shiny_run_arenalytics_dev.R:100-103` | Footer still has `"Your Name"` / `"XYZ Institute"` placeholder text. |
| `rv$ct` unused | `shiny_run_arenalytics_dev.R:205` | `ct = reactiveValues()` is created but never read or written. |
| i18n incomplete | `mod_tool_server2.R` | Analysis panel, filter labels, plot axis titles, and table column headers use hardcoded English strings rather than `i18n$t()` keys. |
| No formal test suite | `tests/` | The `tests/` directory contains manual dev scripts, not `testthat` unit tests. `R CMD check` passes only because no formal tests are expected. |
| `categoryNameOld` column | `fct_varinfo.R:113` | Created as a diagnostic holdover but never used downstream. Safe to remove. |
| Input coercion duplicated | `fct_arenalyse.R:84-88` and `fct_varinfo.R:29-31` | Both coerce the same `.zip` sub-elements to tibbles. Could be done once in `fct_readzip2`. |

---

## Development workflow

```r
# Daily cycle
devtools::load_all()
shiny_run_arenalytics_dev()

# Before committing
devtools::document()   # regenerates NAMESPACE and man/
devtools::check()      # full R CMD check

# Environment variable needed for check
Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = 0)
```

Test ZIPs are in `inst/extdata/`:
- `OLAP_Shiny_demo.zip` — valid demo file (also served from the About page download)
- `OLAP_Shiny_demo_broken.zip` — missing required files (tests error handling)
- `OLAP_Shiny_demo_corrupted.zip` — structurally corrupt (tests `fct_readzip2` fault tolerance)

---

## Things to avoid

- Do not use `apply()`, `lapply()`, `sapply()`, `vapply()` — use `purrr::map()` variants.
- Do not add `dplyr::select()` calls that drop columns silently without `dplyr::any_of()` / `dplyr::all_of()` — the MAU table schema varies between surveys.
- Do not call `fct_varinfo()` with a `.zip` whose sub-elements have not been coerced to tibbles — the function assumes `chain_summary$resultVariables` is a tibble. Either call after `fct_readzip2()` or coerce first.
- Do not bypass `on.exit(options(old_survey_opt), ...)` in `fct_arenalyse.R` — the `survey.*` global options must be restored even if the function errors, to avoid leaking state into other Shiny sessions.
- Do not hardcode the entity prefix `"MAU_"` in new code — use the `.entity_prefix` parameter or `rv$inputs$entity_prefix` (once that field is added to `rv$inputs`).
- Do not remove the `Sys.sleep(0.1)` calls before the survey computation — they are intentional UI yield points (see Coding conventions above).
