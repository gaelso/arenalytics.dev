# `arenalytics.dev` — AI Agent Context

> Last updated: 2026-04-16  
> Purpose: Technical briefing for an AI agent acting as expert R / Shiny developer continuing work on this package.

---

## What the Package Does

`arenalytics.dev` is an R package that ships a Shiny dashboard for analysing ecological/forestry survey data produced by **OpenForis Arena** — a field data collection and processing platform used mainly in national forest inventories (NFIs).

Workflow:
1. A user runs a processing chain in Arena, which outputs a ZIP file of semi-aggregated OLAP data.
2. This package reads that ZIP, displays data insights (dimension frequency tables, measure summaries), and computes design-based statistical estimates (survey means per ha and totals) using `srvyr`.

---

## Repository Structure

```
arenalytics.dev/
├── R/
│   ├── shiny_run_arenalytics_dev.R   # App entry point (exported function)
│   ├── fct_checkzip.R                # Validate ZIP before reading
│   ├── fct_readzip2.R                # Read and parse the ZIP (main loader)
│   ├── fct_varinfo.R                 # Extract & classify column metadata per entity
│   ├── fct_arenalyse.R               # Survey estimation (means + totals)
│   ├── fct_mean.R                    # Small utility for value box rendering
│   ├── fct_readzip.R                 # Old flat-return loader (superseded, kept for reference)
│   ├── mod_tool_UI2.R                # ACTIVE tool module UI
│   ├── mod_tool_server2.R            # ACTIVE tool module server
│   ├── mod_tool_UI.R / server.R      # Older versions (kept for reference, NOT wired in)
│   ├── mod_home_UI/server.R          # Landing page module
│   ├── mod_about_UI/server.R         # About page + demo file download
│   ├── utils.R                       # fct_get_dim_meta() — fully commented out, superseded
│   └── utils-tr.R                    # i18n key registry (.tr_keys())
├── inst/
│   ├── extdata/
│   │   ├── OLAP_Shiny_demo.zip       # Demo dataset (uses OLAP_ prefix, for dev/testing only)
│   │   └── OLAP_Shiny_demo_broken.zip
│   └── assets/                       # CSS, JS, logo, favicon, translations.json
├── tests/
├── app.R                             # Thin wrapper: pkgload::load_all() + run
└── DESCRIPTION
```

Only `mod_tool_UI2` / `mod_tool_server2` are wired into the live app. The `*UI.R` / `*server.R` (no suffix `2`) are legacy and can be ignored.

---

## The ZIP Data Format

The input is a ZIP produced by an Arena processing chain.

| File | Format | Description |
|---|---|---|
| `MAU_<entity>.csv` | CSV | Wide-format OLAP table per entity (e.g. `MAU_tree`, `MAU_bamboo`) |
| `chain_summary.json` | JSON | Survey metadata: sampling strategy, base unit, clustering entity, stratum attribute, result variable definitions, selected language |
| `SchemaSummary.csv` | CSV | Data dictionary: all input dimension columns with types, labels, category names |
| `ReportDimensions.csv` | CSV | Which dimensions are available for reporting per entity |
| `categories.rds` | RDS | Named list of category tables; each maps `code → label` + per-language columns (e.g. `label_en`) |
| `taxonomies.rds` | RDS | Taxon lookup tables for species dimensions |

**Entity prefix**: production ZIPs use `MAU_` prefix; the bundled demo uses `OLAP_`. The prefix is stored in `.ep <- "MAU_"` at the top of `mod_tool_server2` and threaded through the entire pipeline via the `.entity_prefix` argument.

---

## Core Functions

### `fct_checkzip(.path, .entity_prefix)`

Validates ZIP structure without reading data. Returns `list(all_ok = TRUE/FALSE, missing = character())`. Gates the "Read data" button in the UI.

---

### `fct_readzip2(.path, .pb_session, .pb_id, .entity_prefix = "MAU_")`

Main data loader. Reads every file inside the ZIP individually in `tryCatch`, emitting timestamped `message()` calls captured by the Shiny progress console div via `withCallingHandlers`.

**Name normalisation**: file names are lowercased and camelCase-split, then the entity prefix is re-capitalised:
```r
file_names <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", file_names))
file_names <- stringr::str_replace_all(file_names, tolower(.entity_prefix), .entity_prefix)
# e.g. "MAU_tree.csv" → "mau_tree" → "MAU_tree"
```

**Returns**:
```r
list(
  data = list(              # flat named list keyed by normalised file names
    chain_summary    = <list>,        # parsed JSON
    schema_summary   = <data.frame>,
    report_dimensions = <data.frame>,
    categories       = <list of data.frames>,
    taxonomies       = <list>,
    MAU_tree         = <data.frame>,  # OLAP entity table
    MAU_bamboo       = <data.frame>,
    ...
  ),
  errors   = character(0),  # named vector: file → error message
  var_meta = list(          # pre-computed fct_varinfo() result per entity
    tree   = <tibble>,
    bamboo = <tibble>,
    ...
  )
)
```

> **Note**: if any file read errors occurred, `var_meta` is `NULL`.

---

### `fct_varinfo(.zip, .entity, .entity_prefix = "MAU_")`

Called with the **inner flat data** (`rv$inputs$data`). Builds a tibble describing every column of the entity OLAP table by merging `schema_summary` (input dims) with `chain_summary$resultVariables` (computed dims + measures), then filtering to columns that actually appear in the OLAP table.

**Returns a tibble with one row per column:**

| Column | Description |
|---|---|
| `name` | Column name in the OLAP table |
| `label` | Human-readable label (language-aware) |
| `report_type` | `"dimension"` or `"measure"` (NA for weight/internal cols) |
| `type` | `"code"`, `"numeric"`, etc. |
| `categoryName` | Key into `categories` list for code→label lookup |
| `parentEntity` | Entity the column belongs to |
| `dimension_baseunit` | `TRUE` = base-unit level; `FALSE` = sub-unit dim (e.g. tree species is sub-unit of plot) |
| `stratum` | `TRUE` if this column is the stratification variable |
| `categoryType` | `"F"` flat or `"H"` hierarchical (square brackets in `categoryName`) |

Also called by `fct_readzip2` to pre-populate `var_meta` at load time, and by `fct_arenalyse` at analysis time.

---

### `fct_arenalyse(.zip, .entity, .dim)`

Survey estimation engine. Called with the **inner flat data** (`rv$inputs$data`).

**Steps:**
1. Calls `fct_varinfo` internally to get column metadata and dynamically detect the entity prefix.
2. Reads sampling design from `chain_summary$samplingStrategy`: simple SRS (1–2), stratified SRS (3–4), or cluster.
3. Classifies user-selected dims as base-unit vs sub-unit.
4. Expands base-unit × sub-unit combinations with `tidyr::complete()` when sub-unit dims are included.
5. Builds a `srvyr` survey design with `ids` (cluster UUID if clustered), `strata` (stratum column if stratified), `weights` (`exp_factor_`).
6. Computes `survey_mean()` with SE + CI for each measure via `purrr::map` (one measure at a time).
7. Joins expansion areas (`exp_factor_` sum per base-unit dim combination) via a `JOIN_COL` key.
8. Derives totals by multiplying means × area.
9. Cleans column names (strips `srvyr` `_1_` artefact suffix), floors negative CI lower bounds at zero.

**Returns** `list(MEANS = <tibble>, TOTALS = <tibble>)`.

Output columns: dimension columns, `JOIN_COL` (pipe-separated base-unit dim values), `<measure>`, `<measure>_se`, `<measure>_low`, `<measure>_upp`, `area`, `base_unit_count`, `item_count`, optionally `cluster_count`.

> **Important**: all measure columns in the raw OLAP table are stored as **character**. `fct_arenalyse` casts them to numeric internally. The server also calls `as.numeric()` when computing insight summaries.

---

## Shiny App Architecture

### Entry Point

`shiny_run_arenalytics_dev()` in `R/shiny_run_arenalytics_dev.R`. UI and server are defined inside this single function (golem-inspired inline structure). `app.R` at repo root calls `pkgload::load_all()` then this function, enabling RStudio's Run App button.

### Top-Level Layout

`bslib::page_navbar()` with Bootstrap 5 / Bootswatch **Yeti** theme. Three nav panels: **Home**, **Tool**, **About**. A `shinyWidgets::pickerInput` language selector (EN/FR/ES) in the navbar drives `shiny.i18n` translations.

### Reactive Values (`rv`)

A nested `reactiveValues` structure passed to every module server:

```r
rv <- reactiveValues(
  inputs   = reactiveValues(),
  # After fct_readzip2(): rv$inputs$data     = inner flat data list
  #                        rv$inputs$var_meta = pre-computed fct_varinfo per entity
  #                        rv$inputs$errors   = read error vector
  #                        rv$inputs$data_ok  = TRUE/FALSE
  #                        rv$inputs$path_zip = uploaded file path

  insights = reactiveValues(),
  # rv$insights$entities_named  = setNames(entities, entities_labs)
  # rv$insights$bu_choices       = setNames(name, label) for base-unit dims
  # rv$insights$sub_choices      = setNames(name, label) for sub-unit dims
  # rv$insights$meas_choices     = setNames(name, label) for measures
  # rv$insights$entity_table     = rv$inputs$data[[paste0(.ep, entity)]]

  analysis = reactiveValues(),
  # rv$analysis$dim_meta        = fct_varinfo result for selected entity
  # rv$analysis$strat_label     = label of stratum column (or NULL)
  # rv$analysis$measures_meta   = dim_meta filtered to report_type == "measure"
  # rv$analysis$result          = list(MEANS, TOTALS) after label replacement
  # rv$analysis$dims            = character vector of selected dim column names
  # rv$analysis$entity          = selected entity name (e.g. "tree")

  ct       = reactiveValues(),   # crosstalk test state (demo only, dead code)
  actions  = reactiveValues()    # cross-module navigation: to_tool, to_about
)
```

### Custom JS Handlers

Two scripts in `inst/assets/`:
- `js_activate_tab.js` — Shiny message handler `"activate-tab"`: programmatically switches the active tab (`session$sendCustomMessage("activate-tab", list(id=ns("tool_tabs"), value="tab_analysis"))`).
- `js_handlers.js` — `"scroll_top"` handler.

---

## Tool Module — Detailed

### UI (`mod_tool_UI2`)

`navset_card_tab` with a 300px `sidebar` and two main panels.

**Sidebar accordions:**

- **Acc1 "Load ZIP file"** (`ac1`): `fileInput` → validation message → "Read data" `actionButton`.
- ~~Acc2~~ — removed; entity/variable selection moved into the Insights panel.
- **Acc3 "Run analysis"** (`ac3`):
  - `uiOutput("analysis_entity")` — `selectInput` populated server-side from entity list
  - `uiOutput("analysis_dims")` — two `checkboxGroupButtons(individual=TRUE, size="sm")` (base-unit dims then `hr()` then sub-unit dims), rendered server-side
  - `uiOutput("analysis_strat_text")` — italic `text-info` note when a stratum is auto-included
  - `uiOutput("analysis_too_many_dims")` — italic `text-warning` note when > 4 dims selected
  - `actionButton("btn_run_analysis")` — disabled until ≥ 1 dim checked

**Panel: Insights** (`tab_insights`):

Three states managed by `shinyjs::show/hide`: initial message → progress (progressBar + live console div) → data insights.

Data insights layout:
- `selectInput("insight_sel_entity")` for entity selection
- Three `card()` elements using `layout_columns(col_widths=c(6,6))`:
  - **Base-unit dims**: left = `checkboxGroupButtons("insight_bu_sel", individual=TRUE)`, right = `uiOutput("insight_bu_out")`
  - **Sub-unit dims**: same pattern with `"insight_sub_sel"` / `"insight_sub_out"`
  - **Measures**: same pattern with `"insight_meas_sel"` / `"insight_meas_out"`

**Panel: Analysis** (`tab_analysis`):

- Plot controls card: `selectInput` for x-axis dim, measure, fill, facet + error bar `checkboxInput`; dynamic `virtualSelectInput` filters per dimension (`uiOutput("analysis_extra_filters")`)
- Two `plotOutput`s: "Means (per ha)" and "Totals"

---

### Server (`mod_tool_server2`)

**Key constant at the top:**
```r
.ep <- "MAU_"   # entity table prefix in the ZIP
```

**Dev/test setup** (lines 16–27 in the server, commented out):
```r
# rv$inputs <- fct_readzip2(.path = "...", .entity_prefix = .ep)
# input <- list(analysis_sel_entity = "tree", ...)
# result <- fct_arenalyse(.zip = rv$inputs$data, .entity = "tree", .dim = c(...))
```
Run these lines in the R console to set up `rv` and `input` for interactive debugging without launching the app.

**Data loading sequence:**
1. `observeEvent(input$load_zip)` → `fct_checkzip()` → toggle messages/button
2. `observeEvent(input$btn_read_data)` → `fct_readzip2()` captured with `withCallingHandlers` → `rv$inputs <- <result>` → set `rv$inputs$data_ok`
3. `observeEvent(input$btn_data_insights)` → show insights panel
4. `observe({ req(rv$inputs$data) })` → populate `rv$insights$entities_named` from `names(rv$inputs$data) |> str_subset(.ep) |> str_remove(.ep)`

**Analysis sequence:**
1. `observeEvent(input$analysis_sel_entity)` → `rv$analysis$dim_meta <- rv$inputs$var_meta[[entity]]`, detect stratum label
2. `output$analysis_dims` (renderUI) → two `checkboxGroupButtons` from `dim_meta`
3. `observe` → toggle run button when `isTruthy(input$analysis_bu_dims) || isTruthy(input$analysis_sub_dims)`
4. `observeEvent(input$btn_run_analysis)`:
   - `dims_sel <- c(input$analysis_bu_dims, input$analysis_sub_dims)`
   - `fct_arenalyse(.zip = rv$inputs$data, .entity = ..., .dim = dims_sel)`
   - **Apply `replace_dim_labels()`** to both `result$MEANS` and `result$TOTALS` before storing
   - Store in `rv$analysis$result`, `rv$analysis$dims`, `rv$analysis$entity`

**Local helper functions defined inside `moduleServer`:**

- **`replace_dim_labels(df, dim_meta, categories, lang)`**: replaces dimension codes with human-readable labels. Iterates dimension columns present in `df` via `purrr::reduce`; looks up `categoryName` from `dim_meta`, finds the matching table in `categories`, maps `code → label_<lang>` (falls back to `label`). Applied to `MEANS` and `TOTALS` right after `fct_arenalyse()` returns.

- **`make_dim_summary(sel, choices, tbl)`**: renders a `tags$pre()` with `table()` output per selected dimension. Uses `options(width = 60)` temporarily to force paired label/value block wrapping (console style).

- **`make_meas_summary(sel, choices, tbl)`**: renders a `tags$pre()` with `summary(as.numeric(...))` per selected measure.

- **`make_bar_plot(df, x_dim, measure, fill_col, facet_col, show_errbar, dim_meta, measures_meta, extra_filter_vals, comma_y)`**: shared ggplot2 bar chart builder. Handles optional fill (dodged bars), optional facet, optional CI error bars, dimension filters, and comma-formatted y-axis for totals.

**Insight summary outputs** (`output$insight_bu_out`, `output$insight_sub_out`, `output$insight_meas_out`): each is `renderUI` gated on `req(rv$insights$<choices>, rv$insights$entity_table)`.

**Analysis plot selectors**: `observeEvent(rv$analysis$result)` updates `plot_dim`, `plot_measure`, `plot_fill`, `plot_facet` selectors and reveals the results div.

**Dimension filters** (`output$analysis_extra_filters`): one `virtualSelectInput` per dim in `rv$analysis$dims`, defaulting all values selected (no filter applied). Filter values are label strings post `replace_dim_labels()`.

---

## Key Architectural Notes and Known Issues

1. **`rv$inputs` vs `rv$inputs$data`**: After loading, `rv$inputs` is the full `fct_readzip2()` return value (`list(data, errors, var_meta)`). So `rv$inputs$data` is the inner flat list with `MAU_tree` etc. Always access entity tables as `rv$inputs$data[[paste0(.ep, entity)]]` and metadata as `rv$inputs$var_meta[[entity]]`.

2. **`fct_arenalyse` data path**: called with `.zip = rv$inputs$data` (the flat inner list). It dynamically detects the entity prefix via `stringr::str_subset(names(.zip), .entity)` — this is why it works regardless of prefix.

3. **Measures stored as character**: all measure columns in OLAP tables are character. Cast to numeric before any arithmetic (`as.numeric()`). `fct_arenalyse` handles this internally; `make_meas_summary` does it explicitly.

4. **Dual module files**: `mod_tool_UI.R` / `mod_tool_server.R` are legacy, not wired in. `mod_tool_UI2.R` / `mod_tool_server2.R` are the active versions.

5. **`fct_get_dim_meta`** in `utils.R` is fully commented out — superseded by `fct_varinfo`.

6. **i18n**: `shiny.i18n` drives UI text via `i18n$t(.tr$key)`. Keys centralised in `utils-tr.R → .tr_keys()`. Translation JSON at `inst/assets/translations.json`. Only UI-level text is translated; server-generated strings are mostly English.

7. **`%||%` operator**: used for NULL fallbacks (e.g. `lang <- rv$inputs$data$chain_summary$selectedLanguage %||% "en"`). Comes from `rlang`, imported via `@importFrom rlang .data` in the NAMESPACE.

8. **Demo ZIP vs production ZIP**: `OLAP_Shiny_demo.zip` uses `OLAP_` prefix (lowercased to `olap_` then not re-capitalised since `.ep = "MAU_"`). This means `var_meta` ends up empty when testing with the demo ZIP in an actual running app (the server sets `.ep <- "MAU_"`). Use the test setup lines in the server comments, pointing to a real MAU ZIP, for full end-to-end testing.

9. **Dead code — crosstalk panel**: the old UI had an `ac4` accordion and a crosstalk nav panel (d3scatter linked plots). These have been removed from the `UI2`/`server2` active files. Some crosstalk output code may remain commented out in the server.

---

## Dependencies (key)

| Package | Role |
|---|---|
| `bslib` | Bootstrap 5 layout, theming, cards, sidebars |
| `shinyWidgets` | `checkboxGroupButtons`, `virtualSelectInput`, `progressBar`, `pickerInput`, `sendSweetAlert` |
| `shinyjs` | `show/hide/toggle`, `enable/disable`, DOM manipulation |
| `srvyr` | Survey-weighted estimation (`as_survey_design`, `survey_mean`) |
| `ggplot2` | All plots in the analysis panel |
| `purrr` | `map`, `reduce`, `list_c` throughout |
| `dplyr` / `tidyr` | Data wrangling in all functions |
| `stringr` | String manipulation (entity prefix handling, name normalisation) |
| `jsonlite` | Parse `chain_summary.json` |
| `zip` | List and extract ZIP archive contents |
| `shiny.i18n` | EN/FR/ES translations |
| `scales` | Comma-formatted y-axis labels for totals plots |
