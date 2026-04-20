#' Run survey estimation for a given entity and set of reporting dimensions
#'
#' Computes weighted means and totals per hectare for all measures of a selected
#' entity, stratified or clustered according to the sampling design described in
#' the chain summary. Supports simple random sampling (SRS), stratified SRS, and
#' cluster sampling designs.
#'
#' @param .zip Named list produced by \code{fct_readzip2()}. Must contain:
#'   \describe{
#'     \item{chain_summary}{Survey chain metadata (sampling strategy, base unit,
#'       result variables, language, etc.).}
#'     \item{schema_summary}{Data dictionary describing all input dimensions.}
#'     \item{report_dimensions}{Available reporting dimensions.}
#'     \item{MAU_<entity>}{Minimal Area Unit tables for the target entity.}
#'   }
#' @param .entity Character scalar. Name of the entity to analyse (e.g.
#'   \code{"tree"}, \code{"plot"}).
#' @param .dim Character vector. Names of the reporting dimensions to include
#'   (e.g. \code{c("plot_forest_type", "plot_province")}).
#' @param .cm Compute mode, One of `"fast"` or `"safe"`. `"fast"` computes all
#'   measures in a single survey summary call; `"safe"` computes each measure
#'   separately and keeps partial results when some measures fail. Defaults to
#'   `"fast"`.
#' @param .pb_ss A Shiny session for [shinyWidgets::updateProgressBar()].
#'   Default `NULL`.
#' @param .pb_id The widget ID for [shinyWidgets::updateProgressBar()].
#'   Default `NULL`.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{MEANS}{Tibble of per-hectare survey means with standard errors and
#'       confidence intervals for each measure × dimension combination.}
#'     \item{TOTALS}{Tibble of estimated totals derived from \code{MEANS} by
#'       multiplying by expansion area.}
#'   }
#'   Both tibbles include \code{area}, \code{item_count},
#'   \code{base_unit_count}, and optionally \code{cluster_count} columns.
#'
#' @importFrom rlang .data
#'
#' @export
fct_arenalyse <- function(.zip, .entity, .dim, .cm = c("fast", "safe"), .pb_ss = NULL, .pb_id = NULL) {

  ## !!! FOR TESTING ONLY
  # .zip <- fct_readzip2(.path = "inst/extdata/OLAP_Shiny_demo.zip")$data
  # .zip <- fct_readzip2(.path = "~/syncwork/FAO-2026/support/support-arenalytics/MAU_Shiny_(png_nfi_2024_upperplant) 2.zip")$data
  # .entity <- .zip$chain_summary$analysis$entity
  # .dim <- .zip$chain_summary$analysis$dimensions
  # .dim
  # .dim <- "cluster_forest_type"
  # .dim <- "stratum_calc"
  # .cm = "fast" ; .pb_ss = NULL ; .pb_id = NULL
  ## !!!

  ## ++ ##
  log_step <- function(text, value = NULL) {
    message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text))

    if (!is.null(value) && !is.null(.pb_ss) && !is.null(.pb_id)) {
      shinyWidgets::updateProgressBar(
        session = .pb_ss,
        id = .pb_id,
        value = round(value)
      )
    }
  }

  ## 0. Coerce inputs ------
  log_step(paste0("Preparing analysis for entity '", .entity, "'."), value = 5)
  .cm <- match.arg(.cm)

  .zip$chain_summary$resultVariables <- tibble::as_tibble(
    .zip$chain_summary$resultVariables
  )
  .zip$schema_summary    <- tibble::as_tibble(.zip$schema_summary)
  .zip$report_dimensions <- tibble::as_tibble(.zip$report_dimensions)

  chain          <- .zip$chain_summary
  # label_language <- paste0("label_", chain$selectedLanguage)
  # clevel         <- chain$analysis$pValue

  entity_tblname <- stringr::str_subset(names(.zip), .entity)
  entity_prefix <- stringr::str_remove(entity_tblname, .entity)

  ## Get entity data
  wt <- .zip[[entity_tblname]] |> tibble::as_tibble()

  ## Get entity columns metadata
  wt_names <- fct_varinfo(
    .zip = .zip, .entity = .entity, .entity_prefix = entity_prefix
  )
  log_step("Entity metadata loaded.", value = 15)

  old_survey_opt <- options(
    survey.ultimate.cluster = FALSE,
    survey.adjust.domain.lonely = TRUE,
    survey.lonely.psu = "adjust"
  )
  on.exit(options(old_survey_opt), add = TRUE)
  ## ++ ##

  ## 3. Analysis configuration ------
  base_uuid    <- paste0(chain$baseUnit, "_uuid")
  cluster_uuid <- if (nchar(chain$clusteringEntity) > 0) {
    paste0(chain$clusteringEntity, "_uuid")
  } else {
    ""
  }
  use_cluster  <- cluster_uuid != ""

  all_dim_names <- wt_names |>
    dplyr::filter(.data$report_type == "dimension") |>
    dplyr::pull("name")
  measures <- wt_names |>
    dplyr::filter(.data$report_type == "measure") |>
    dplyr::pull("name")

  strat_attr_raw <- if (is.null(chain$stratumAttribute)) {
    ""
  } else {
    chain$stratumAttribute
  }
  use_strat <- chain$samplingStrategy %in% c(3L, 4L)
  strat_col <- if (use_strat) strat_attr_raw else ""

  dims_at_bu <- wt_names |>
    dplyr::filter(.data$name %in% .dim, .data$dimension_baseunit == TRUE) |>
    dplyr::pull("name")

  ## Without base-unit dims, stratification has no effect on this analysis
  if (length(dims_at_bu) == 0) {
    use_strat <- FALSE
    strat_col <- ""
  }

  dims          <- .dim
  strat_in_dims <- FALSE

  if (use_strat) {
    strat_in_dims <- strat_col %in% dims
    dims          <- unique(c(dims, strat_col))
    dims_at_bu    <- unique(c(dims_at_bu, strat_col))
  }

  all_at_bu <- length(dims) == length(dims_at_bu)

  ## 4. Filter & cast analysis data ------
  ## OLAP_baseunit_total flags which rows belong to this aggregation level
  ## !!! Now mau_baseunit_total, renaming column for backward compatibility
  if ("OLAP_baseunit_total" %in% names(wt)) {
    wt <- wt |> dplyr::rename(mau_baseunit_total = "OLAP_baseunit_total")
  }

  ## ++ ##
  log_step("Filtering MAU rows and preparing analysis table.", value = 25)
  ## ++ ##

  df_data <- wt |>
    dplyr::filter(.data$mau_baseunit_total == all_at_bu) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(all_dim_names), as.character)) |>
    dplyr::select(-"mau_baseunit_total")

  ## 5. Build df_total ------
  if (all_at_bu) {

    ## Simple case: all reported dimensions are at base-unit level or above.
    df_total <- df_data |>
      dplyr::filter(.data$weight > 0) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(measures), ~tidyr::replace_na(.x, 0)),
        dplyr::across(dplyr::any_of(dims),     ~tidyr::replace_na(.x, "NoData"))
      )

  } else {

    ## Complex case: at least one dimension is below base-unit level (e.g.
    ## tree_species). tidyr::complete() generates all base-unit x sub-unit
    ### combinations, then base-unit attributes (weight, exp_factor_, base-unit
    ## dims) are rejoined.

    dims_subunit     <- setdiff(dims, dims_at_bu)
    dims_to_complete <- unique(c(base_uuid, dims_subunit))
    names_to_rejoin  <- dims_at_bu  # drop before complete(), rejoin after

    if (use_strat) {
      df_data <- df_data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(strat_col), ~tidyr::replace_na(.x, "NoData")
          )
        )
    }

    ## Expand all sub-unit x base-unit combinations
    df_data <- if (use_strat) {
      df_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(strat_col))) |>
        tidyr::complete(!!!rlang::syms(dims_to_complete)) |>
        dplyr::ungroup()
    } else {
      df_data |> tidyr::complete(!!!rlang::syms(dims_to_complete))
    }

    df_data <- df_data |>
      dplyr::mutate(entity_count_ = tidyr::replace_na(.data$entity_count_, 0L))

    ## Recover base-unit attributes for each base_uuid (one row per base unit)
    bu_attrs <- df_data |>
      dplyr::filter(!is.na(.data$exp_factor_)) |>
      dplyr::distinct(!!rlang::sym(base_uuid), .keep_all = TRUE) |>
      dplyr::select(
        dplyr::all_of(base_uuid),
        dplyr::all_of(names_to_rejoin),
        "weight", "exp_factor_"
      )

    df_data <- df_data |>
      dplyr::select(
        -"weight", -"exp_factor_", -dplyr::all_of(names_to_rejoin)
      ) |>
      dplyr::left_join(bu_attrs, by = base_uuid) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(measures), ~tidyr::replace_na(.x, 0))
      )

    ## Aggregate to base-unit x dimension groups
    bu_core <- df_data |>
      dplyr::select(dplyr::all_of(base_uuid), "weight", "exp_factor_") |>
      dplyr::distinct()

    df_total <- df_data |>
      dplyr::filter(.data$weight > 0) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(unique(c(base_uuid, dims))))
      ) |>
      dplyr::summarise(
        entity_count_ = max(.data$entity_count_),
        dplyr::across(dplyr::all_of(measures), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::left_join(bu_core, by = base_uuid)

    if (use_cluster) {
      df_total <- df_total |>
        dplyr::left_join(
          dplyr::distinct(
            df_data, dplyr::across(dplyr::all_of(c(base_uuid, cluster_uuid)))
          ),
          by = base_uuid
        )
    }
  }

  ## Drop zero-area strata (area set to 0 ha)
  df_total <- df_total |> dplyr::filter(.data$exp_factor_ != 0)
  ## ++ ##
  log_step("Analysis table assembled.", value = 40)
  ## ++ ##

  ## 6. Per-hectare values ------
  df_mean <- df_total |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(measures), ~ .x / .data$exp_factor_)
    )

  if (use_strat) {
    df_mean <- df_mean |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(strat_col), ~tidyr::replace_na(.x, ""))
      )
  }

  ## 7. Survey design ------
  ## !! on a string injects the column name; !!NULL is treated as no argument
  ids_val    <- if (use_cluster) cluster_uuid else NULL
  strata_val <- if (use_strat)   strat_col    else NULL

  design <- df_mean |>
    srvyr::as_survey_design(
      ids       = !!ids_val,
      strata    = !!strata_val,
      weights   = "exp_factor_",
      nest      = TRUE,
      variables = dplyr::all_of(c(dims, measures, "exp_factor_"))
    )
  ## ++ ##
  log_step("Survey design created.", value = 50)
  ## ++ ##

  ## Remove stratum from reported dimensions if not in the original .dim request
  if (use_strat && !strat_in_dims) {
    dims       <- setdiff(dims, strat_col)
    dims_at_bu <- setdiff(dims_at_bu, strat_col)
  }

  ## 8. Expansion areas ------
  if (length(dims_at_bu) == 0) {
    ## Single scalar: sum across all unique base units
    total_area <- df_total |>
      dplyr::select(dplyr::all_of(c(base_uuid, "exp_factor_"))) |>
      dplyr::distinct() |>
      dplyr::pull("exp_factor_") |>
      sum()
  } else {
    ## Area by each combination of base-unit dimensions
    areas_df <- df_total |>
      tidyr::unite(
        "JOIN_COL", dplyr::all_of(dims_at_bu), sep = "**", remove = FALSE,
        na.rm = FALSE
      ) |>
      dplyr::select(dplyr::all_of(base_uuid), "JOIN_COL", "exp_factor_") |>
      dplyr::distinct() |>
      dplyr::summarise(area_ = sum(.data$exp_factor_), .by = "JOIN_COL")
  }

  ## 9. Survey estimation ------
  ## !!! TESTING MAP OVER MEASURES TO INC PROGRESS BAR
  t0 <- Sys.time()

  if (.cm == "fast") { 
    log_step("Computing survey means in fast mode.", value = 50)
    Sys.sleep(0.1)
    log_step("Wait! Executing the {survey} package...", value = 50)
    
    out_mean <- design |>
      dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::any_of(measures),
          .fns  = list(~srvyr::survey_mean(
            .x, na.rm = FALSE, vartype = c("se", "ci"),
            proportion = FALSE, level = chain$analysis$pValue, df = Inf
          ))
        )
      ) |>
      dplyr::rename_with(
        ~stringr::str_replace(.x, "_1$", "_1_"), dplyr::ends_with("_1")
      )

    ## Add test for NULL or NA in all measures (unlikely)
    check_na <- out_mean |> dplyr::select(dplyr::where(~all(is.na(.))))
    check_na <- ncol(check_na) == 4 * length(measures)

    check_null <- ncol(out_mean) == length(dims)

    if (check_na | check_null) stop("All measures NA or NULL") 

  } else {
    read_errors <- character(0)
    measure_n <- length(measures)

    log_step("Computing survey means in safe mode.", value = 50)
    Sys.sleep(0.1)
    log_step("Wait! Executing the {survey} package...", value = 50)

    ## ++ ##
    out_mean <- purrr::imap(measures, function(m, idx) {
      tt <- tryCatch(
        {
          design |>
            dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
            dplyr::summarise(
              dplyr::across(
                .cols = dplyr::any_of(m),
                .fns  = list(~srvyr::survey_mean(
                  .x, na.rm = FALSE, vartype = c("se", "ci"),
                  proportion = FALSE, level = chain$analysis$pValue, df = Inf
                ))
              )
            ) |>
            ## srvyr suffix: _1 -> _1_ (placeholder, resolved in step 11)
            dplyr::rename_with(
              ~stringr::str_replace(.x, "_1$", "_1_"), dplyr::ends_with("_1")
            )
        },
        warning = function(w) {
          msg <- conditionMessage(w)
          message(sprintf(
            "[%s] WARNING computing %s \u2014 %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m, msg
          ))
          read_errors[[m]] <<- msg
          NULL
        },
        error = function(e) {
          msg <- conditionMessage(e)
          message(sprintf(
            "[%s] ERROR computing %s \u2014 %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m, msg
          ))
          read_errors[[m]] <<- msg
          NULL
        }
      )

      if (!is.null(tt)) {
        log_step(
          paste0("Processed measure ", idx, "/", measure_n, ": ", m, "."),
          value = 50 + (idx / max(measure_n, 1)) * 35
        )
      }

      tt
    })
    ## ++ ##

    out_mean <- purrr::compact(out_mean)

    if (length(out_mean) == 0) {
      stop("No measures were successfully computed. Check warnings and errors for details.")
    }

    out_mean <- purrr::reduce(out_mean, dplyr::left_join, by = dims)

  } ## End IF out_mean calc

  log_step(
    paste(
      "survey_mean() completed in: ",
      round(difftime(Sys.time(), t0, units = "secs"), 1), "s"
    ),
    value = 85
  )

  ## 10. Join expansion areas ------
  if (length(dims_at_bu) == 0) {
    out_mean <- out_mean |> dplyr::mutate(area_ = total_area)
  } else {
    out_mean <- out_mean |>
      tidyr::unite(
        "JOIN_COL", dplyr::all_of(dims_at_bu), sep = "**", remove = FALSE,
        na.rm = FALSE
      ) |>
      dplyr::left_join(areas_df, by = "JOIN_COL")
  }

  ## 11. Compute totals ------
  ## Strip _1_ infix (srvyr list naming artefact) then trim trailing _
  tidy_srvyr_names <- function(df) {
    df |>
      dplyr::rename_with(
        ~stringr::str_replace(.x, "_1_", "_"), dplyr::contains("_1_")
      ) |>
      dplyr::rename_with(
        ~stringr::str_sub(.x, end = -2),        dplyr::ends_with("_")
      )
  }

  out_total <- out_mean |>
    dplyr::mutate(dplyr::across(dplyr::contains("_1_"), ~ .x * .data$area_)) |>
    tidy_srvyr_names()

  out_mean <- tidy_srvyr_names(out_mean)
  ## ++ ##
  log_step("Joined expansion areas and computing totals.", value = 90)
  ## ++ ##

  ## 12. Polish outputs ------
  polish <- function(df) {
    df |>
      ## "" instead of NA for character dimension columns
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(.x, ""))
      ) |>
      ## Negative lower CI is a modelling artefact; floor at zero
      dplyr::mutate(
        dplyr::across(dplyr::ends_with("_low"), ~dplyr::if_else(.x < 0, 0, .x))
      )
  }

  out_mean  <- polish(out_mean)
  out_total <- polish(out_total)

  ## 13. PSU / SSU / item counts ------
  if (length(dims_at_bu) == 0) {

    ## Scalar counts broadcast to all output rows
    n_bu    <- dplyr::n_distinct(df_total[[base_uuid]])
    n_items <- sum(df_total$entity_count_)

    out_mean  <- out_mean |>
      dplyr::mutate(base_unit_count = n_bu, item_count = n_items)
    out_total <- out_total |>
      dplyr::mutate(base_unit_count = n_bu, item_count = n_items)

    if (use_cluster) {
      n_cluster <- dplyr::n_distinct(df_total[[cluster_uuid]])
      out_mean  <- out_mean  |> dplyr::mutate(cluster_count = n_cluster)
      out_total <- out_total |> dplyr::mutate(cluster_count = n_cluster)
    }

  } else {

    ## Reuse JOIN_COL already present in out_mean / out_total from step 10
    add_join_col <- function(df) {
      tidyr::unite(df, "JOIN_COL", dplyr::all_of(dims_at_bu),
                   sep = "**", remove = FALSE, na.rm = FALSE)
    }

    psu_counts <- df_total |>
      add_join_col() |>
      dplyr::select(dplyr::all_of(c(base_uuid, "JOIN_COL"))) |>
      dplyr::distinct() |>
      dplyr::summarise(base_unit_count = dplyr::n(), .by = "JOIN_COL")

    item_counts <- df_total |>
      add_join_col() |>
      dplyr::summarise(item_count = sum(.data$entity_count_), .by = "JOIN_COL")

    psu_counts <- dplyr::left_join(item_counts, psu_counts, by = "JOIN_COL")

    if (use_cluster) {
      cluster_counts <- df_total |>
        add_join_col() |>
        dplyr::select(dplyr::all_of(c(cluster_uuid, "JOIN_COL"))) |>
        dplyr::distinct() |>
        dplyr::summarise(cluster_count = dplyr::n(), .by = "JOIN_COL")

      psu_counts <- psu_counts |>
        dplyr::left_join(cluster_counts, by = "JOIN_COL")
    }

    out_mean  <- dplyr::left_join(out_mean,  psu_counts, by = "JOIN_COL")
    out_total <- dplyr::left_join(out_total, psu_counts, by = "JOIN_COL")
  }

  ## ++ ##
  log_step("Polished outputs - analysis completed.", value = 100)
  list(MEANS = out_mean, TOTALS = out_total)
  ## ++ ##
}
