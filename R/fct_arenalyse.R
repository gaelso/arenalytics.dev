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
#'     \item{OLAP_<entity>}{Wide-format OLAP table for the target entity.}
#'   }
#' @param .entity Character scalar. Name of the entity to analyse (e.g.
#'   \code{"tree"}, \code{"plot"}).
#' @param .dim Character vector. Names of the reporting dimensions to include
#'   (e.g. \code{c("plot_forest_type", "plot_province")}).
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{MEANS}{Tibble of per-hectare survey means with standard errors and
#'       confidence intervals for each measure × dimension combination.}
#'     \item{TOTALS}{Tibble of estimated totals derived from \code{MEANS} by
#'       multiplying by expansion area.}
#'   }
#'   Both tibbles include \code{area}, \code{item_count}, \code{base_unit_count},
#'   and optionally \code{cluster_count} columns.
#'
#' @importFrom rlang .data
#'
#' @export
fct_arenalyse <- function(.zip, .entity, .dim) {

  ## !!! FOR TESTING ONLY
  # .zip <- fct_readzip2(.path = "inst/extdata/OLAP_Shiny_demo.zip") ; names(.zip)
  # .entity <- .zip$chain_summary$analysis$entity
  # .dim <- .zip$chain_summary$analysis$dimensions
  # .dim
  # .dim <- c("tree_plant_type", "province", "stratum_calc", "dbh_up100_10cm", "cluster_land_use", "cluster_forest_type", "cluster_forest_status")
  # summary(.zip[[paste0("OLAP_", .entity)]])
  ## !!!

  ## 0. Coerce inputs ------
  .zip$chain_summary$resultVariables <- tibble::as_tibble(.zip$chain_summary$resultVariables)
  .zip$schema_summary    <- tibble::as_tibble(.zip$schema_summary)
  .zip$report_dimensions <- tibble::as_tibble(.zip$report_dimensions)

  chain          <- .zip$chain_summary
  label_language <- paste0("label_", chain$selectedLanguage)
  clevel         <- chain$analysis$pValue

  ## 1. Entity labels & wide table ------
  label_cols <- .zip$schema_summary |>
    dplyr::filter(.data$type == "entity") |>
    dplyr::select(entity = "name", label = dplyr::all_of(label_language))

  wt_filename <- chain$resultVariables |>
    dplyr::filter(.data$areaBased, .data$active) |>
    dplyr::select("entityPath", "entity") |>
    dplyr::distinct() |>
    dplyr::mutate(wide_table = paste0("OLAP_", .data$entity)) |>
    dplyr::left_join(label_cols, by = "entity") |>
    dplyr::filter(.data$entity == .entity) |>
    dplyr::pull("wide_table")

  wt <- tibble::as_tibble(.zip[[wt_filename]])

  ## 2. Column metadata ------
  ## Result variables: code dimensions + measures derived from chain
  rv_meta <- chain$resultVariables |>
    dplyr::select("name", "type", "categoryName", parentEntity = "entity", "label") |>
    dplyr::mutate(
      report_type = dplyr::if_else(.data$type == "Q", "measure", "dimension"),
      type        = dplyr::if_else(.data$type == "Q", "numeric", "code"),
      source      = "chain"
    )

  ## Schema summary: input dimensions
  ss_meta <- .zip$schema_summary |>
    dplyr::mutate(
      categoryName = dplyr::if_else(
        .data$taxonomyName != "", .data$taxonomyName, .data$categoryName
      )
    ) |>
    dplyr::select(
      "name", "type", "categoryName", "parentEntity",
      label = dplyr::all_of(label_language)
    ) |>
    dplyr::mutate(report_type = "dimension", source = "input")

  ## Merge: schema covers input dims, rv_meta covers code dims + measures.
  ## Fields don't overlap; suffix + coalesce handles the seam cleanly.
  wt_names <- tibble::tibble(name = names(wt)) |>
    dplyr::left_join(ss_meta, by = "name") |>
    dplyr::left_join(rv_meta, by = "name", suffix = c("", "_rv")) |>
    dplyr::mutate(
      type         = dplyr::coalesce(.data$type,                           .data$type_rv),
      categoryName = dplyr::coalesce(dplyr::na_if(.data$categoryName, ""), .data$categoryName_rv),
      parentEntity = dplyr::coalesce(dplyr::na_if(.data$parentEntity, ""), .data$parentEntity_rv),
      label        = dplyr::coalesce(dplyr::na_if(.data$label, ""),        .data$label_rv),
      report_type  = dplyr::coalesce(dplyr::na_if(.data$report_type, ""),  .data$report_type_rv),
      source       = dplyr::coalesce(dplyr::na_if(.data$source, ""),       .data$source_rv)
    ) |>
    dplyr::select(-dplyr::ends_with("_rv")) |>
    dplyr::mutate(
      dimension_baseunit = dplyr::if_else(.data$parentEntity == .entity, FALSE, TRUE),
      report_type        = dplyr::if_else(.data$name == "weight", NA_character_, .data$report_type)
    ) |>
    ## Separate mutate: dimension_baseunit references the value set above
    dplyr::mutate(
      dimension_baseunit = dplyr::if_else(.data$name == "weight", NA, .data$dimension_baseunit)
    )

  ## Stratum attribute tagging
  strat_attr_raw <- if (is.null(chain$stratumAttribute)) "" else chain$stratumAttribute
  wt_names <- wt_names |>
    dplyr::mutate(stratum = strat_attr_raw != "" & .data$name == strat_attr_raw)

  ## Category type: Flat (F) vs Hierarchical (H — square brackets in categoryName)
  wt_names <- wt_names |>
    dplyr::mutate(
      categoryNameOld = .data$categoryName,
      categoryType    = dplyr::if_else(
        stringr::str_detect(.data$categoryName, "(?<=\\[).*(?=\\])"), "H", "F"
      ),
      categoryName    = stringr::str_remove(.data$categoryName, "\\[.*")
    )

  ## 3. Analysis configuration ------
  base_uuid    <- paste0(chain$baseUnit, "_uuid")
  cluster_uuid <- if (nchar(chain$clusteringEntity) > 0) paste0(chain$clusteringEntity, "_uuid") else ""
  use_cluster  <- cluster_uuid != ""

  all_dim_names <- wt_names |> dplyr::filter(.data$report_type == "dimension") |> dplyr::pull("name")
  measures      <- wt_names |> dplyr::filter(.data$report_type == "measure")   |> dplyr::pull("name")

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
  df_data <- wt |>
    dplyr::filter(.data$OLAP_baseunit_total == all_at_bu) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(all_dim_names), as.character)) |>
    dplyr::select(-"OLAP_baseunit_total")

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

    ## Complex case: at least one dimension is below base-unit level (e.g. tree_species).
    ## tidyr::complete() generates all base-unit x sub-unit combinations,
    ## then base-unit attributes (weight, exp_factor_, base-unit dims) are rejoined.

    dims_subunit     <- setdiff(dims, dims_at_bu)
    dims_to_complete <- unique(c(base_uuid, dims_subunit))
    names_to_rejoin  <- dims_at_bu  # drop before complete(), rejoin after

    if (use_strat) {
      df_data <- df_data |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(strat_col), ~tidyr::replace_na(.x, "NoData"))
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
      dplyr::select(-"weight", -"exp_factor_", -dplyr::all_of(names_to_rejoin)) |>
      dplyr::left_join(bu_attrs, by = base_uuid) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(measures), ~tidyr::replace_na(.x, 0)))

    ## Aggregate to base-unit x dimension groups
    bu_core <- df_data |>
      dplyr::select(dplyr::all_of(base_uuid), "weight", "exp_factor_") |>
      dplyr::distinct()

    df_total <- df_data |>
      dplyr::filter(.data$weight > 0) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(base_uuid, dims))))) |>
      dplyr::summarise(
        entity_count_ = max(.data$entity_count_),
        dplyr::across(dplyr::all_of(measures), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::left_join(bu_core, by = base_uuid)

    if (use_cluster) {
      df_total <- df_total |>
        dplyr::left_join(
          dplyr::distinct(df_data, dplyr::across(dplyr::all_of(c(base_uuid, cluster_uuid)))),
          by = base_uuid
        )
    }
  }

  ## Drop zero-area strata (area set to 0 ha)
  df_total <- df_total |> dplyr::filter(.data$exp_factor_ != 0)

  ## 6. Per-hectare values ------
  df_mean <- df_total |>
    dplyr::mutate(dplyr::across(dplyr::all_of(measures), ~ .x / .data$exp_factor_))

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
      tidyr::unite("JOIN_COL", dplyr::all_of(dims_at_bu), sep = "**", remove = FALSE, na.rm = FALSE) |>
      dplyr::select(dplyr::all_of(base_uuid), "JOIN_COL", "exp_factor_") |>
      dplyr::distinct() |>
      dplyr::summarise(area_ = sum(.data$exp_factor_), .by = "JOIN_COL")
  }

  ## 9. Survey estimation ------
  # t0 <- Sys.time()
  #
  # out_mean_test <- design |>
  #   dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
  #   dplyr::summarise(
  #     dplyr::across(
  #       .cols = dplyr::any_of(measures),
  #       .fns  = list(~srvyr::survey_mean(
  #         .x, na.rm = FALSE, vartype = c("se", "ci"),
  #         proportion = FALSE, level = clevel, df = Inf
  #       ))
  #     )
  #   ) |>
  #   ## srvyr list-output suffix: _1 -> _1_ (placeholder, resolved in step 11)
  #   dplyr::rename_with(~stringr::str_replace(.x, "_1$", "_1_"), dplyr::ends_with("_1"))
  #
  # message("survey_mean(): ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

  ## !!! TESTING MAP OVER MEASURES TO INC PROGRESS BAR
  t0 <- Sys.time()

  read_errors <- character(0)

  out_mean <- purrr::map(measures, function(m){

    tt <- tryCatch(
      {
        design |>
          dplyr::group_by(dplyr::across(dplyr::all_of(dims))) |>
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::any_of(m),
              .fns  = list(~srvyr::survey_mean(
                .x, na.rm = FALSE, vartype = c("se", "ci"),
                proportion = FALSE, level = clevel, df = Inf
              ))
            )
          ) |>
          ## srvyr list-output suffix: _1 -> _1_ (placeholder, resolved in step 11)
          dplyr::rename_with(~stringr::str_replace(.x, "_1$", "_1_"), dplyr::ends_with("_1"))
      },
      warning = function(w) {
        msg <- conditionMessage(w)
        message(sprintf(
          "[%s] WARNING reading %s \u2014 %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m, msg
        ))
        read_errors[[m]] <<- msg  # <<- writes to the enclosing map() env
        NULL
        #invokeRestart("muffleWarning")  # log and resume; do not abort
      },
      error = function(e) {
        msg <- conditionMessage(e)
        message(sprintf(
          "[%s] ERROR reading %s \u2014 %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m, msg
        ))
        read_errors[[m]] <<- msg  # <<- writes to the enclosing map() env
        NULL
      }
    )

    ## Log success (only when the file was actually parsed)
    if (!is.null(tt)) {
      message(sprintf(
        "[%s] Computed %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m
      ))
    }

    ## Update progress bar when running inside Shiny
    # if (!is.null(.pb_session) && !is.null(.pb_id)) {
    #   shinyWidgets::updateProgressBar(
    #     session = .pb_session,
    #     id      = .pb_id,
    #     value   = round(pct)
    #   )
    # }
    #
    # Sys.sleep(0.1)

    tt

  }) |> purrr::reduce(dplyr::left_join, by = dims)

  message("survey_mean(): ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

  ## !!!

  ## 10. Join expansion areas ------
  if (length(dims_at_bu) == 0) {
    out_mean <- out_mean |> dplyr::mutate(area_ = total_area)
  } else {
    out_mean <- out_mean |>
      tidyr::unite("JOIN_COL", dplyr::all_of(dims_at_bu), sep = "**", remove = FALSE, na.rm = FALSE) |>
      dplyr::left_join(areas_df, by = "JOIN_COL")
  }

  ## 11. Compute totals ------
  ## Strip _1_ infix (srvyr list naming artefact) then trim trailing _
  tidy_srvyr_names <- function(df) {
    df |>
      dplyr::rename_with(~stringr::str_replace(.x, "_1_", "_"), dplyr::contains("_1_")) |>
      dplyr::rename_with(~stringr::str_sub(.x, end = -2),        dplyr::ends_with("_"))
  }

  out_total <- out_mean |>
    dplyr::mutate(dplyr::across(dplyr::contains("_1_"), ~ .x * .data$area_)) |>
    tidy_srvyr_names()

  out_mean <- tidy_srvyr_names(out_mean)

  ## 12. Polish outputs ------
  polish <- function(df) {
    df |>
      ## "" instead of NA for character dimension columns
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~tidyr::replace_na(.x, ""))) |>
      ## Negative lower CI is a modelling artefact; floor at zero
      dplyr::mutate(dplyr::across(dplyr::ends_with("_low"), ~dplyr::if_else(.x < 0, 0, .x)))
  }

  out_mean  <- polish(out_mean)
  out_total <- polish(out_total)

  ## 13. PSU / SSU / item counts ------
  if (length(dims_at_bu) == 0) {

    ## Scalar counts broadcast to all output rows
    n_bu    <- dplyr::n_distinct(df_total[[base_uuid]])
    n_items <- sum(df_total$entity_count_)

    out_mean  <- out_mean  |> dplyr::mutate(base_unit_count = n_bu, item_count = n_items)
    out_total <- out_total |> dplyr::mutate(base_unit_count = n_bu, item_count = n_items)

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

      psu_counts <- dplyr::left_join(psu_counts, cluster_counts, by = "JOIN_COL")
    }

    out_mean  <- dplyr::left_join(out_mean,  psu_counts, by = "JOIN_COL")
    out_total <- dplyr::left_join(out_total, psu_counts, by = "JOIN_COL")
  }

  list(MEANS = out_mean, TOTALS = out_total)
}
