#' Get classified dimension metadata for a given entity
#'
#' Extracts and classifies all dimension columns from the OLAP table for a
#' given entity. Mirrors the column-metadata steps of \code{fct_arenalyse()},
#' making the result available independently for use in UI selectors.
#'
#' @param .zip Named list produced by \code{fct_readzip2()}.
#' @param .entity Character scalar. Entity name (e.g. \code{"tree"}).
#' @param .entity_prefix A character describing how OpenForis Arena prefix entity
#'   tables. Default "MAU_".
#'
#' @return A tibble with one row per dimension column, containing:
#'   \code{name}, \code{label}, \code{parentEntity}, \code{type},
#'   \code{categoryName}, \code{source}, \code{dimension_baseunit},
#'   \code{stratum}, \code{categoryType}.
#'
#' @importFrom rlang .data
#'
#' @noRd
fct_varinfo <- function(.zip, .entity, .entity_prefix = "MAU_"){

  ## !!! FOR TESTING ONLY
  # .zip <- fct_readzip2(.path = "inst/extdata/OLAP_Shiny_demo.zip") ; names(.zip)
  # .entity <- .zip$chain_summary$analysis$entity
  # .entity_prefix = "OLAP_"
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
    dplyr::mutate(wide_table = paste0(.entity_prefix, .data$entity)) |>
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

  ## Filter out dims with no data
  report_dims <- .zip$report_dimensions |>
    dplyr::filter(.data$entity == .entity) |>
    dplyr::pull(dimension)

  wt_names <- wt_names |>
    dplyr::filter((.data$report_type == "dimension" & .data$name %in% report_dims) | .data$report_type != "dimension")

  wt_names
}
