#' Get classified dimension metadata for a given entity
#'
#' Extracts and classifies all dimension columns from the OLAP table for a
#' given entity. Mirrors the column-metadata steps of \code{fct_arenalyse()},
#' making the result available independently for use in UI selectors.
#'
#' @param .zip Named list produced by \code{fct_readzip2()}.
#' @param .entity Character scalar. Entity name (e.g. \code{"tree"}).
#'
#' @return A tibble with one row per dimension column, containing:
#'   \code{name}, \code{label}, \code{parentEntity}, \code{type},
#'   \code{categoryName}, \code{source}, \code{dimension_baseunit},
#'   \code{stratum}.
#'
#' @importFrom rlang .data
#'
#' @noRd
fct_get_dim_meta <- function(.zip, .entity) {

  chain          <- .zip$chain_summary
  label_language <- paste0("label_", chain$selectedLanguage)
  strat_attr_raw <- if (is.null(chain$stratumAttribute)) "" else chain$stratumAttribute

  ## OLAP column names (no need to load the full table)
  wt_cols <- names(.zip[[paste0("OLAP_", .entity)]])

  ## Result variables: code dimensions + measures from chain
  rv_meta <- tibble::as_tibble(chain$resultVariables) |>
    dplyr::select("name", "type", "categoryName", parentEntity = "entity", "label") |>
    dplyr::mutate(
      report_type = dplyr::if_else(.data$type == "Q", "measure", "dimension"),
      type        = dplyr::if_else(.data$type == "Q", "numeric", "code"),
      source      = "chain"
    )

  ## Schema summary: input dimensions
  ss_meta <- tibble::as_tibble(.zip$schema_summary) |>
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

  ## Merge against actual OLAP column names (same coalesce logic as fct_arenalyse)
  tibble::tibble(name = wt_cols) |>
    dplyr::left_join(ss_meta,   by = "name") |>
    dplyr::left_join(rv_meta,   by = "name", suffix = c("", "_rv")) |>
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
      dimension_baseunit = dplyr::if_else(.data$name == "weight", NA, .data$dimension_baseunit),
      report_type        = dplyr::if_else(.data$name == "weight", NA_character_, .data$report_type),
      stratum            = strat_attr_raw != "" & .data$name == strat_attr_raw,
      ## Fallback: use column name when label is missing
      label              = dplyr::if_else(is.na(.data$label) | .data$label == "", .data$name, .data$label)
    ) |>
    dplyr::filter(.data$report_type == "dimension")
}
