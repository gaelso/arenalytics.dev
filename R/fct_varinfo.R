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
#'   \code{stratum}, \code{categoryType}.
#'
#' @importFrom rlang .data
#'
#' @noRd
fct_varinfo <- function(.zip, .entity){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # .zip <- fct_readzip2(.path = .path)$data ; names(.zip)
  # .entity = "tree"
  ## !!!

  ## 0. Coerce inputs ------
  schema <- tibble::as_tibble(.zip$schema_summary)
  resvar <- tibble::as_tibble(.zip$chain_summary$resultVariables)
  repdim <- tibble::as_tibble(.zip$report_dimensions)
  chain  <- .zip$chain_summary

  label_language <- paste0("label_", chain$selectedLanguage)

  entity_prefix <- if (any(stringr::str_detect(names(.zip), "OLAP_*"))) "OLAP_" else "MAU_"

  ## 1. Entity labels & wide table ------
  label_cols <- schema |>
    dplyr::filter(.data$type == "entity") |>
    dplyr::select(entity = "name", label = dplyr::all_of(label_language))

  wt_filename <- resvar |>
    dplyr::filter(.data$areaBased, .data$active) |>
    dplyr::select("entityPath", "entity") |>
    dplyr::distinct() |>
    dplyr::mutate(wide_table = paste0(entity_prefix, .data$entity)) |>
    dplyr::left_join(label_cols, by = "entity") |>
    dplyr::filter(.data$entity == .entity) |>
    dplyr::pull("wide_table")

  wt <- tibble::as_tibble(.zip[[wt_filename]])

  ## 2. Column metadata ------
  ## Result variables: code dimensions + measures derived from chain
  rv_meta <- resvar |>
    dplyr::select("name", "type", "categoryName", parentEntity = "entity", "label") |>
    dplyr::mutate(
      report_type = dplyr::if_else(.data$type == "Q", "measure", "dimension"),
      type        = dplyr::if_else(.data$type == "Q", "numeric", "code"),
      source      = "chain"
    )

  ## Schema summary: input dimensions
  ss_meta <- schema |>
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

  ## exp_factor_ info for reporting area

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
      label = stringr::str_remove(.data$label, " \\(C\\)$")
    ) |>
    dplyr::mutate(
      dimension_baseunit = dplyr::if_else(.data$parentEntity == .entity, FALSE, TRUE),
      report_type        = dplyr::if_else(.data$name == "weight", NA_character_, .data$report_type)
    ) |>
    ## Separate mutate: dimension_baseunit references the value set above
    dplyr::mutate(
      dimension_baseunit = dplyr::if_else(.data$name == "weight", NA, .data$dimension_baseunit)
    )
  ## !!! ADD Information for exp_factor_ to get area as measure in the shinyapp

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

  ## Filter: dims with no data
  report_dims <- .zip$report_dimensions |>
    dplyr::filter(.data$entity == .entity) |>
    dplyr::pull("dimension")

  ## Filter: vars where multiple is TRUE
  multivars <- schema |> dplyr::filter(as.logical(.data$multiple), .data$type != "entity") |> dplyr::pull("name")

  ## !!! TMP !!!
  ## In future version there will be a field "hiddenInAnalyticalDashboard" in schemaSummary.csv
  if (!"hiddenInAnalyticalDashboard" %in% names(schema)) schema$hiddenInAnalyticalDashboard <- FALSE

  ## Filter: vars that are set as hidden for the analytical dashboard
  hiddenvars <- schema |> dplyr::filter(.data$hiddenInAnalyticalDashboard) |> dplyr::pull("name")

  wt_names <- wt_names |>
    dplyr::filter(
      (.data$report_type == "dimension" & .data$name %in% report_dims) | .data$report_type != "dimension",
      !.data$name %in% multivars,
      !.data$name %in% hiddenvars
    )

  wt_names
}
