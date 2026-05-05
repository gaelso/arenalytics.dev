#' Find labels in a schema-like table using the selected language
#'
#' @param .df A data frame containing a name column and label columns.
#' @param .name Character vector of names to look up.
#' @param .lang Language code used to build the preferred label column.
#' @param .name_col Column containing the keys to match. Default `"name"`.
#'
#' @return A character vector of labels aligned with `.name`.
#'
#' @noRd
utils_find_label <- function(.df, .name, .lang = "en", .name_col = "name") {

  .df <- tibble::as_tibble(.df)
  label_col <- paste0("label_", .lang)

  if (!label_col %in% names(.df)) {
    label_col <- if ("label" %in% names(.df)) "label" else NULL
  }

  if (is.null(label_col)) {
    return(.name)
  }

  lookup_df <- .df |>
    dplyr::filter(.data[[.name_col]] %in% .name) |>
    dplyr::select(name = dplyr::all_of(.name_col), label = dplyr::all_of(label_col))

  lookup <- stats::setNames(as.character(lookup_df$label), lookup_df$name)
  labels <- unname(lookup[.name])

  dplyr::coalesce(dplyr::na_if(labels, ""), .name)
}


#' Make named vectors of dimensions for shinyWidgets::checkboxGroupButtons()
#'
#' @param .rv reactiveValues from the main shiny App.
#' @param .is_bu `TRUE` or `FALSE`, are the target dimensions at base unit?
#'
#' @return A named vector of choices
#'
#' @noRd
utils_make_grp <- function(.rv, .is_bu) {

  sub <- .rv$analysis$dim_meta |>
    dplyr::filter(.data$report_type == "dimension", .data$dimension_baseunit == .is_bu)

  if (nrow(sub) == 0) return(character(0))

  stats::setNames(sub$name, sub$label)
}
