#' FUNCTION TITLE
#'
#' @description Take a numeric column from a dataframe and return it's mean rounded
#'              to the specified number of digits.
#'
#' @param .df      A table with at least a numeric column.
#' @param .colnum   A column name of the input table.
#' @param .rounding Rounding number of digits, defaults to 0.

#' @return the mean of the input column values.
#'
#' @importFrom rlang .data
#'
#' @examples
#' fct_mean(.df = iris, .colnum = Sepal.Width, .rounding = 1)
#'
#' @export
fct_mean <- function(.df, .colnum, .rounding = 0){

  ## !!! FOR TESTING ONLY
  # .df = datasets::iris
  # colnum = rlang::quo(Sepal.Width)
  ## !!!

  colnum <- rlang::enquo(.colnum)
  vec    <- .df |> dplyr::pull(!!colnum)

  if (is.numeric(vec)) round(mean(vec), .rounding) else 0

}
