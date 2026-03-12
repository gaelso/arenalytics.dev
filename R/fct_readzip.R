#' Read OpenForis Arena OLAP zipfile data
#'
#' @description Processing chaina of OpenForis Arena surveys generate a ZIP file
#'              containing field entity data at the minimum area unit (OLAP tables) and
#'              survey descriptors. fct_readzip() is tailored to load this data.
#'
#' @param .path A path to the data ZIP file.
#' @param .pb_session A shiny app session for updating shinyWidgets::updateProgressBar(). Default: NULL,
#'                    activate only in a shiny context where a shinyWidgets::progressBar() is set.
#' @param .pb_id The widget ID for shinyWidgets::updateProgressBar(). Default: NULL,
#'               activate only in a shiny context where a shinyWidgets::progressBar() is set.
#'
#' @returns a list of entity level data frames and survey descriptors.
#'
#' @examples
#' zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
#' if (nzchar(zipfile) && file.exists(zipfile)) {
#'   zipdata <- fct_readzip(.path = zipfile)
#'   summary(zipdata$OLAP_tree$tree_biomass_ag)
#' }
#'
#' @export
fct_readzip <- function(.path, .pb_session = NULL, .pb_id = NULL){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # .pb_session = NULL
  # .pb_id = NULL
  # !!!

  ## Get file names
  zip_content <- zip::zip_list(.path)$filename |> sort()

  file_names <- zip_content |> stringr::str_remove("\\..*")

  ## NO TEMP DIR needed however may yield error with loading RDS files
  out <- purrr::map(zip_content, function(x){

    ext <- stringr::str_remove(x, pattern = ".*\\.")
    if (ext == "csv") {
      tt <- utils::read.csv(unz(.path, x))
    } else if (ext == "rds") {
      tt <- readRDS(gzcon(unz(.path, x)))
      #tt <- purrr::map(tt, dplyr::as_tibble)
    } else if (ext == "json") {
      tt <- jsonlite::fromJSON(unz(.path, x))
    } else {
      tt <- NULL
    }

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log       <- paste("Loaded", x)
    pct <- which(zip_content == x) / length(zip_content) * 100
    message(paste0("[", timestamp, "] ", log))
    if (!is.null(.pb_session) & !is.null(.pb_id)){
      shinyWidgets::updateProgressBar(
        session = .pb_session,
        id = .pb_id,
        value = round(pct)
      )
    }

    Sys.sleep(0.1)

    tt

  })

  names(out) <- file_names
  out

  # TEMP DIR solution - TEST GIVES SAME DATA
  # temp_dir <- file.path(tempdir(), paste0("arenalytics-", Sys.time()))
  # dir.create(temp_dir)
  # utils::unzip(zipfile = .path, exdir = temp_dir)
  #
  # file_paths <- sort(list.files(temp_dir, full.names = T))
  #
  # out1 <- purrr::map(file_paths, function(x){
  #
  #   ext <- stringr::str_remove(x, pattern = ".*\\.")
  #   if (ext == "csv") {
  #     tt <- utils::read.csv(x)
  #   } else if (ext == "rds") {
  #     tt <- readRDS(x)
  #     #tt <- purrr::map(tt, dplyr::as_tibble)
  #   } else if (ext == "json") {
  #     tt <- jsonlite::fromJSON(x)
  #   } else {
  #     tt <- NULL
  #   }
  #   tt
  # })
  #
  # unlink(temp_dir)
  #
  # names(out1) <- file_names
  #
  # ## Check
  # identical(out, out1)
  # purrr::map2_lgl(out, out1, identical)
  # identical(out$OLAP_bamboo, out1$OLAP_bamboo)
  # all.equal(out$OLAP_bamboo, out1$OLAP_bamboo)

}

## TMP
# tt <- out$SchemaSummary |> dplyr::as_tibble()
# ls <- out$chain_summary
# ls2 <- ls$analysis
# tt2 <- ls$resultVariables |> dplyr::tibble()
# names(ls)
# out$chain_summary$surveyName
# out$chain_summary$surveyLabel
# out$chain_summary$baseUnit
# out$chain_summary$analysis

