#' Check OpenForis Arena OLAP ZIP file integrity
#'
#' @description fct_checkzip() checks if the ZIP files uploaded in the master ShinyApp has
#'              the core files required to run the analysis.
#'
#' @param .path a path to the zip file from
#'
#' @returns A list with TRUE/FALSE.
#'
#' @examples
#' zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
#' if (nzchar(zipfile) && file.exists(zipfile)) {
#'   zip_check <- fct_checkzip(.path = zipfile)
#'   zip_check$all_ok
#' }
#'
#' @export
fct_checkzip <- function(.path){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo_broken.zip"
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # !!!

  checklist <- data.frame(
    item = c("chain_summary.json", "SchemaSummary.csv", "ReportDimensions.csv", "categories.rds", "taxonomies.rds"),
    check = c("chain", "schema", "dimensions", "categories", "taxonomies")
  )

  ## Get file names
  zip_content <- zip::zip_list(.path)$filename |> sort() |> stringr::str_remove(".*/")

  ## Check is files names match the checklist
  present  <- checklist$item %in% zip_content
  zipcheck <- as.list(stats::setNames(present, paste0("has_", checklist$check)))
  zipmissing <- checklist$item[!present]

  ## Check number of entity tables
  nb_entities <- stringr::str_subset(zip_content, pattern = "OLAP_.*\\.csv") |> length()
  zipcheck$has_OLAPentities <- nb_entities > 0
  if (nb_entities == 0) zipmissing <- c(zipmissing, "OLAP_*.csv")

  ## Summary
  zipcheck$all_ok <- all(unlist(zipcheck))

  zipcheck$missing <- zipmissing

  zipcheck

}

## TEST
# fct_checkzip(.path = "inst/extdata/OLAP_shiny_demo.zip")

