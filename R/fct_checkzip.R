#' Check OpenForis Arena pre-processed ZIP file integrity
#'
#' @description fct_checkzip() checks if the ZIP files uploaded in the master ShinyApp has
#'              the core files required to run the analysis.
#'
#' @param .path a path to the zip file from
#' @param .entity_prefix A character describing how OpenForis Arena prefix entity
#'   tables. Default "MAU_".
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
fct_checkzip <- function(.path, .entity_prefix){

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo_broken.zip"
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # .entity_prefix = "MAU_"
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
  nb_entities <- stringr::str_subset(zip_content, pattern = paste0(.entity_prefix, ".*\\.csv")) |> length()
  zipcheck$has_OLAPentities <- nb_entities > 0
  if (nb_entities == 0) zipmissing <- c(zipmissing, paste0(.entity_prefix, "*.csv"))

  ## Summary
  zipcheck$all_ok <- all(unlist(zipcheck))

  zipcheck$missing <- zipmissing

  zipcheck

}


