#' Read OpenForis Arena OLAP zipfile data (error-safe version)
#'
#' @description A fault-tolerant version of [fct_readzip()]. Each file inside
#'   the ZIP is read inside a [tryCatch()] so that a single corrupted file does
#'   not abort the entire load. Files that fail to read are returned as `NULL`
#'   in the output list, and the error messages are attached to the result as
#'   the `.errors` attribute. The caller can inspect that attribute to decide
#'   whether to surface a warning in the UI.
#'
#'   The outer listing of the ZIP archive itself is also guarded: if
#'   [zip::zip_list()] fails (e.g. the file is not a valid ZIP), the function
#'   returns `NULL` immediately with a descriptive error message.
#'
#' @param .path A path to the data ZIP file.
#' @param .pb_ss A Shiny session for [shinyWidgets::updateProgressBar()].
#'   Default `NULL` (progress bar is only updated inside a Shiny context).
#' @param .pb_id The widget ID for [shinyWidgets::updateProgressBar()].
#'   Default `NULL`.
#' @param .entity_prefix A character describing how OpenForis Arena prefix entity
#'   tables. Default "MAU_".
#'
#' @returns A named list of entity data frames and survey descriptors, or
#'   `NULL` if the archive itself cannot be opened. Each element is either the
#'   parsed file content or `NULL` when reading failed. Failed files are
#'   recorded in `attr(result, ".errors")` as a named character vector
#'   (`name = error message`).
#'
#' @examples
#' zipfile <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics.dev")
#' if (nzchar(zipfile) && file.exists(zipfile)) {
#'   zipdata <- fct_readzip2(.path = zipfile)
#'
#'   ## Check for any read errors
#'   errs <- attr(zipdata, ".errors")
#'   if (length(errs) > 0) {
#'     message("Files with read errors: ", paste(names(errs), collapse = ", "))
#'   }
#'
#'   summary(zipdata$OLAP_tree$tree_biomass_ag)
#' }
#'
#' @export
fct_readzip2 <- function(.path, .pb_ss = NULL, .pb_id = NULL, .entity_prefix = "MAU_") {

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo.zip"
  # .path = "inst/extdata/OLAP_shiny_demo_corrupted.zip"
  # .pb_ss = NULL ; .pb_id = NULL ; .entity_prefix = "MAU_"
  # !!!


  ## -- 1. Guard: list archive contents -----------------------------------------
  zip_content <- tryCatch(
    zip::zip_list(.path)$filename |> sort(),
    error = function(e) {
      message(sprintf(
        "[%s] ERROR: Could not open ZIP archive \u2014 %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        conditionMessage(e)
      ))
      NULL
    }
  )

  if (is.null(zip_content)) return(NULL)

  ## Strip directory prefixes and extensions for output list names
  ## (basename handles ZIPs that embed a subdirectory; file_path_sans_ext strips ext)
  file_names <- tools::file_path_sans_ext(basename(zip_content))

  ## Accumulate per-file read errors: name = file, value = error message
  read_errors <- character(0)

  ## -- 2. Read each file individually -------------------------------------------
  zipdata <- purrr::map(zip_content, function(x) {

    ext <- tools::file_ext(x)
    pct <- which(zip_content == x) / length(zip_content) * 100

    ## For improved version that gets both errors and warnings together see:
    ## tests/dev-fct/dev_readzip2.R
    tt <- tryCatch(
        {
          if (ext == "csv") {
            utils::read.csv(unz(.path, x), stringsAsFactors = FALSE)
          } else if (ext == "rds") {
            readRDS(gzcon(unz(.path, x)))
          } else if (ext == "json") {
            jsonlite::fromJSON(unz(.path, x))
          } else {
            NULL  # unsupported format; not an error
          }
        },
        warning = function(w) {
          msg <- conditionMessage(w)
          message(sprintf(
            "[%s] WARNING reading %s \u2014 %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), x, msg
          ))
          read_errors[[x]] <<- msg  # <<- writes to the enclosing map() env
          NULL
          #invokeRestart("muffleWarning")  # log and resume; do not abort
        },
      error = function(e) {
        msg <- conditionMessage(e)
        message(sprintf(
          "[%s] ERROR reading %s \u2014 %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), x, msg
        ))
        read_errors[[x]] <<- msg  # <<- writes to the enclosing map() env
        NULL
      }
    )

    ## Log success (only when the file was actually parsed)
    if (!is.null(tt)) {
      message(sprintf(
        "[%s] Loaded %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), x
      ))
    }

    ## Update progress bar when running inside Shiny
    if (!is.null(.pb_ss) && !is.null(.pb_id)) {
      shinyWidgets::updateProgressBar(
        session = .pb_ss,
        id      = .pb_id,
        value   = round(pct)
      )
    }

    Sys.sleep(0.1)

    tt

  })

  ## Change names to lowercase (myTable -> my_table)
  file_names <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", file_names))
  file_names <- stringr::str_replace_all(file_names, tolower(.entity_prefix), .entity_prefix)
  names(zipdata) <- file_names

  ## -- 3. Emit a final summary message so the Shiny console div shows outcome -----
  ## This is captured by withCallingHandlers() in mod_tool_server the same way
  ## as the per-file messages above.
  if (length(read_errors) > 0) {

    out <- list(data = zipdata, errors = read_errors, var_meta = NULL)

    message(sprintf(
      "[%s] \u26a0 Load finished with %d error(s): %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(read_errors),
      paste(names(read_errors), collapse = ", ")
    ))


  } else {

    ## Generate metadata of entities column names
    ## CF: fct_varinfo() in R/fct_varinfo.R
    entity_names <- file_names |>
      stringr::str_subset(.entity_prefix) |>
      stringr::str_remove(.entity_prefix)

    var_meta <- purrr::map(entity_names, function(x){
      fct_varinfo(.zip = zipdata, .entity = x, .entity_prefix = .entity_prefix)
    })

    names(var_meta) <- entity_names

    out <- list(data = zipdata, errors = read_errors, var_meta = var_meta)

    message(sprintf(
      "[%s] \u2713 All %d file(s) loaded successfully and variables metadata tables built.",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(zipdata)
    ))


  }

  # ## -- 4. Attach error log as an attribute so callers can inspect it ------------
  # attr(out, ".errors") <- read_errors

  out

}
