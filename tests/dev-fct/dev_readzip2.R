## Alternative to fct_readzip2()
##
## Reading content from a ZIP file is now wrapped in both tryCatch() and
## withCallingHandlers() to have both warnings and errors captured. Current function
## Only gets the first item between a warning and error if both present. Chosen for
## simplicity but less functional that this alternative here.

dev_readzip2 <- function(.path, .pb_session = NULL, .pb_id = NULL) {

  ## !!! FOR TESTING ONLY
  # .path = "inst/extdata/OLAP_shiny_demo_corrupted.zip" ; .pb_session = NULL ; .pb_id = NULL
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
  out <- purrr::map(zip_content, function(x) {

    ext <- tools::file_ext(x)
    pct <- which(zip_content == x) / length(zip_content) * 100

    ## tryCatch handles errors (returns NULL + logs).
    ## withCallingHandlers handles warnings without aborting execution — it logs
    ## them and resumes, so the read still returns a value if it can.
    ##
    ## Previous approach used tryCatch(error=, warning=) with a re-run inside
    ## the warning handler. That caused errors thrown during the re-run to escape
    ## the outer tryCatch, since handler bodies are not covered by the same
    ## tryCatch frame. The nested pattern below avoids that entirely.
    tt <- tryCatch(
      withCallingHandlers(
        {
          if (ext == "csv") {
            utils::read.csv(unz(.path, x))
          } else if (ext == "rds") {
            readRDS(gzcon(unz(.path, x)))
          } else if (ext == "json") {
            jsonlite::fromJSON(unz(.path, x))
          } else {
            NULL  # unsupported format; not an error
          }
        },
        warning = function(w) {
          message(sprintf(
            "[%s] WARNING reading %s \u2014 %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), x, conditionMessage(w)
          ))
          invokeRestart("muffleWarning")  # log and resume; do not abort
        }
      ),
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
    if (!is.null(.pb_session) && !is.null(.pb_id)) {
      shinyWidgets::updateProgressBar(
        session = .pb_session,
        id      = .pb_id,
        value   = round(pct)
      )
    }

    Sys.sleep(0.1)

    tt

  })

  names(out) <- file_names

  ## -- 3. Emit a final summary message so the Shiny console div shows outcome --
  ## This is captured by withCallingHandlers() in mod_tool_server the same way
  ## as the per-file messages above.
  if (length(read_errors) > 0) {
    message(sprintf(
      "[%s] \u26a0 Load finished with %d error(s): %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(read_errors),
      paste(names(read_errors), collapse = ", ")
    ))
  } else {
    message(sprintf(
      "[%s] \u2713 All %d file(s) loaded successfully.",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(out)
    ))
  }

  ## -- 4. Attach error log as an attribute so callers can inspect it ------------
  attr(out, ".errors") <- read_errors

  out

}

zips <- list(
  good      = "inst/extdata/OLAP_Shiny_demo.zip",
  broken    = "inst/extdata/OLAP_Shiny_demo_broken.zip",
  corrupted = "inst/extdata/OLAP_Shiny_demo_corrupted.zip"
)

for (label in names(zips)) {
  cat("\n===", label, "===\n")
  console <- character(0)
  result <- withCallingHandlers(
    dev_readzip2(.path = zips[[label]]),
    message = function(m) {
      console <<- c(console, m$message)
      invokeRestart("muffleMessage")
    }
  )
  cat(console, sep = "")
  errs <- attr(result, ".errors")
  cat("data_ok:", !is.null(result) && length(errs) == 0, "\n")
}

