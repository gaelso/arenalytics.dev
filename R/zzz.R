
.onLoad <- function(libname, pkgname) {
  ## Make content of inst/assets available to the shiny app
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file("assets", package = "arenalytics.dev")
  )

  ## Set "survey" package options
  ## See more at https://r-survey.r-forge.r-project.org/survey/html/surveyoptions.html
  op <- options()
  defaults <- list(
    survey.ultimate.cluster     = FALSE,
    survey.adjust.domain.lonely = FALSE,
    survey.lonely.psu           = "remove",
    digits                      = 10
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
}


.onUnload <- function(libpath) {
  ## Remove path to assets
  shiny::removeResourcePath("assets")

  ## Restore options
  op <- c(
    "survey.ultimate.cluster",
    "survey.adjust.domain.lonely",
    "survey.lonely.psu"
  )
  options(stats::setNames(
    vector("list", length(op)),  # list of NULLs
    op
  ))
  options(digits = 7) ## Default

}

