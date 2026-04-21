
.onLoad <- function(libname, pkgname) {
  ## Make content of inst/assets available to the shiny app
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file("assets", package = "arenalytics.dev")
  )

  ## Survey options are set locally inside fct_arenalyse() with on.exit()
  ## restore, so they do not need to be managed here.
  options(digits = 10)
}


.onUnload <- function(libpath) {
  ## Remove path to assets
  shiny::removeResourcePath("assets")

  ## Restore digits
  options(digits = 7) ## Default

}

