#' About module server function
#'
#' @noRd
mod_about_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$dl_example <- downloadHandler(
      filename = function(){"OLAP_Shiny_demo.zip"},
      content  = function(file){file.copy(system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics.dev"), file)}
    )

    output$dl_example2 <- downloadHandler(
      filename = function(){"OLAP_Shiny_demo_broken.zip"},
      content  = function(file){file.copy(system.file("extdata/OLAP_Shiny_demo_broken.zip", package = "arenalytics.dev"), file)}
    )


  }) ## END module server function

}
