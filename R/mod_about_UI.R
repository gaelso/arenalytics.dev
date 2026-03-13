
mod_about_UI <- function(id, i18n, .tr){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  card1 <- card(
    card_header("Download mock datasets"),
    downloadButton(outputId = ns("dl_example"), label = "Working example file"),
    downloadButton(outputId = ns("dl_example2"), label = "Broken example file")
  )


  ##
  ## Layout UI elements with tagList() function ######
  ##

  tagList(

    h3(i18n$t("About {arenalytics}")),

    br(),

    div(class = "col-md-4", card1)

  ) ## END tagList

} ## END module UI function
