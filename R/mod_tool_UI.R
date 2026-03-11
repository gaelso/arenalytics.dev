#' Tool module UI function
#'
#' @noRd
mod_tool_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  ## + Sidebar =================================================================

  ## ++ Accordion 1 -------------------------------------------------
  ac1 <- accordion_panel(
    title = i18n$t("Action 1"),
    icon = bsicons::bs_icon("1-circle"),
    value = ns("ac1"),

    ## Accordion content
    div(
      p(
        "Fusce vitae ante non turpis rutrum varius. Curabitur pulvinar lacinia velit,
        ut pellentesque lectus facilisis vitae."
      ),
      selectInput(ns("species"), "Species", levels(datasets::iris$Species), multiple = TRUE),
      sliderInput(
        ns("petal_length"), "Petal Length",
        min = min(datasets::iris$Petal.Length), max = max(datasets::iris$Petal.Length),
        value = c(min(datasets::iris$Petal.Length), max(datasets::iris$Petal.Length))
      )
    )
  )


  ## ++ Accordion 2 ---------------------------------------------------
  ac2 <-  accordion_panel(
    title = i18n$t("Action 2"),
    icon = bsicons::bs_icon("2-circle"),
    value = ns("ac2"),

    ## Content
    div(
      id = ns("msg_no_check"),
      p("Suspendisse suscipit accumsan sagittis. Aliquam ut scelerisque mauris."),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    div(
        actionButton(
          inputId = ns("btn_panel2"),
          label = "To Panel 2"
        ),
      style = "margin-top: 1rem;"
    )
  )

  ## ++ Accordion 3 -------------------------------------------
  ac3 <-  accordion_panel(
    title = i18n$t("Action 3"),
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac3"),

    ## Content
    h4("coming soon")
  )

  ## + Main panel 1 ============================================================

  ## ++ Value boxes ------------------------------------------------------------
  vb1 <- value_box(
    title = "Sepal Mean length",
    value = htmlOutput(ns("vb_seplen_mean")),
    showcase = bsicons::bs_icon("calendar3", size = "40px"),
    theme = "primary"
  )

  vb2 <- value_box(
    title = "Sepal Mean Width",
    value = htmlOutput(ns("vb_sepwid_mean")),
    showcase = bsicons::bs_icon("pin-map", size = "40px"),
    theme = "secondary"
  )

  vb3 <- value_box(
    title = "Number of Species",
    value = htmlOutput(ns("vb_nb_species")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme = "warning"
  )


  ## ++ cards ------------------------------------------------------------------
  card1 <- card(
    full_screen = T,
    h5(i18n$t("Scatter 1")),
    d3scatter::d3scatterOutput(ns("scatter1"))
  )

  card2 <- card(
    full_screen = T,
    h5(i18n$t("Scatter 2")),
    d3scatter::d3scatterOutput(ns("scatter2"))
  )

  card3 <- card(
    h5(i18n$t("Summary of selected data")),
    verbatimTextOutput(ns("summary"))
  )



  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    h2(i18n$t("TOOL")),

    br(),

    navset_card_tab(
      id = ns("tool_tabs"),

      ## + Sidebar =============================================================

      sidebar = sidebar(
        width = "300px",
        accordion(
          open = TRUE,
          multiple = TRUE,
          ac1, ac2, ac3
        )
      ),

      ## Spacer to right align menu items
      nav_spacer(),

      ## + Main panel 1 ========================================================

      nav_panel(
        title = i18n$t("Panel 1"),
        value = "tab1",
        icon = icon("circle-check"),
        ## Value boxes
        div(
          id = ns("vb_section"),
          layout_column_wrap(
            #width = "200px",
            fill = FALSE,
            vb1, vb2, vb3
          )
        ),
        ## Cards
        div(
          id = ns("card_section"),
          layout_column_wrap(card1, card2, width = "300px"),
          card3
        )
      ),

      ## + panel 2===========================================================

      nav_panel(
        title = i18n$t("Panel 2"),
        value = "tab2",
        icon = icon("chart-simple"),

      ),

      ## + panel 3 ============================================

      nav_panel(
        title = i18n$t("Panel 3"),
        value = "tab3",
        icon = icon("magnifying-glass"),

      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
