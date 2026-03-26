#' Tool module UI function
#'
#' @noRd
mod_tool_UI <- function(id, i18n, .tr){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)



  ##
  ## UI Elements ######
  ##

  ## 3 parts sidebar: Load data, Get insights, Get analysis results
  ## 2 panels: Insights and Analysis


  ## + Sidebar ======

  ## . + Acc1: Load data ------
  ac1 <- accordion_panel(
    title = i18n$t(.tr$ac1_title),
    icon = bsicons::bs_icon("1-circle"),
    value = ns("ac_load"),

    ## Input ZIP file
    div(
      p(i18n$t(.tr$ac1_p1)),
      p(i18n$t(.tr$ac1_p2)),
      fileInput(
        inputId = ns("load_zip"),
        accept = ".zip",
        #buttonLabel = i18n$t(.tr$ac1_input1),
        #placeholder = i18n$t(.tr$ac1_input2),
        label = NULL
      ),
      ## TEST alternative shinyFiles
      ## !!! Package old and not maintained
      # br(),
      # p(
      #   "The dashboard requires a ZIP file that is produced by running the processing
      #   chain from your OF Arena survey in Rstudio (local or online). Once this file
      #   is produced, extract the data and point to its location here, so that the app
      #   can automatically find the data and structure files:"
      # ),
      #
      # shinyFiles::shinyDirButton(
      #   id = ns('path_to_folder'),
      #   label = 'Select a folder',
      #   title = 'Please select a folder',
      #   FALSE
      # )
    ),

    ## MESSAGES
    div(
      id = ns("msg_no_file"),
      i18n$t(.tr$ac1_msg_nodata),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id = ns("msg_file_ok"),
      i18n$t(.tr$ac1_msg_ok),
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id = ns("msg_file_error"),
      i18n$t(.tr$ac1_msg_err),
      verbatimTextOutput(ns("file_error_detail")),
      class = "text-danger",
      style = "font-style: italic;"
    )),

    ## ACTION BUTTON
    div(
      shinyjs::disabled(
        actionButton(
          inputId = ns("btn_read_data"),
          label = i18n$t(.tr$ac1_btn)
        )
      ),
      style = "margin-top: 1rem;"
    )

  )


  ## . + Acc2: Insights --------
  ac2 <-  accordion_panel(
    title = "Get insights",
    icon = bsicons::bs_icon("2-circle"),
    value = ns("ac2"),

    ## Content
    div(
      id = ns("msg_tmp"),
      p("Under construction"),
      class = "text-warning",
      style = "font-style: italic;"
    ),

    div(
      id = ns("insight_filters"),
      uiOutput(outputId = ns("insight_entity")),
      uiOutput(outputId = ns("insight_vars"))
    )
  )

  ## . + Accordion 3 -------------------------------------------
  ac3 <-  accordion_panel(
    title = "Run analysis",
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac3"),

    ## $$$
    ## Content
    ## h4("coming soon"),

    ## Entity selector (populated after data loads)
    uiOutput(ns("analysis_entity")),

    ## Grouped dimension selector (populated after entity is chosen)
    uiOutput(ns("analysis_dims")),

    ## Stratum auto-include note (only when sampling design requires it)
    uiOutput(ns("analysis_strat_text")),

    ## Run button
    div(
      style = "margin-top: 1rem;",
      shinyjs::disabled(
        actionButton(
          inputId = ns("btn_run_analysis"),
          label   = "Run analysis",
          icon    = icon("play"),
          class   = "btn-primary btn-sm"
        )
      )
    )
    ## $$$
  )

  ## . + Acc4: test crosstalk --------
  ac4 <-  accordion_panel(
    title = "Test Crosstalk",
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac4"),

    ## Content
    selectInput(ns("species"), "Species", levels(datasets::iris$Species), multiple = TRUE),
    sliderInput(
      ns("petal_length"), "Petal Length",
      min = min(datasets::iris$Petal.Length), max = max(datasets::iris$Petal.Length),
      value = c(min(datasets::iris$Petal.Length), max(datasets::iris$Petal.Length))
    ),
    div(
      actionButton(
        inputId = ns("btn_to_ctalk"),
        label = "To Test Panel"
      ),
      style = "margin-top: 1rem;"
    )
  )

  ## + Panels UI ======

  ## . + Insights elements ------
  ## . . + Initial message ------
  insight_msg <- div(
    id = ns("panel_insight_msg"),
    bsicons::bs_icon("arrow-left"), " Start with uploading your OLAP zipfile in the sidebar.",
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## . . + Read progress ------
  insight_progress <- shinyjs::hidden(div(
    id = ns("panel_insight_progress"),
    h3("Reading Data"),
    shinyWidgets::progressBar(
      id = ns("readdata_progress"),
      value = 0,
      title = "Reading data",
      display_pct = TRUE
    ),
    br(),
    div(
      id = ns("readdata_console"),
      style =
        "height: 300px; overflow-y: auto; background-color:#f7f7f7; font-family:monospace; font-size: small;"
    ),
    br(),
    shinyjs::disabled(
      actionButton(inputId = ns("btn_data_insights"), label = "Show data insights")
    )
  ))

  ## . . + Data insights -----
  insight_p_title <- tags$h5(
    tags$span("Survey name: ", style = "font-weight:700;"), textOutput(ns("insight_title"), inline = TRUE)
  )

  insight_tab_chain <- card(
    h5("Number of results variables per Entity"),
    tableOutput(outputId = ns("insight_chain"))
  )

  insight_out_summary <- card(
    h5("Summary of the selected numerical results variables"),
    verbatimTextOutput(outputId = ns("insight_summary"))
  )


  ## . + Panel: analysis ------
  ## Statistical analysis

  ## . + Panel crosstalk ------

  ## Value boxes
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

  ## Cards
  card1 <- card(
    full_screen = TRUE,
    h5(i18n$t("Scatter 1")),
    d3scatter::d3scatterOutput(ns("scatter1"))
  )

  card2 <- card(
    full_screen = TRUE,
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

    #h2(i18n$t("TOOL")),

    br(),

    navset_card_tab(
      id = ns("tool_tabs"),

      ## + Sidebar =====
      sidebar = sidebar(
        width = "300px",
        accordion(
          open = TRUE,
          multiple = TRUE,
          ac1, ac2, ac3, ac4
        )
      ),

      ## Spacer to right align menu items
      nav_spacer(),

      ## + Panel insights ===========
      nav_panel(
        title = i18n$t("Insights"),
        value = "tab_insights",
        icon = icon("circle-check"),
        insight_msg,
        insight_progress,
        br(),
        shinyjs::hidden(div(
          id = ns("panel_insights"),
          tags$h3("Data insights"),
          insight_p_title,
          br(),
          layout_column_wrap(insight_tab_chain, insight_out_summary, width = "300px")
        ))
      ),

      ## + panel Analysis ======================================================

      nav_panel(
        title = i18n$t("Analysis"),
        value = "tab_analysis",
        icon = icon("chart-simple"),

        ## $$$

        ## No-results message (visible until first analysis is run)
        div(
          id    = ns("analysis_no_result"),
          bsicons::bs_icon("arrow-left"),
          " Configure and run an analysis in the sidebar.",
          class = "text-warning",
          style = "font-style: italic;"
        ),

        ## Results layout - hidden until analysis completes
        shinyjs::hidden(div(
          id = ns("analysis_results"),

          ## -- Row 1: main plot controls ----------------------------------
          card(
            layout_column_wrap(
              width = "180px",
              fill  = FALSE,
              selectInput(ns("plot_dim"),     "X-axis dimension", choices = NULL),
              selectInput(ns("plot_measure"), "Measure (Y axis)", choices = NULL),
              ## $$$
              ## selectInput(ns("plot_fill"),  "Group by (fill)", choices = NULL),
              ## selectInput(ns("plot_facet"), "Facet by",        choices = NULL),
              selectizeInput(ns("plot_fill"),  "Group by (fill)", choices = NULL,
                             options = list(placeholder = "-- none --", allowEmptyOption = TRUE)),
              selectizeInput(ns("plot_facet"), "Facet by",        choices = NULL,
                             options = list(placeholder = "-- none --", allowEmptyOption = TRUE)),
              ## $$$
              ## $$$
              ## class = "pt-4" reduced to pt-1
              div(
                class = "pt-1",
                checkboxInput(ns("plot_errbar"), "Error bars", value = TRUE)
              )
              ## $$$
            ),
            ## -- Row 2: extra dimension filters (shown only when >3 dims used) --
            uiOutput(ns("analysis_extra_filters"))
          ),

          ## -- MEANS plot --------------------------------------------------
          card(
            full_screen  = TRUE,
            card_header("Means (per ha)"),
            plotOutput(ns("analysis_plot_means"), height = "400px")
          ),

          ## -- TOTALS plot -------------------------------------------------
          card(
            full_screen  = TRUE,
            card_header("Totals"),
            plotOutput(ns("analysis_plot_totals"), height = "400px")
          )

        ))

        ## $$$
      ),

      ## + crosstalk panel =========

      nav_panel(
        title = "crosstalk",
        value = "tab_ctalk",
        icon = icon("magnifying-glass"),
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
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
