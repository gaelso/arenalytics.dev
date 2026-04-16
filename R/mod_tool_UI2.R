#' Tool module UI function
#'
#' @noRd
mod_tool_UI2 <- function(id, i18n, .tr){

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


  ## $$$
  ## . + Acc2: Insights — REMOVED (entity/variable selection moved into Insights panel)
  ## ac2 <-  accordion_panel(
  ##   title = "Get insights",
  ##   icon = bsicons::bs_icon("2-circle"),
  ##   value = ns("ac2"),
  ##   div(
  ##     id = ns("msg_tmp"),
  ##     p("Under construction"),
  ##     class = "text-warning",
  ##     style = "font-style: italic;"
  ##   ),
  ##   div(
  ##     id = ns("insight_filters"),
  ##     uiOutput(outputId = ns("insight_entity")),
  ##     uiOutput(outputId = ns("insight_vars"))
  ##   )
  ## )
  ## $$$

  ## . + Acc3: Analysis of measures across selected dimensions -----------------
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

    ## Warning when more than 4 dimensions are selected
    uiOutput(ns("analysis_too_many_dims")),

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
    tags$span("Survey name: ", style = "font-weight:700;"),
    textOutput(ns("insight_title"), inline = TRUE)
  )

  ## $$$

  ## Entity selector — choices populated server-side on data load
  insight_entity_sel <- selectInput(
    inputId  = ns("insight_sel_entity"),
    label    = "Entity",
    choices  = NULL
  )

  ## Row 1: Base-unit dimensions
  insight_row_bu <- card(
    min_height = "200px",
    card_header(bsicons::bs_icon("diagram-3"), " Base-unit dimensions"),
    layout_columns(
      col_widths = c(6, 6),
      shinyWidgets::checkboxGroupButtons(
        inputId   = ns("insight_bu_sel"),
        label     = NULL,
        choices   = character(0),
        individual = TRUE,
        size      = "sm"
      ),
      uiOutput(ns("insight_bu_out"))
    )
  )

  ## Row 2: Sub-unit dimensions
  insight_row_sub <- card(
    min_height = "200px",
    card_header(bsicons::bs_icon("diagram-2"), " Sub-unit dimensions"),
    layout_columns(
      col_widths = c(6, 6),
      shinyWidgets::checkboxGroupButtons(
        inputId   = ns("insight_sub_sel"),
        label     = NULL,
        choices   = character(0),
        individual = TRUE,
        size      = "sm"
      ),
      uiOutput(ns("insight_sub_out"))
    )
  )

  ## ++ ##
  ## Row 3: Measures
  insight_row_meas <- card(
    min_height = "160px",
    card_header(bsicons::bs_icon("bar-chart"), " Measures"),
    uiOutput(ns("insight_meas_out"))
  )
  ## ++ ##

  ## $$$


  ## . + Panel: analysis ------
  ## Statistical analysis


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
          ## $$$
          ## ac2 removed
          ac1, ac3
          ## $$$
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
          ## $$$
          insight_p_title,
          br(),
          insight_entity_sel,
          hr(),
          insight_row_bu,
          insight_row_sub,
          insight_row_meas
          ## $$$
        ))
      ),

      ## + panel Analysis ======================================================

      nav_panel(
        title = i18n$t("Analysis"),
        value = "tab_analysis",
        icon = icon("chart-simple"),

        ## $$$

        ## ++ ##
        ## No-results message (visible until first analysis is run)
        div(
          id    = ns("analysis_no_result"),
          bsicons::bs_icon("arrow-left"),
          " Configure and run an analysis in the sidebar.",
          class = "text-warning",
          style = "font-style: italic;"
        ),

        ## Analysis progress - shown while fct_arenalyse() is running
        shinyjs::hidden(div(
          id = ns("analysis_progress"),
          h3("Running analysis"),
          shinyWidgets::progressBar(
            id = ns("analysis_progress_bar"),
            value = 0,
            title = "Running analysis",
            display_pct = TRUE
          ),
          br(),
          div(
            id = ns("analysis_console"),
            style =
              "height: 300px; overflow-y: auto; background-color:#f7f7f7; font-family:monospace; font-size: small;"
          )
        )),

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
        ## ++ ##

        ## $$$
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
