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
    radioButtons(
      inputId = ns("analysis_mode"),
      label   = strong("Analysis type"),
      choices = c("Area" = "area", "Other measures" = "other"),
      selected = "other",
      inline = TRUE
    ),

    uiOutput(ns("analysis_entity")),

    ## Grouped dimension selector (populated after entity is chosen)
    uiOutput(ns("analysis_dims")),

    ## Stratum auto-include note (only when sampling design requires it)
    uiOutput(ns("analysis_strat_text")),

    ## Warning when more than 4 dimensions are selected
    uiOutput(ns("analysis_too_many_dims")),

    ## Lonely-PSU strategy
    shinyWidgets::radioGroupButtons(
      inputId  = ns("analysis_lonely_psu"),
      label    = strong("Lonely PSU handling"),
      choices  = c("Adjust (conservative)" = "adjust",
                   "Remove (permissive)"   = "remove"),
      selected = "adjust",
      size     = "xs",
      justified = TRUE
    ),

    ## ++ ##
    shinyWidgets::radioGroupButtons(
      inputId  = ns("analysis_compute_mode"),
      label    = strong("Computation mode"),
      choices  = c("Fast" = "fast",
                   "Safe" = "safe"),
      selected = "fast",
      size     = "xs",
      justified = TRUE
    ),
    div(
      class = "text-info",
      style = "font-size: 0.85em; font-style: italic; margin-top: 0.25rem;",
      bsicons::bs_icon("info-circle"),
      " Fast computes all measures together. Safe is slower, but keeps going when some measures fail."
    ),
    ## ++ ##

    ## Confidence level
    div(
      class = "pvalue-select-wrap",
      selectInput(
        inputId  = ns("analysis_p_value"),
        label    = strong("Confidence level"),
        choices  = c("0.99", "0.95", "0.90", "0.80", "0.68"),
        selected = "0.95"
      )
    ),

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
    bsicons::bs_icon("arrow-left"), " Start with uploading your MAU zipfile in the sidebar.",
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
  insight_p_title <- tagList(
    tags$h5(textOutput(ns("insight_title"), inline = TRUE)),
    hr(),
    uiOutput(ns("insight_chain_info"))
  )

  ## $$$

  ## Row 1: Base-unit dimensions
  insight_row_bu <- card(
    min_height = "200px",
    card_header(bsicons::bs_icon("diagram-3"), " Base-unit dimensions"),
    uiOutput(ns("insight_bu_out"))
  )

  ## Row 2: Sub-unit dimensions
  insight_row_sub <- card(
    min_height = "200px",
    card_header(bsicons::bs_icon("diagram-2"), " Sub-unit dimensions"),
    uiOutput(ns("insight_sub_out"))
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
          insight_p_title,
          hr(),
          uiOutput(ns("insight_current_selection")),
          hr(),
          insight_row_bu,
          insight_row_sub,
          insight_row_meas
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
          ),
          br(),
          shinyjs::disabled(
            actionButton(
              inputId = ns("btn_analysis_results"),
              label = "Show analysis results"
            )
          )
        )),

        ## Results layout - hidden until analysis completes
        shinyjs::hidden(div(
          id = ns("analysis_results"),

          ## ++ ##
          ## -- Row 1: analysis table --------------------------------------
          ## ++ ##
          card(
            card_header("Cross outputs settings"),
            layout_column_wrap(
              width = "220px",
              fill  = FALSE,
              selectInput(
                ns("analysis_sel_measure"),
                "Measure",
                choices = NULL,
                selected = NULL
              )
            ),
            uiOutput(ns("analysis_extra_filters"))
          ),

          card(
            full_screen = TRUE,
            card_header("Results table"),
            layout_column_wrap(
              width = "220px",
              fill  = FALSE,
              div(
                id = ns("analysis_table_source_wrap"),
                selectInput(
                  ns("analysis_table_source"),
                  "Table source",
                  choices = c("Means (per ha)" = "MEANS", "Totals" = "TOTALS"),
                  selected = "MEANS"
                )
              )
            ),
            div(
              style = "display: flex; gap: 0.75rem; align-items: center; margin: 0.75rem 0;",
              actionButton(
                ns("analysis_table_copy"),
                "Copy visible table",
                class = "btn-sm"
              ),
              downloadButton(
                ns("analysis_table_download"),
                "Download full table (CSV)",
                class = "btn-sm"
              )
            ),
            DT::DTOutput(ns("analysis_table"))
          ),

          card(
            card_header("Figure settings"),
            layout_column_wrap(
              width = "180px",
              fill  = FALSE,
              selectInput(ns("plot_dim"), "X-axis dimension", choices = NULL),
              selectizeInput(
                ns("plot_fill"),
                "Group by",
                choices = NULL,
                options = list(placeholder = "-- none --", allowEmptyOption = TRUE)
              ),
              selectizeInput(
                ns("plot_facet"),
                "Facet by",
                choices = NULL,
                options = list(placeholder = "-- none --", allowEmptyOption = TRUE)
              )
            ),
            hr(style = "margin: 0.9rem 0 0.75rem 0;"),
            layout_column_wrap(
              width = "180px",
              fill  = FALSE,
              div(
                class = "pt-1",
                checkboxInput(ns("plot_errbar"), "Error bars", value = TRUE)
              ),
              div(
                class = "pt-1",
                checkboxInput(ns("plot_flip"), "Swap axis", value = FALSE)
              ),
              div(
                class = "pt-1",
                checkboxInput(ns("plot_wrap_labels"), "Wrap labels", value = TRUE)
              ),
              div(
                class = "pt-1",
                checkboxInput(ns("plot_hide_legend"), "Hide legend", value = FALSE)
              )
            ),
            uiOutput(ns("analysis_plot_guidance"))
          ),

          div(
            id = ns("analysis_means_card"),
            card(
              full_screen  = TRUE,
              card_header("Means (per ha)"),
              div(
                style = "width: fit-content; margin-bottom: 0.75rem;",
                downloadButton(
                  ns("analysis_plot_means_download"),
                  "Download means plot (PNG)",
                  class = "btn-sm"
                )
              ),
              plotOutput(ns("analysis_plot_means"), height = "400px")
            )
          ),

          card(
            full_screen  = TRUE,
            card_header("Totals"),
            div(
              style = "width: fit-content; margin-bottom: 0.75rem;",
              downloadButton(
                ns("analysis_plot_totals_download"),
                "Download totals plot (PNG)",
                class = "btn-sm"
              )
            ),
            plotOutput(ns("analysis_plot_totals"), height = "400px")
          ),

          card(
            card_header("Export report"),
            div(
              style = "display: flex; gap: 0.75rem; align-items: end; flex-wrap: wrap;",
              div(
                style = "min-width: 220px;",
                selectInput(
                  ns("analysis_report_format"),
                  "Report format",
                  choices = c("Word" = "docx", "HTML" = "html"),
                  selected = "docx"
                )
              ),
              div(
                style = "width: fit-content; margin-bottom: 0.5rem;",
                downloadButton(
                  ns("analysis_report_download"),
                  "Download report",
                  class = "btn-sm"
                )
              )
            )
          )
          ## ++ ##

        ))
        ## ++ ##

        ## $$$
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
