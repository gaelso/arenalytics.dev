#' Tool module server function
#'
#' @importFrom rlang .data
#' @importFrom stats setNames
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## !!! FOR TESTING ONLY
    # rv <- list()
    # rv$inputs <- list()
    # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo_broken.zip", package = "arenalytics")
    # rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)
    ## !!!

    ##
    ## Accordions outputs and events ######
    ##

    ## + Acc1: check data ======
    ## Action 1: (1) check data files list, (2) update message and (3) active read button
    observeEvent(input$load_zip, {

      rv$inputs$path_zip <- input$load_zip$datapath
      rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)

      if(rv$inputs$check_zip$all_ok) {
        shinyjs::hide("msg_no_file")
        shinyjs::show("msg_file_ok")
        shinyjs::hide("msg_file_error")
        shinyjs::enable("btn_read_data")
      } else {
        shinyjs::hide("msg_no_file")
        shinyjs::hide("msg_file_ok")
        shinyjs::show("msg_file_error")
        shinyjs::disable("btn_read_data")
      }

    })

    output$file_error_detail <- renderPrint({
      req(rv$inputs$check_zip)
      # if(!rv$inputs$check_zip$all_ok) data.frame(res = unlist(rv$inputs$check_zip))
      if(!rv$inputs$check_zip$all_ok) {
        cat("Missing files:\n", paste(rv$inputs$check_zip$missing, collapse = ", "))
      }
    })

    ## + Acc1: read data ======
    ## . + Show progress -----
    ## Action: show progress in panel insights
    observeEvent(input$btn_read_data, {

      ## Hide/Show panels
      shinyjs::hide("panel_insight_msg")
      shinyjs::show("panel_insight_progress")
      shinyjs::hide("panel_insights")

      ## Reset progress
      rv$inputs$data <- NULL
      rv$inputs$data_ok <- FALSE
      shinyjs::html("readdata_console", "")  # clear on restart
      shinyWidgets::updateProgressBar(
        session = session,
        id = "readdata_progress",
        value = 0
      )
      shinyjs::disable("btn_data_insights")

      Sys.sleep(0.4)

      ## Read data and update progress
      rv$inputs$data <- withCallingHandlers(
        {
          fct_readzip(
            .path = rv$inputs$path_zip, .pb_session = session, .pb_id = "readdata_progress"
          )
        },
        message = function(m) {
          shinyjs::html(id = "readdata_console", html = paste0(m$message, '<br>'), add = TRUE)
          invokeRestart("muffleMessage")
        }
      )
      ## Make insight button visible (to be improved)
      if (!is.null(rv$inputs$data)) rv$inputs$data_ok <- TRUE

    })

    ## . + Enable insight button -----
    observe({
      req(rv$inputs$data_ok)
      if (rv$inputs$data_ok) {
        shinyjs::enable("btn_data_insights")
      } else {
        shinyjs::disable("btn_data_insights")
      }
    })

    ## . +  Show insights ------
    observeEvent(input$btn_data_insights, {
      req(rv$inputs$data)

      ## Hide progress and show insights
      shinyjs::hide("panel_insight_progress")
      shinyjs::show("panel_insights")
    })

    ## + Acc2: Insights ======
    ## . + Create labels for pickerInput() ------
    observe({
      req(rv$inputs$data)

      if (length(names(rv$inputs$data)) > 0) {
        rv$insights$entities <- names(rv$inputs$data) |> stringr::str_subset("OLAP_")
        rv$insights$entities_labs <- rv$insights$entities |> stringr::str_remove("OLAP_")
        rv$insights$entities_named <- setNames(rv$insights$entities, rv$insights$entities_labs)
      } else {
        rv$insights$entities_named <- NULL
      }

    })

    observeEvent(input$insight_sel_entity, {
      req(rv$inputs$data$SchemaSummary, rv$inputs$data$chain_summary)

      rv$insights$vars <- rv$inputs$data$chain_summary$resultVariables |>
        dplyr::filter(.data$entity == stringr::str_remove(input$insight_sel_entity, "OLAP_")) |>
        dplyr::filter(.data$areaBased)
      rv$insights$vars_named <- setNames(rv$insights$vars$name, rv$insights$vars$label)

      ## USING SchemaSummary - WRONG, should use chain_summary$resultVariables
      # rv$insights$vars <- rv$inputs$data$SchemaSummary |>
      #   dplyr::filter(parentEntity == stringr::str_remove(input$insight_sel_entity, "OLAP_")) |>
      #   dplyr::filter(type != "text") |>
      #   dplyr::select(name, paste0("label_", rv$inputs$data$chain_summary$selectedLanguage))
      #   # dplyr::select(name, paste0("label_", i18n$get_translation_language())) ## Get app language to set label
      # rv$insights$vars_named <- setNames(rv$insights$vars[[1]], rv$insights$vars[[2]])

    })

    ## . + Make pickerInput ------
    output$insight_entity <- renderUI({
      req(rv$inputs$data, rv$insights$entities_named)
      selectInput(
        inputId = ns("insight_sel_entity"),
        label = "Entities",
        choices = rv$insights$entities_named,
        multiple = FALSE
        )
    })

    output$insight_vars <- renderUI({
      req(rv$inputs$data, rv$insights$vars_named)
      selectInput(
        inputId = ns("insight_sel_vars"),
        label = "Area-based Variables",
        choices = rv$insights$vars_named,
        selected = rv$insights$vars_named,
        multiple = TRUE
      )
    })


    ## + Acc 4: crosstalk ======
    ## + + Filter data ------
    observe({
      rv$ct$user_iris <- datasets::iris |> # data("iris", envir = environment())
        dplyr::filter(is.null(input$species) | .data$Species %in% input$species) |>
        dplyr::filter(
          .data$Petal.Length >= min(input$petal_length),
          .data$Petal.Length<= max(input$petal_length)
        )
      rv$ct$shared_iris <- crosstalk::SharedData$new(rv$ct$user_iris)
    })

    ## + + Go to crosstalk panel ------
    observeEvent(input$btn_to_ctalk, {
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_ctalk"))
      session$sendCustomMessage("scroll_top", list())
    })



    ##
    ## Panel outputs ######
    ##

    ## + Insights outputs ======

    ## . + Survey title ------
    output$insight_title <- renderText({
      req(rv$inputs$data)
      paste(
        rv$inputs$data$chain_summary$surveyName,
        rv$inputs$data$chain_summary$surveyLabel,
        sep = " - "
      )
    })

    ## . + Variables ------
    output$insight_chain <- renderTable({
      req(rv$inputs$data)

      rv$inputs$data$chain_summary$resultVariables |>
        dplyr::filter(.data$active) |>
        dplyr::group_by(.data$entity, .data$areaBased) |>
        dplyr::summarise(n_var = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(areaBased = dplyr::if_else(.data$areaBased, "areaBased", "notAreaBased")) |>
        tidyr::pivot_wider(names_from = .data$areaBased, values_from = .data$n_var) |>
        dplyr::mutate(
          areaBased = dplyr::if_else(is.na(.data$areaBased), 0, .data$areaBased),
          notAreaBased = dplyr::if_else(is.na(.data$notAreaBased), 0, .data$notAreaBased)
        )

    })

    output$insight_summary <- renderPrint({
      req(
        rv$inputs$data, input$insight_sel_entity, input$insight_sel_vars,
        input$insight_sel_vars %in% names(rv$inputs$data[[input$insight_sel_entity]])
        )

      # rv$insights$entities_named
      # rv$insights$vars_named
      summary(rv$inputs$data[[input$insight_sel_entity]][,input$insight_sel_vars])

    })

    ## + crosstalk outputs ======

    ## + + Virtual boxes ------
    output$vb_seplen_mean <- renderUI({
      fct_mean(.df = rv$ct$user_iris, .colnum = .data$Sepal.Length, .rounding = 1)
    })

    output$vb_sepwid_mean <- renderUI({
      fct_mean(.df = rv$ct$user_iris, .colnum = .data$Sepal.Width, .rounding = 1)
    })

    output$vb_nb_species <- renderUI({
      length(unique(rv$ct$user_iris$Species))
    })

    ## + + Cards ------
    output$scatter1 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$ct$shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
    })

    output$scatter2 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$ct$shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width = "100%")
    })

    output$summary <- renderPrint({
      df <- rv$ct$shared_iris$data(withSelection = TRUE) |>
        dplyr::filter(.data$selected_ | is.na(.data$selected_)) |>
        dplyr::mutate(selected_ = NULL)

      cat(nrow(df), "observation(s) selected\n\n")
      summary(df)
    })

  })

}

