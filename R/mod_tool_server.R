#' Tool module server function
#'
#' @importFrom rlang .data
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




    ## + Acc 4: crosstalk ======
    ## + + Filter data ------
    observe({
      rv$rv1$user_iris <- datasets::iris |> # data("iris", envir = environment())
        dplyr::filter(is.null(input$species) | .data$Species %in% input$species) |>
        dplyr::filter(
          .data$Petal.Length >= min(input$petal_length),
          .data$Petal.Length<= max(input$petal_length)
        )
      rv$rv1$shared_iris <- crosstalk::SharedData$new(rv$rv1$user_iris)
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

    ## + + Survey title ------
    output$insight_title <- renderText({
      req(rv$inputs$data)
      paste(
        rv$inputs$data$chain_summary$surveyName,
        rv$inputs$data$chain_summary$surveyLabel,
        sep = " - "
      )
    })

    output$insight_chain <- renderTable({
      req(rv$inputs$data)

      rv$inputs$data$chain_summary$resultVariables |>
        dplyr::filter(active) |>
        dplyr::group_by(entity, areaBased) |>
        dplyr::summarise(n_var = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(areaBased = dplyr::if_else(areaBased, "areaBased", "notAreaBased")) |>
        tidyr::pivot_wider(names_from = areaBased, values_from = n_var) |>
        dplyr::mutate(
          areaBased = dplyr::if_else(is.na(areaBased), 0, areaBased),
          notAreaBased = dplyr::if_else(is.na(notAreaBased), 0, notAreaBased)
        )

    })


    ## + crosstalk outputs ======

    ## + + Virtual boxes ------
    output$vb_seplen_mean <- renderUI({
      fct_mean(.df = rv$rv1$user_iris, .colnum = .data$Sepal.Length, .rounding = 1)
    })

    output$vb_sepwid_mean <- renderUI({
      fct_mean(.df = rv$rv1$user_iris, .colnum = .data$Sepal.Width, .rounding = 1)
    })

    output$vb_nb_species <- renderUI({
      length(unique(rv$rv1$user_iris$Species))
    })

    ## + + Cards ------
    output$scatter1 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$rv1$shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
    })

    output$scatter2 <- d3scatter::renderD3scatter({
      d3scatter::d3scatter(rv$rv1$shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width = "100%")
    })

    output$summary <- renderPrint({
      df <- rv$rv1$shared_iris$data(withSelection = TRUE) |>
        dplyr::filter(.data$selected_ | is.na(.data$selected_)) |>
        dplyr::mutate(selected_ = NULL)

      cat(nrow(df), "observation(s) selected\n\n")
      summary(df)
    })

  })

}

