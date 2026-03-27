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

      shinyjs::hide("msg_no_file")
      shinyjs::toggle("msg_file_ok",    condition = rv$inputs$check_zip$all_ok)
      shinyjs::toggle("msg_file_error", condition = !rv$inputs$check_zip$all_ok)
      shinyjs::toggleState("btn_read_data", condition = rv$inputs$check_zip$all_ok)

    })

    output$file_error_detail <- renderPrint({
      req(rv$inputs$check_zip)
      if(!rv$inputs$check_zip$all_ok) {
        cat("Missing files:\n", paste(rv$inputs$check_zip$missing, collapse = ", "))
      }
    })

    ## + Acc1: read data ======
    ## . + Show progress -----
    ## Action: show progress in panel insights
    observeEvent(input$btn_read_data, {

      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_insights"))
      session$sendCustomMessage("scroll_top", list())

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
      ## All messages from fct_readzip2() — including per-file success/error lines
      ## and the final summary — are captured here and appended to the console div.
      rv$inputs$data <- withCallingHandlers(
        {
          ## fct_readzip(                                             ## replaced by fct_readzip2
          ##   .path = rv$inputs$path_zip, .pb_session = session, .pb_id = "readdata_progress"
          ## )
          fct_readzip2(
            .path = rv$inputs$path_zip, .pb_session = session, .pb_id = "readdata_progress"
          )
        },
        message = function(m) {
          shinyjs::html(id = "readdata_console", html = paste0(m$message, '<br>'), add = TRUE)
          invokeRestart("muffleMessage")
        }
      )

      ## Enable the insight button only when data loaded AND no read errors.
      ## Any error lines are already visible in the console div above.
      ## if (!is.null(rv$inputs$data)) rv$inputs$data_ok <- TRUE  ## replaced: did not check errors
      read_errors <- attr(rv$inputs$data, ".errors")
      rv$inputs$data_ok <- !is.null(rv$inputs$data) && length(read_errors) == 0

      shinyjs::toggleState("btn_data_insights", condition = rv$inputs$data_ok)

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
        rv$insights$entities_named <- stats::setNames(rv$insights$entities, rv$insights$entities_labs)
      } else {
        rv$insights$entities_named <- NULL
      }

    })

    observeEvent(input$insight_sel_entity, {
      req(rv$inputs$data$schema_summary, rv$inputs$data$chain_summary)

      rv$insights$vars <- rv$inputs$data$chain_summary$resultVariables |>
        dplyr::filter(.data$entity == stringr::str_remove(input$insight_sel_entity, "OLAP_")) |>
        dplyr::filter(.data$areaBased)
      rv$insights$vars_named <- stats::setNames(rv$insights$vars$name, rv$insights$vars$label)

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


    ## + Acc3: Run analysis ======

    ## $$$

    ## . + Entity selector — reuses entity list built by Acc2 observer ------
    output$analysis_entity <- renderUI({
      req(rv$insights$entities_named)
      selectInput(
        inputId  = ns("analysis_sel_entity"),
        label    = "Entity",
        choices  = rv$insights$entities_named,
        multiple = FALSE
      )
    })

    ## . + Dim metadata — recomputed each time entity changes ------
    observeEvent(input$analysis_sel_entity, {
      req(rv$inputs$data)

      entity <- stringr::str_remove(input$analysis_sel_entity, "OLAP_")
      rv$analysis$dim_meta <- fct_get_dim_meta(.zip = rv$inputs$data, .entity = entity)

      strat_row <- dplyr::filter(rv$analysis$dim_meta, .data$stratum)
      rv$analysis$strat_label <- if (nrow(strat_row) > 0) strat_row$label[1] else NULL

      shinyjs::disable("btn_run_analysis")
    })

    ## . + Grouped dimension selectizeInput ------
    output$analysis_dims <- renderUI({
      req(rv$analysis$dim_meta)

      make_grp <- function(is_bu) {
        sub <- dplyr::filter(rv$analysis$dim_meta,
                             .data$dimension_baseunit == is_bu, !.data$stratum)
        if (nrow(sub) == 0) return(NULL)
        stats::setNames(sub$name, sub$label)
      }
      choices <- Filter(Negate(is.null), list(
        "Base-unit dimensions" = make_grp(TRUE),
        "Sub-unit dimensions"  = make_grp(FALSE)
      ))

      ## $$$
      ## selectizeInput(
      ##   inputId  = ns("analysis_sel_dims"),
      ##   label    = "Reporting dimensions",
      ##   choices  = choices,
      ##   multiple = TRUE,
      ##   options  = list(placeholder = "Select dimensions...")
      ## )
      shinyWidgets::virtualSelectInput(
        inputId          = ns("analysis_sel_dims"),
        label            = "Reporting dimensions",
        choices          = choices,
        selected         = NULL,
        multiple         = TRUE,
        showValueAsTags = TRUE,
        search           = TRUE,
        placeholder      = "Select dimensions...",
        dropboxWrapper   = "body",
        width            = "100%"
      )
      ## $$$
    })

    ## . + Stratum note ------
    output$analysis_strat_text <- renderUI({
      req(rv$analysis$strat_label)
      div(
        class = "text-info",
        style = "font-size: 0.85em; font-style: italic; margin-top: 0.25rem;",
        bsicons::bs_icon("info-circle"),
        paste0(" '", rv$analysis$strat_label, "' will be included automatically.")
      )
    })

    ## . + Enable run button when at least one dim is selected ------
    observe({
      shinyjs::toggleState(
        id        = "btn_run_analysis",
        condition = isTruthy(input$analysis_sel_dims)
      )
    })

    ## . + Run fct_arenalyse() ------
    observeEvent(input$btn_run_analysis, {
      req(rv$inputs$data, input$analysis_sel_entity, input$analysis_sel_dims)

      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_analysis"))
      session$sendCustomMessage("scroll_top", list())

      entity <- stringr::str_remove(input$analysis_sel_entity, "OLAP_")
      dims   <- input$analysis_sel_dims

      ## Store measure metadata for plot selectors
      rv$analysis$measures_meta <- tibble::as_tibble(
        rv$inputs$data$chain_summary$resultVariables
      ) |>
        dplyr::filter(
          .data$entity == entity,
          .data$areaBased,
          .data$active,
          .data$type == "Q"
        )

      shinyjs::disable("btn_run_analysis")

      result <- tryCatch(
        withProgress(message = "Running analysis...", value = 0.5, {
          fct_arenalyse(.zip = rv$inputs$data, .entity = entity, .dim = dims)
        }),
        error = function(e) {
          shinyWidgets::sendSweetAlert(
            session = session, title = "Analysis error",
            text = e$message, type = "error"
          )
          NULL
        }
      )

      shinyjs::enable("btn_run_analysis")

      if (!is.null(result)) {
        rv$analysis$result <- result
        rv$analysis$dims   <- dims
        rv$analysis$entity <- entity
      }
    })

    ## $$$

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

    ## + Analysis outputs ======

    ## $$$

    ## . + Shared plot builder (local helper) --------------------------------
    ## Called by both MEANS and TOTALS renderPlot to avoid duplication.
    make_bar_plot <- function(df, x_dim, measure, fill_col, facet_col,
                              show_errbar, dim_meta, measures_meta,
                              extra_filter_vals, comma_y = FALSE) {

      ## $$$
      ## Apply multi-value filters for all dimensions
      for (nm in names(extra_filter_vals)) {
        vals <- extra_filter_vals[[nm]]
        if (length(vals) > 0) {
          df <- dplyr::filter(df, .data[[nm]] %in% vals)
        }
      }
      ## $$$

      use_fill  <- isTruthy(fill_col)  && fill_col  != ""
      use_facet <- isTruthy(facet_col) && facet_col != ""
      low_col   <- paste0(measure, "_low")
      upp_col   <- paste0(measure, "_upp")
      has_ci    <- all(c(low_col, upp_col) %in% names(df))

      get_lbl <- function(meta, col) {
        meta |> dplyr::filter(.data$name == col) |> dplyr::pull("label") |> dplyr::first()
      }
      x_label    <- get_lbl(dim_meta,      x_dim)
      y_label    <- get_lbl(measures_meta, measure)
      fill_label <- if (use_fill)  get_lbl(dim_meta, fill_col)  else NULL

      dodge   <- ggplot2::position_dodge(width = 0.9, preserve = "single")
      bar_pos <- if (use_fill) dodge else "identity"

      base_aes <- if (use_fill) {
        ggplot2::aes(x = .data[[x_dim]], y = .data[[measure]], fill = .data[[fill_col]])
      } else {
        ggplot2::aes(x = .data[[x_dim]], y = .data[[measure]])
      }

      p <- ggplot2::ggplot(df, base_aes) +
        ggplot2::geom_col(position = bar_pos) +
        ggplot2::labs(x = x_label, y = y_label, fill = fill_label) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      if (show_errbar && has_ci) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[[low_col]], ymax = .data[[upp_col]]),
          position = bar_pos,
          width    = 0.2
        )
      }

      if (use_facet) {
        p <- p + ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_col)),
          labeller = ggplot2::label_value
        )
      }

      ## $$$
      if (comma_y) p <- p + ggplot2::scale_y_continuous(labels = scales::comma)
      ## $$$

      p
    }

    ## . + Update plot selectors when a new result arrives ------
    observeEvent(rv$analysis$result, {
      req(rv$analysis$result, rv$analysis$dim_meta, rv$analysis$measures_meta)

      dim_meta    <- dplyr::filter(rv$analysis$dim_meta, .data$name %in% rv$analysis$dims)
      dim_choices <- stats::setNames(dim_meta$name, dim_meta$label)

      meas_meta    <- rv$analysis$measures_meta
      meas_choices <- stats::setNames(meas_meta$name, meas_meta$label)

      ## $$$
      ## None + all dims for fill/facet selectors — blank label so selectize shows placeholder
      optional_choices <- c("-- None --" = "", dim_choices)
      ## optional_choices <- c(stats::setNames("", ""), dim_choices)
      ## $$$

      updateSelectInput(session, "plot_dim",     choices = dim_choices,     selected = dim_choices[1])
      updateSelectInput(session, "plot_measure", choices = meas_choices,    selected = meas_choices[1])
      updateSelectInput(session, "plot_fill",    choices = optional_choices, selected = "")
      updateSelectInput(session, "plot_facet",   choices = optional_choices, selected = "")

      shinyjs::hide("analysis_no_result")
      shinyjs::show("analysis_results")
    })

    ## $$$
    ## . + Dimension filters -------------------------------------------------
    ## One multi-select per dimension used in the analysis (all dims, regardless
    ## of role). Default = all values selected (no filter applied).
    output$analysis_extra_filters <- renderUI({
      req(rv$analysis$result, rv$analysis$dim_meta)

      df       <- rv$analysis$result$MEANS
      dim_meta <- rv$analysis$dim_meta

      filter_inputs <- lapply(rv$analysis$dims, function(d) {
        lbl  <- dim_meta |> dplyr::filter(.data$name == d) |> dplyr::pull("label") |> dplyr::first()
        vals <- sort(unique(df[[d]]))
        ## $$$
        ## selectizeInput(
        ##   inputId  = ns(paste0("filter_dim__", d)),
        ##   label    = lbl,
        ##   choices  = stats::setNames(vals, vals),
        ##   selected = vals,
        ##   multiple = TRUE
        ## )
        shinyWidgets::virtualSelectInput(
          inputId          = ns(paste0("filter_dim__", d)),
          label            = lbl,
          choices          = stats::setNames(vals, vals),
          selected         = vals,          ## all selected by default → no filter
          multiple         = TRUE,
          showValueAsTags  = TRUE,
          search           = TRUE,
          dropboxWrapper   = "body",
          width            = "100%"
        )
        ## $$$
      })

      if (length(filter_inputs) == 0) return(NULL)

      div(
        style = "margin-top: 0.75rem; border-top: 1px solid #dee2e6; padding-top: 0.75rem;",
        tags$small(class = "text-muted", bsicons::bs_icon("funnel"), " Dimension filters"),
        layout_column_wrap(width = "180px", fill = FALSE, !!!filter_inputs)
      )
    })

    ## . + Helper: collect current filter values for all dims ----------------
    ## (replaces get_extra_filter_vals — covers every dim, not just unallocated)
    get_filter_vals <- function() {
      req(rv$analysis$dims)
      lapply(
        stats::setNames(rv$analysis$dims, rv$analysis$dims),
        function(d) input[[paste0("filter_dim__", d)]]
      )
    }
    ## $$$

    ## . + MEANS bar plot ----------------------------------------------------
    output$analysis_plot_means <- renderPlot({
      req(rv$analysis$result, input$plot_dim, input$plot_measure)
      make_bar_plot(
        df                = rv$analysis$result$MEANS,
        x_dim             = input$plot_dim,
        measure           = input$plot_measure,
        fill_col          = input$plot_fill,
        facet_col         = input$plot_facet,
        show_errbar       = isTRUE(input$plot_errbar),
        dim_meta          = rv$analysis$dim_meta,
        measures_meta     = rv$analysis$measures_meta,
        ## $$$
        extra_filter_vals = get_filter_vals()
        ## $$$
      )
    })

    ## . + TOTALS bar plot ---------------------------------------------------
    output$analysis_plot_totals <- renderPlot({
      req(rv$analysis$result, input$plot_dim, input$plot_measure)
      make_bar_plot(
        df                = rv$analysis$result$TOTALS,
        x_dim             = input$plot_dim,
        measure           = input$plot_measure,
        fill_col          = input$plot_fill,
        facet_col         = input$plot_facet,
        show_errbar       = isTRUE(input$plot_errbar),
        dim_meta          = rv$analysis$dim_meta,
        measures_meta     = rv$analysis$measures_meta,
        ## $$$
        extra_filter_vals = get_filter_vals(),
        comma_y           = TRUE
        ## $$$
      )
    })

    ## $$$

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

