#' Tool module server function
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server2 <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Backward compatibility prefix of entity tables in the ZIP file
    .ep <- "MAU_"

    ## !!! FOR TESTING ONLY
    # rv <- list()
    # rv$inputs <- list()
    # # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics")
    # rv$inputs$path_zip <- "/Users/gaelsola/syncwork/FAO-2026/support/support-arenalytics/MAU_Shiny_(png_nfi_2024_upperplant) 1.zip"
    # rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip, .entity_prefix = .ep)
    # rv$inputs      <- fct_readzip2(.path = rv$inputs$path_zip, .entity_prefix = .ep)
    # input <- list()
    # input$analysis_sel_entity <-  input$insight_sel_entity  <- "tree"
    # input$analysis_sel_dims <- c("cluster_forest_type", "dbh_up100_10cm")
    # result <- fct_arenalyse(.zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = input$analysis_sel_dims)
    # rv$insights <- list()
    ## !!!



    ##
    ## Accordions outputs and events ######
    ##

    ## + Acc1: check data ======
    ## Action 1: (1) check data files list, (2) update message and (3) active read button
    observeEvent(input$load_zip, {

      rv$inputs$path_zip <- input$load_zip$datapath
      rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip, .entity_prefix = .ep)

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
    ## . + Show progress and insights -----
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
      rv$inputs <- withCallingHandlers(
        {
          fct_readzip2(
            .path = rv$inputs$path_zip, .pb_session = session, .pb_id = "readdata_progress", .entity_prefix = .ep
          )
        },
        message = function(m) {
          shinyjs::html(id = "readdata_console", html = paste0(m$message, '<br>'), add = TRUE)
          invokeRestart("muffleMessage")
        }
      )

      ## Enable the insight button only when data loaded AND no read errors.
      ## Any error lines are already visible in the console div above.
      rv$inputs$data_ok <- !is.null(rv$inputs$data) && length(rv$inputs$errors) == 0

      shinyjs::toggleState("btn_data_insights", condition = rv$inputs$data_ok)

    })


    ## . +  Show insights ------
    observeEvent(input$btn_data_insights, {
      req(rv$inputs$data)

      ## Hide progress and show insights
      shinyjs::hide("panel_insight_progress")
      shinyjs::show("panel_insights")
    })

    ## + Entities observer — shared by Insights panel and Acc3 ======
    observe({
      req(rv$inputs$data)

      if (length(names(rv$inputs$data)) > 0) {
        ## Convention entities = 'tree' etc., entities_labs = 'Tree', fetch entity data -> 'MAU_tree'
        rv$insights$entities       <- names(rv$inputs$data) |> stringr::str_subset(.ep) |> stringr::str_remove(.ep)
        rv$insights$entities_labs  <- rv$insights$entities # Labs to be done
        rv$insights$entities_named <- stats::setNames(rv$insights$entities, rv$insights$entities_labs)

      } else {
        rv$insights$entities_named <- NULL
      }
    })

    ## $$$
    ## Acc2 entity/var selectors — removed (selection moved into Insights panel cards)
    ##
    ## observeEvent(input$insight_sel_entity, { ... })
    ## output$insight_entity <- renderUI({ ... })
    ## output$insight_vars   <- renderUI({ ... })
    ## $$$


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

      rv$analysis$dim_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]]

      ## !!! TO INSPECT FURTHER !!!
      strat_row <- rv$analysis$dim_meta |> dplyr::filter(.data$stratum)
      rv$analysis$strat_label <- if (nrow(strat_row) > 0) strat_row$label[1] else NULL

      shinyjs::disable("btn_run_analysis")
    })

    ## . + Dimension checkboxes (base-unit then sub-unit) ------
    output$analysis_dims <- renderUI({
      req(rv$analysis$dim_meta)

      make_grp <- function(is_bu) {
        sub <- rv$analysis$dim_meta |>
          dplyr::filter(.data$report_type == "dimension",
                        .data$dimension_baseunit == is_bu)
        if (nrow(sub) == 0) return(character(0))
        stats::setNames(sub$name, sub$label)
      }

      bu_choices  <- make_grp(TRUE)
      sub_choices <- make_grp(FALSE)

      tagList(
        tags$label(class = "form-label", "Base-unit dimensions"),
        shinyWidgets::checkboxGroupButtons(
          inputId    = ns("analysis_bu_dims"),
          label      = NULL,
          choices    = if (length(bu_choices) > 0) bu_choices else character(0),
          individual = TRUE,
          size       = "sm"
        ),
        if (length(sub_choices) > 0) tagList(
          hr(style = "margin: 0.5rem 0;"),
          tags$label(class = "form-label", "Sub-unit dimensions"),
          shinyWidgets::checkboxGroupButtons(
            inputId    = ns("analysis_sub_dims"),
            label      = NULL,
            choices    = sub_choices,
            individual = TRUE,
            size       = "sm"
          )
        )
      )
    })

    ## . + Too-many-dims warning ------
    output$analysis_too_many_dims <- renderUI({
      n <- length(c(input$analysis_bu_dims, input$analysis_sub_dims))
      if (n <= 4) return(NULL)
      div(
        class = "text-warning",
        style = "font-size: 0.85em; font-style: italic; margin-top: 0.25rem;",
        bsicons::bs_icon("exclamation-triangle"),
        " More than 4 dimensions selected, computation may be slow."
      )
    })

    ## . + Stratum note ------
    output$analysis_strat_text <- renderUI({
      req(rv$analysis$strat_label)
      div(
        class = "text-info",
        style = "font-size: 0.85em; font-style: italic; margin-top: 0.25rem;",
        bsicons::bs_icon("info-circle"),
        # paste0(" '", rv$analysis$strat_label, "' will be included automatically.")
        paste0(" '", rv$analysis$strat_label, "' is used for stratification.")
      )
    })

    ## . + Enable run button when at least one dim is selected ------
    observe({
      shinyjs::toggleState(
        id        = "btn_run_analysis",
        condition = isTruthy(input$analysis_bu_dims) || isTruthy(input$analysis_sub_dims)
      )
    })

    ## ++ ##
    ## . + Run fct_arenalyse() ------
    observeEvent(input$btn_run_analysis, {
      dims_sel <- c(input$analysis_bu_dims, input$analysis_sub_dims)
      req(rv$inputs$data, input$analysis_sel_entity, dims_sel)

      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_analysis"))
      session$sendCustomMessage("scroll_top", list())


      ## Replaced with meta generated from readzip2()
      ## Store measure metadata for plot selectors
      # rv$analysis$measures_meta <- tibble::as_tibble(
      #   rv$inputs$data$chain_summary$resultVariables
      # ) |>
      #   dplyr::filter(
      #     .data$entity == entity,
      #     .data$areaBased,
      #     .data$active,
      #     .data$type == "Q"
      #   )

      ## Measures info for plot selector
      rv$analysis$measures_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]] |>
        dplyr::filter(report_type == "measure")

      shinyjs::disable("btn_run_analysis")
      shinyjs::hide("analysis_no_result")
      shinyjs::hide("analysis_results")
      shinyjs::show("analysis_progress")
      shinyjs::html("analysis_console", "")
      shinyWidgets::updateProgressBar(
        session = session,
        id = "analysis_progress_bar",
        value = 0
      )

      result <- tryCatch(
        withCallingHandlers(
          {
            fct_arenalyse(
              .zip = rv$inputs$data,
              .entity = input$analysis_sel_entity,
              .dim = dims_sel,
              .pb_session = session,
              .pb_id = "analysis_progress_bar"
            )
          },
          message = function(m) {
            shinyjs::html(id = "analysis_console", html = paste0(m$message, '<br>'), add = TRUE)
            invokeRestart("muffleMessage")
          }
        ),
        error = function(e) {
          shinyjs::hide("analysis_progress")
          shinyjs::toggle("analysis_results", condition = !is.null(rv$analysis$result))
          shinyjs::toggle("analysis_no_result", condition = is.null(rv$analysis$result))
          shinyWidgets::sendSweetAlert(
            session = session, title = "Analysis error",
            text = e$message, type = "error"
          )
          NULL
        }
      )

      shinyjs::enable("btn_run_analysis")

      if (!is.null(result)) {
        lang <- rv$inputs$data$chain_summary$selectedLanguage %||% "en"
        dim_meta  <- rv$analysis$dim_meta
        cats      <- rv$inputs$data$categories

        result$MEANS  <- replace_dim_labels(result$MEANS,  dim_meta, cats, lang)
        result$TOTALS <- replace_dim_labels(result$TOTALS, dim_meta, cats, lang)

        rv$analysis$result <- result
        rv$analysis$dims   <- dims_sel
        rv$analysis$entity <- input$analysis_sel_entity

        shinyjs::hide("analysis_progress")
      }
    })
    ## ++ ##

    ## $$$


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

    ## $$$
    ## output$insight_chain / insight_summary / insight_entity_rows — all replaced below.

    ## . + Populate entity dropdown when data loads ------
    observe({
      req(rv$insights$entities_named)
      updateSelectInput(session, "insight_sel_entity", choices = rv$insights$entities_named)
    })

    ## ++ ##
    ## . + Populate the insight controls when entity changes ------
    observeEvent(input$insight_sel_entity, {
      req(rv$inputs$data)

      ## Get dims and measures from meta
      # entity   <- stringr::str_remove(input$insight_sel_entity, "OLAP_")
      # dim_meta <- fct_varinfo(.zip = rv$inputs$data, .entity = entity)
      #
      # measures_meta <- tibble::as_tibble(rv$inputs$data$chain_summary$resultVariables) |>
      #   dplyr::filter(
      #     .data$entity == entity,
      #     .data$areaBased,
      #     .data$active,
      #     .data$type == "Q"
      #   )
      # bu_choices   <- with(dplyr::filter(dim_meta, .data$dimension_baseunit == TRUE,  !.data$stratum),
      #                      stats::setNames(name, label))
      # sub_choices  <- with(dplyr::filter(dim_meta, .data$dimension_baseunit == FALSE, !.data$stratum),
      #                      stats::setNames(name, label))
      # meas_choices <- with(measures_meta, stats::setNames(name, label))

      dim_meta_bu <- rv$inputs$var_meta[[input$insight_sel_entity]] |>
        dplyr::filter(report_type == "dimension", dimension_baseunit)
      dim_meta_sub <- rv$inputs$var_meta[[input$insight_sel_entity]] |>
        dplyr::filter(report_type == "dimension", !dimension_baseunit)
      meas_meta <- rv$inputs$var_meta[[input$insight_sel_entity]] |>
        dplyr::filter(report_type == "measure")

      entity_name <- stringr::str_subset(names(rv$inputs$data), input$insight_sel_entity)

      ## Store choices and entity table for use in summary outputs
      rv$insights$bu_choices    <- stats::setNames(dim_meta_bu$name, dim_meta_bu$label)
      rv$insights$sub_choices   <- stats::setNames(dim_meta_sub$name, dim_meta_sub$label)
      rv$insights$meas_choices  <- stats::setNames(meas_meta$name, meas_meta$label)
      rv$insights$dim_meta      <- rv$inputs$var_meta[[input$insight_sel_entity]]
      rv$insights$entity_table  <- rv$inputs$data[[entity_name]] |> tibble::as_tibble()

      shinyWidgets::updateCheckboxGroupButtons(session, "insight_bu_sel",   choices = rv$insights$bu_choices,   selected = character(0))
      shinyWidgets::updateCheckboxGroupButtons(session, "insight_sub_sel",  choices = rv$insights$sub_choices,  selected = character(0))
    })
    ## ++ ##

    ## ++ ##
    ## . + Summary outputs — right column of each insight card ------

    ## Helper: placeholder shown when nothing is selected
    insight_no_sel <- tags$p(
      class = "text-muted fst-italic",
      style = "font-size: 0.85em;",
      "No selection."
    )

    get_dim_label_lookup <- function(meta_row, categories, lang) {
      cat_name <- meta_row$categoryName[[1]]

      if (is.na(cat_name) || !nzchar(cat_name) || is.null(categories[[cat_name]])) {
        return(NULL)
      }

      label_col <- paste0("label_", lang)
      cat_tbl <- tibble::as_tibble(categories[[cat_name]])
      lbl_col <- if (label_col %in% names(cat_tbl)) label_col else "label"

      stats::setNames(
        as.character(cat_tbl[[lbl_col]]),
        as.character(cat_tbl$code)
      )
    }

    make_dim_summary <- function(sel, meta, tbl, categories, lang) {
      if (is.null(sel) || length(sel) == 0) return(insight_no_sel)

      sections <- purrr::map(sel, function(v) {
        meta_row <- meta |>
          dplyr::filter(.data$name == v) |>
          dplyr::slice(1)

        values_raw <- tbl[[v]]
        has_na <- any(is.na(values_raw) | values_raw == "")

        lookup <- get_dim_label_lookup(meta_row, categories, lang)

        values_clean <- values_raw |>
          as.character() |>
          (\(x) x[!is.na(x) & nzchar(x)])() |>
          unique() |>
          sort()

        if (!is.null(lookup)) {
          values_clean <- dplyr::coalesce(
            unname(lookup[values_clean]),
            values_clean
          )
        }

        tags$div(
          style = "margin-bottom: 0.85rem;",
          tags$div(
            tags$strong(meta_row$label[[1]]),
            if (has_na) tags$span(
              " NA present",
              class = "badge text-bg-danger",
              style = "margin-left: 0.5rem;"
            )
          ),
          if (length(values_clean) > 0) {
            tags$div(
              style = "font-size: 0.85em; margin-top: 0.35rem;",
              paste(values_clean, collapse = ", ")
            )
          } else {
            tags$p(
              class = "text-muted fst-italic",
              style = "font-size: 0.85em; margin-top: 0.35rem;",
              "No non-missing classes."
            )
          }
        )
      })

      tagList(sections)
    }

    make_meas_summary <- function(choices) {
      if (is.null(choices) || length(choices) == 0) {
        return(tags$p(
          class = "text-muted fst-italic",
          style = "font-size: 0.85em;",
          "No measures available."
        ))
      }

      tags$ul(
        style = "font-size: 0.85em; margin-bottom: 0;",
        purrr::map(names(choices), tags$li)
      )
    }

    output$insight_bu_out <- renderUI({
      req(rv$insights$bu_choices, rv$insights$entity_table, rv$insights$dim_meta, rv$inputs$data$categories)
      make_dim_summary(
        sel = input$insight_bu_sel,
        meta = rv$insights$dim_meta,
        tbl = rv$insights$entity_table,
        categories = rv$inputs$data$categories,
        lang = rv$inputs$data$chain_summary$selectedLanguage %||% "en"
      )
    })

    output$insight_sub_out <- renderUI({
      req(rv$insights$sub_choices, rv$insights$entity_table, rv$insights$dim_meta, rv$inputs$data$categories)
      make_dim_summary(
        sel = input$insight_sub_sel,
        meta = rv$insights$dim_meta,
        tbl = rv$insights$entity_table,
        categories = rv$inputs$data$categories,
        lang = rv$inputs$data$chain_summary$selectedLanguage %||% "en"
      )
    })

    output$insight_meas_out <- renderUI({
      req(rv$insights$meas_choices)
      make_meas_summary(rv$insights$meas_choices)
    })
    ## ++ ##

    ## + Analysis outputs ======

    ## $$$

    ## . + Label replacement helper ------------------------------------------
    ## Replaces dimension codes with human-readable labels from categories list.
    ## Iterates over dimension columns present in df; looks up categoryName from
    ## dim_meta to find the right category table, then maps code → label.
    replace_dim_labels <- function(df, dim_meta, categories, lang = "en") {
      label_col <- paste0("label_", lang)
      dim_cols  <- intersect(
        dplyr::filter(dim_meta, .data$report_type == "dimension") |> dplyr::pull("name"),
        names(df)
      )
      purrr::reduce(dim_cols, \(acc, col) {
        cat_name <- dim_meta |>
          dplyr::filter(.data$name == col) |>
          dplyr::pull("categoryName") |>
          dplyr::first()
        if (is.na(cat_name) || !nzchar(cat_name)) return(acc)
        cat_tbl <- categories[[cat_name]]
        if (is.null(cat_tbl)) return(acc)
        lbl_col <- if (label_col %in% names(cat_tbl)) label_col else "label"
        lookup  <- stats::setNames(
          as.character(cat_tbl[[lbl_col]]),
          as.character(cat_tbl$code)
        )
        dplyr::mutate(acc, !!col := dplyr::coalesce(unname(lookup[as.character(.data[[col]])]),
                                                     as.character(.data[[col]])))
      }, .init = df)
    }

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

      ## ++ ##
      filter_inputs <- purrr::map(rv$analysis$dims, function(d) {
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
      ## ++ ##

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
      ## ++ ##
      purrr::map(
        stats::setNames(rv$analysis$dims, rv$analysis$dims),
        function(d) input[[paste0("filter_dim__", d)]]
      )
      ## ++ ##
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

  })

}
