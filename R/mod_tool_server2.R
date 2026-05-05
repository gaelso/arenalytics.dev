#' Tool module server function
#'
#' @importFrom rlang .data :=
#'
#' @noRd
mod_tool_server2 <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Backward compatibility prefix of entity tables in the ZIP file
    .ep <- "MAU_"

    ## !!! FOR TESTING: see tests/test-tool-server.R
    
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

      ## Disable Read data button (second clcik triggers error message)
      shinyjs::disable("btn_read_data")

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
            .path = rv$inputs$path_zip, .pb_ss = session, .pb_id = "readdata_progress", .entity_prefix = .ep
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

    ## + Acc3: Run analysis ======

    ## . + Entities observer — shared by Insights panel and Acc3 ------
    observe({
      req(rv$inputs$data)

      if (length(names(rv$inputs$data)) > 0) {
        ## Convention entities = 'tree' etc., entities_labs = 'Tree'
        rv$insights$entities  <- names(rv$inputs$data) |>
          stringr::str_subset(.ep) |>
          stringr::str_remove(.ep)

        entity_lang <- rv$inputs$data$schema_summary |>
          dplyr::as_tibble() |>
          dplyr::filter(.data$type == "entity") |>
          dplyr::select("name", dplyr::starts_with("label"))

        rv$insights$entities_labs  <- utils_find_label(
          .df = entity_lang,
          .name = rv$insights$entities,
          .lang = rv$inputs$data$chain_summary$selectedLanguage
        )
        rv$insights$entities_named <- stats::setNames(
          # c("area", rv$insights$entities), c("Area", rv$insights$entities_labs)
          rv$insights$entities, rv$insights$entities_labs
        )

      } else {
        rv$insights$entities_named <- NULL
      }
    })

    ## . + Entity selector — reuses entity list built by Acc2 observer ------
    output$analysis_entity <- renderUI({
      req(rv$insights$entities_named)
      selectInput(
        inputId  = ns("analysis_sel_entity"),
        label    = strong("Entity"),
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

    observeEvent(input$analysis_mode, {
      shinyjs::disable("btn_run_analysis")
    })

    ## . + Dimension checkboxes (base-unit then sub-unit) ------
    output$analysis_dims <- renderUI({
      req(rv$analysis$dim_meta, input$analysis_mode)

      bu_choices  <- utils_make_grp(.rv = rv, .is_bu = TRUE)
      sub_choices <- utils_make_grp(.rv = rv, .is_bu = FALSE)

      tagList(
        tags$label(class = "form-label", strong("Base-unit dimensions")),
        shinyWidgets::checkboxGroupButtons(
          inputId    = ns("analysis_bu_dims"),
          label      = NULL,
          choices    = if (length(bu_choices) > 0) bu_choices else character(0),
          individual = TRUE,
          size       = "sm"
        ),
        if (input$analysis_mode != "area" && length(sub_choices) > 0) tagList(
          hr(style = "margin: 0.5rem 0;"),
          tags$label(class = "form-label", strong("Sub-unit dimensions")),
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
      n <- if (identical(input$analysis_mode, "area")) {
        length(input$analysis_bu_dims)
      } else {
        length(c(input$analysis_bu_dims, input$analysis_sub_dims))
      }
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
      req(!identical(input$analysis_mode, "area"))
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
      rv$analysis$dims_sel <- if (identical(input$analysis_mode, "area")) {
        input$analysis_bu_dims
      } else {
        c(input$analysis_bu_dims, input$analysis_sub_dims)
      }

      shinyjs::toggleState(
        id        = "btn_run_analysis",
        condition = isTruthy(rv$analysis$dims_sel)
      )
    })

    ## . + Run fct_arenalyse() ------
    observeEvent(input$btn_run_analysis, {
      # dims_sel <- if (identical(input$analysis_mode, "area")) {
      #   input$analysis_bu_dims
      # } else {
      #   c(input$analysis_bu_dims, input$analysis_sub_dims)
      # }
      # req(rv$inputs$data, input$analysis_sel_entity, dims_sel)
      req(rv$inputs$data, input$analysis_sel_entity, rv$analysis$dims_sel)

      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "tab_analysis"))
      session$sendCustomMessage("scroll_top", list())


      ## Measures info for plot selector
      if (identical(input$analysis_mode, "area")) {
        rv$analysis$measures_meta <- tibble::tibble(
          name = "area",
          label = "Area"
        )
      } else {
        rv$analysis$measures_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]] |>
          dplyr::filter(.data$report_type == "measure")
      }

      shinyjs::disable("btn_run_analysis")
      shinyjs::hide("analysis_no_result")
      shinyjs::hide("analysis_results")
      shinyjs::show("analysis_progress")
      shinyjs::html("analysis_console", "")
      shinyjs::disable("btn_analysis_results")
      shinyWidgets::updateProgressBar(
        session = session,
        id = "analysis_progress_bar",
        value = 0
      )

      result <- tryCatch(
        withCallingHandlers(
          {
            if (identical(input$analysis_mode, "area")) {
              message(sprintf(
                "[%s] Preparing area summary.",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              shinyWidgets::updateProgressBar(
                session = session,
                id = "analysis_progress_bar",
                value = 30
              )

              area_result <- build_area_result(
                .zip = rv$inputs$data,
                .entity = input$analysis_sel_entity,
                .dim = rv$analysis$dims_sel,
                .entity_prefix = .ep
              )

              message(sprintf(
                "[%s] Area summary completed.",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              shinyWidgets::updateProgressBar(
                session = session,
                id = "analysis_progress_bar",
                value = 100
              )

              area_result
            } else {
              fct_arenalyse(
                .zip    = rv$inputs$data,
                .entity = input$analysis_sel_entity,
                .dim    = rv$analysis$dims_sel,
                .pvalue = as.numeric(input$analysis_p_value),
                .cm     = input$analysis_compute_mode,
                .lonely = input$analysis_lonely_psu,
                .pb_ss  = session,
                .pb_id  = "analysis_progress_bar"
              )

            }
          },
          message = function(m) {
            shinyjs::html(id = "analysis_console", html = paste0(m$message, '<br>'), add = TRUE)
            invokeRestart("muffleMessage")
          }
        ),
        error = function(e) {
          shinyjs::disable("btn_analysis_results")
          # shinyjs::hide("analysis_progress")
          # shinyjs::toggle("analysis_results", condition = !is.null(rv$analysis$result))
          # shinyjs::toggle("analysis_no_result", condition = is.null(rv$analysis$result))
          shinyWidgets::sendSweetAlert(
            session = session, title = "Analysis error",
            text = e$message, type = "error"
          )
          NULL
        }
      )

      shinyjs::enable("btn_run_analysis")

      if (!is.null(result)) {
        # lang <- rv$inputs$data$chain_summary$selectedLanguage %||% "en"
        lang <- rv$inputs$data$chain_summary$selectedLanguage
        dim_meta  <- rv$analysis$dim_meta
        cats      <- rv$inputs$data$categories

        result$MEANS  <- replace_dim_labels(result$MEANS,  dim_meta, cats, lang)
        result$TOTALS <- replace_dim_labels(result$TOTALS, dim_meta, cats, lang)

        if (!identical(input$analysis_mode, "area")) {
          result$MEANS  <- result$MEANS  |> dplyr::select(-dplyr::any_of(c("area", "JOIN_COL")))
          result$TOTALS <- result$TOTALS |> dplyr::select(-dplyr::any_of(c("area", "JOIN_COL")))
        }

        rv$analysis$result <- result
        rv$analysis$dims   <- rv$analysis$dims_sel
        rv$analysis$entity <- input$analysis_sel_entity
        rv$analysis$mode   <- input$analysis_mode

        shinyjs::enable("btn_analysis_results")
      }
    })

    observeEvent(input$btn_analysis_results, {
      req(rv$analysis$result)
      shinyjs::hide("analysis_progress")
      shinyjs::hide("analysis_no_result")
      sync_analysis_result_ui()
      shinyjs::show("analysis_results")
    })


    ##
    ## Panel outputs ######
    ##

    ## + Insights outputs ======

    ## . + Survey title ------
    output$insight_title <- renderText({
      req(rv$inputs$data)
      rv$inputs$data$chain_summary$surveyLabel
    })

    ## . + Chain summary info block ------
    output$insight_chain_info <- renderUI({
      req(rv$inputs$data)
      ch <- rv$inputs$data$chain_summary

      info_row <- function(label, value) {
        tags$p(
          style = "margin: 0.15rem 0;",
          tags$strong(paste0(label, ": ")), value
        )
      }

      is_clustered  <- nzchar(ch$clusteringEntity %||% "")
      cluster_attr  <- if (is_clustered) {
        paste(ch$clusteringEntityKeys %||% "-", collapse = ", ")
      } else {
        "-"
      }

      tags$div(
        info_row("Survey code",                    ch$surveyName %||% "-"),
        info_row("Cycle number",                   ch$selectedCycle %||% "-"),
        info_row("Sampling strategy code",         ch$samplingStrategy %||% "-"),
        info_row("Stratification attribute",       ch$stratumAttribute %||% "-"),
        info_row("Clustering",                     if (is_clustered) "Yes" else "No"),
        info_row("Clustering attribute",           cluster_attr),
        info_row("Non-response bias correction",   "Unknown")
      )
    })

    ## $$$
    ## output$insight_chain / insight_summary / insight_entity_rows — all replaced below.

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
        has_na <- any(is.na(values_raw))
        has_empty <- any(!is.na(values_raw) & values_raw == "")

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
            if (has_empty) tags$span(
              " Empty class present",
              class = "badge text-bg-warning",
              style = "margin-left: 0.5rem;"
            ),
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

    get_current_insight_context <- function() {
      req(rv$inputs$data, input$analysis_sel_entity)

      entity_name <- paste0(.ep, input$analysis_sel_entity)
      dim_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]]
      entity_table <- rv$inputs$data[[entity_name]] |> tibble::as_tibble()

      list(
        dim_meta = dim_meta,
        entity_table = entity_table,
        base_dims = input$analysis_bu_dims %||% character(0),
        sub_dims = if (identical(input$analysis_mode, "area")) character(0) else input$analysis_sub_dims %||% character(0),
        meas_choices = if (identical(input$analysis_mode, "area")) {
          c(Area = "area")
        } else {
          meas_meta <- dim_meta |> dplyr::filter(.data$report_type == "measure")
          stats::setNames(meas_meta$name, meas_meta$label)
        }
      )
    }

    output$insight_current_selection <- renderUI({
      req(rv$inputs$data)

      if (!isTruthy(input$analysis_sel_entity)) {
        return(tags$p(
          class = "text-muted fst-italic",
          "Choose an entity and dimensions in 'Run analysis' to inspect the current selection."
        ))
      }

      selected_dims <- c(input$analysis_bu_dims %||% character(0), input$analysis_sub_dims %||% character(0))
      selection_text <- if (length(selected_dims) == 0) "No dimensions selected." else paste(selected_dims, collapse = ", ")

      tags$div(
        if (!identical(input$analysis_mode, "area")) tags$p(
          tags$strong("Entity: "),
          input$analysis_sel_entity
        ),
        tags$p(
          tags$strong("Analysis type: "),
          if (identical(input$analysis_mode, "area")) "Area" else "Other measures"
        ),
        tags$p(
          tags$strong("Selected dimensions: "),
          selection_text
        )
      )
    })

    output$insight_bu_out <- renderUI({
      ctx <- get_current_insight_context()
      make_dim_summary(
        sel = ctx$base_dims,
        meta = ctx$dim_meta,
        tbl = ctx$entity_table,
        categories = rv$inputs$data$categories,
        lang = rv$inputs$data$chain_summary$selectedLanguage %||% "en"
      )
    })

    output$insight_sub_out <- renderUI({
      ctx <- get_current_insight_context()
      make_dim_summary(
        sel = ctx$sub_dims,
        meta = ctx$dim_meta,
        tbl = ctx$entity_table,
        categories = rv$inputs$data$categories,
        lang = rv$inputs$data$chain_summary$selectedLanguage %||% "en"
      )
    })

    output$insight_meas_out <- renderUI({
      ctx <- get_current_insight_context()
      make_meas_summary(ctx$meas_choices)
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
                              show_errbar, flip_coords, wrap_labels, hide_legend,
                              dim_meta, measures_meta,
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
      wrap_lab <- function(x, width = 20) {
        if (isTRUE(wrap_labels)) {
          stringr::str_wrap(as.character(x), width = width)
        } else {
          as.character(x)
        }
      }

      x_label    <- wrap_lab(get_lbl(dim_meta,      x_dim), width = 28)
      y_label    <- wrap_lab(get_lbl(measures_meta, measure), width = 28)
      fill_label <- if (use_fill) wrap_lab(get_lbl(dim_meta, fill_col), width = 24) else NULL

      dodge   <- ggplot2::position_dodge(width = 0.9, preserve = "single")
      bar_pos <- if (use_fill) dodge else "identity"

      base_aes <- if (use_fill) {
        ggplot2::aes(x = .data[[x_dim]], y = .data[[measure]], fill = .data[[fill_col]])
      } else {
        ggplot2::aes(x = .data[[x_dim]], y = .data[[measure]])
      }

      geom_col_layer <- if (use_fill) {
        ggplot2::geom_col(position = bar_pos, col = "grey30")
      } else {
        ggplot2::geom_col(position = bar_pos, col = "grey30", fill = "grey70")
      }

      p <- ggplot2::ggplot(df, base_aes) +
        geom_col_layer +
        ggplot2::labs(x = x_label, y = y_label, fill = fill_label) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 10)),
          axis.title.y = ggplot2::element_text(size = 14, margin = ggplot2::margin(r = 10)),
          legend.title = ggplot2::element_text(size = 13),
          legend.text = ggplot2::element_text(size = 11),
          strip.text = ggplot2::element_text(size = 12, face = "bold")
        ) +
        ggplot2::scale_x_discrete(labels = \(x) wrap_lab(x, width = 18))

      if (use_fill) {
        p <- p + ggplot2::scale_fill_discrete(labels = \(x) wrap_lab(x, width = 18))
      }

      if (show_errbar && has_ci) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[[low_col]], ymax = .data[[upp_col]]),
          position = bar_pos,
          width    = 0.2,
          col = "black"
        )
      }

      if (use_facet) {
        p <- p + ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_col)),
          labeller = ggplot2::labeller(.default = ggplot2::label_wrap_gen(width = 18))
        )
      }

      if (isTRUE(flip_coords)) {
        p <- p + ggplot2::coord_flip()
      }

      if (isTRUE(hide_legend)) {
        p <- p + ggplot2::theme(legend.position = "none")
      }

      ## $$$
      if (comma_y) p <- p + ggplot2::scale_y_continuous(labels = scales::comma)
      ## $$$

      p
    }

    ## ++ ##
    build_area_result <- function(.zip, .entity, .dim, .entity_prefix = .ep) {
      chain <- .zip$chain_summary
      base_uuid <- paste0(chain$baseUnit, "_uuid")
      entity_tbl <- .zip[[paste0(.entity_prefix, .entity)]] |> tibble::as_tibble()

      if ("OLAP_baseunit_total" %in% names(entity_tbl)) {
        entity_tbl <- entity_tbl |> dplyr::rename(mau_baseunit_total = "OLAP_baseunit_total")
      }

      area_df <- entity_tbl |>
        dplyr::filter(.data$mau_baseunit_total == TRUE, .data$weight > 0, .data$exp_factor_ != 0) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(.dim), as.character)) |>
        dplyr::select(dplyr::all_of(c(base_uuid, .dim)), "exp_factor_") |>
        dplyr::distinct() |>
        dplyr::summarise(area = sum(.data$exp_factor_), .by = dplyr::all_of(.dim))

      list(MEANS = area_df, TOTALS = area_df)
    }

    filtered_analysis_df <- function(source = c("MEANS", "TOTALS")) {
      source <- match.arg(source)

      req(rv$analysis$result)

      df <- rv$analysis$result[[source]]
      filters <- tryCatch(get_filter_vals(), error = function(e) NULL)

      if (is.null(filters)) {
        return(df)
      }

      for (nm in names(filters)) {
        vals <- filters[[nm]]
        if (length(vals) > 0) {
          df <- dplyr::filter(df, .data[[nm]] %in% vals)
        }
      }

      df
    }

    table_display_df <- function(source = c("MEANS", "TOTALS")) {
      source <- match.arg(source)

      df <- filtered_analysis_df(source)
      dim_names <- rv$analysis$dims %||% character(0)
      measure_names <- rv$analysis$measures_meta |> dplyr::pull("name")
      selected_measure <- input$analysis_sel_measure %||% measure_names[1]

      measure_cols <- names(df)[
        purrr::map_lgl(
          names(df),
          \(col) stringr::str_detect(col, paste0("^", selected_measure, "($|_)"))
        )
      ]

      other_cols <- setdiff(names(df), c(dim_names, unlist(
        purrr::map(measure_names, \(m) names(df)[stringr::str_detect(names(df), paste0("^", m, "($|_)"))])
      )))

      keep_cols <- unique(c(dim_names, other_cols, measure_cols))

      dplyr::select(df, dplyr::all_of(keep_cols))
    }

    label_analysis_table_columns <- function(df) {
      req(rv$analysis$dim_meta, rv$analysis$measures_meta)

      dim_lookup <- rv$analysis$dim_meta |>
        dplyr::filter(.data$report_type == "dimension") |>
        (\(x) stats::setNames(x$label, x$name))()

      selected_measure <- input$analysis_sel_measure %||%
        (rv$analysis$measures_meta |> dplyr::pull("name") |> dplyr::first())
      measure_label <- rv$analysis$measures_meta |>
        dplyr::filter(.data$name == selected_measure) |>
        dplyr::pull("label") |>
        dplyr::first()

      fixed_lookup <- c(
        area = "Area",
        item_count = "Item count",
        base_unit_count = "Base unit count",
        cluster_count = "Cluster count"
      )

      nice_names <- purrr::map_chr(names(df), function(col) {
        if (col %in% names(dim_lookup)) {
          return(dim_lookup[[col]])
        }
        if (col %in% names(fixed_lookup)) {
          return(fixed_lookup[[col]])
        }
        if (identical(col, selected_measure)) {
          return(measure_label)
        }
        if (identical(col, paste0(selected_measure, "_se"))) {
          return(paste0(measure_label, " (SE)"))
        }
        if (identical(col, paste0(selected_measure, "_low"))) {
          return(paste0(measure_label, " (Lower CI)"))
        }
        if (identical(col, paste0(selected_measure, "_upp"))) {
          return(paste0(measure_label, " (Upper CI)"))
        }
        col
      })

      stats::setNames(df, nice_names)
    }

    analysis_result_mode <- function() {
      rv$analysis$mode %||% "other"
    }

    survey_download_stub <- function() {
      req(rv$inputs$data)
      safe_file_stub(rv$inputs$data$chain_summary$surveyName %||% "survey")
    }

    analysis_download_name <- function(output_name, ext) {
      paste0("AA-", survey_download_stub(), "-", output_name, ".", ext)
    }

    selected_measure_name <- function() {
      req(rv$analysis$measures_meta)
      input$analysis_sel_measure %||%
        (rv$analysis$measures_meta |> dplyr::pull("name") |> dplyr::first())
    }

    selected_measure_label <- function() {
      req(rv$analysis$measures_meta)
      rv$analysis$measures_meta |>
        dplyr::filter(.data$name == selected_measure_name()) |>
        dplyr::pull("label") |>
        dplyr::first()
    }

    selected_entity_label <- function() {
      req(rv$insights$entities_named, rv$analysis$entity)
      entity_idx <- which(unname(rv$insights$entities_named) == rv$analysis$entity)[1]
      names(rv$insights$entities_named)[entity_idx] %||% rv$analysis$entity
    }

    selected_dim_labels <- function(baseunit = TRUE) {
      req(rv$analysis$dim_meta, rv$analysis$dims)
      rv$analysis$dim_meta |>
        dplyr::filter(
          .data$report_type == "dimension",
          .data$name %in% rv$analysis$dims,
          .data$dimension_baseunit == baseunit
        ) |>
        dplyr::pull("label")
    }

    active_filter_summary <- function() {
      req(rv$analysis$result, rv$analysis$dim_meta, rv$analysis$dims)

      purrr::compact(purrr::map(rv$analysis$dims, function(d) {
        selected_vals <- input[[paste0("filter_dim__", d)]] %||% character(0)
        all_vals <- sort(unique(rv$analysis$result$MEANS[[d]]))
        if (length(selected_vals) == 0 || setequal(selected_vals, all_vals)) {
          return(NULL)
        }
        dim_label <- rv$analysis$dim_meta |>
          dplyr::filter(.data$name == d) |>
          dplyr::pull("label") |>
          dplyr::first()
        paste0(dim_label, ": ", paste(selected_vals, collapse = ", "))
      }))
    }

    report_summary_params <- function() {
      req(rv$analysis$result, rv$inputs$data)

      base_dims <- selected_dim_labels(TRUE)
      sub_dims  <- selected_dim_labels(FALSE)
      active_filters <- active_filter_summary()
      ch <- rv$inputs$data$chain_summary
      is_clustered <- nzchar(ch$clusteringEntity %||% "")

      list(
        survey_label              = ch$surveyLabel %||% "Survey",
        report_author             = paste(
          "Prepared with Arena Analytics",
          "https://openforis-shiny.shinyapps.io/arenalytics/",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
          sep = " | "
        ),
        survey_code               = ch$surveyName %||% "-",
        cycle_number              = as.character(ch$selectedCycle %||% "-"),
        sampling_strategy         = as.character(ch$samplingStrategy %||% "-"),
        stratification_attribute  = ch$stratumAttribute %||% "-",
        clustering                = if (is_clustered) "Yes" else "No",
        clustering_attribute      = if (is_clustered) paste(ch$clusteringEntityKeys %||% "-", collapse = ", ") else "-",
        nonresponse_correction    = "Unknown",
        analysis_entity           = selected_entity_label(),
        analysis_type             = if (identical(analysis_result_mode(), "area")) "Area" else "Other measures",
        base_unit_dimensions      = if (length(base_dims) > 0) paste(base_dims, collapse = ", ") else "None",
        sub_unit_dimensions       = if (length(sub_dims) > 0) paste(sub_dims, collapse = ", ") else "None",
        measure_label             = selected_measure_label(),
        confidence_level          = as.character(input$analysis_p_value %||% "0.95"),
        filters_applied           = if (length(active_filters) > 0) paste(active_filters, collapse = "; ") else "None",
        table_title               = if (identical(input$analysis_table_source, "TOTALS")) "Totals" else "Means (per ha)",
        has_means_plot            = !identical(analysis_result_mode(), "area")
      )
    }

    sync_analysis_result_ui <- function() {
      is_area <- identical(analysis_result_mode(), "area")

      shinyjs::toggle("analysis_table_source_wrap", condition = !is_area)
      shinyjs::toggle("analysis_means_card", condition = !is_area)

      updateSelectInput(
        session,
        "analysis_table_source",
        choices = if (is_area) c("Totals" = "TOTALS") else c("Means (per ha)" = "MEANS", "Totals" = "TOTALS"),
        selected = if (is_area) "TOTALS" else (input$analysis_table_source %||% "MEANS")
      )
    }

    get_plot_validation <- function() {
      req(rv$analysis$dims, input$plot_dim)

      role_dims <- unique(c(
        input$plot_dim %||% character(0),
        if (isTruthy(input$plot_fill) && input$plot_fill != "") input$plot_fill else character(0),
        if (isTruthy(input$plot_facet) && input$plot_facet != "") input$plot_facet else character(0)
      ))

      filter_vals <- tryCatch(get_filter_vals(), error = function(e) list())
      uncovered_dims <- setdiff(rv$analysis$dims, role_dims)
      invalid_dims <- uncovered_dims[
        purrr::map_lgl(uncovered_dims, \(d) length(filter_vals[[d]] %||% character(0)) != 1)
      ]

      if (length(invalid_dims) == 0) {
        return(list(
          ok = TRUE,
          message = "All selected dimensions are accounted for in the figure."
        ))
      }

      list(
        ok = FALSE,
        message = paste0(
          "To display the figure, assign or reduce these dimensions to one class: ",
          paste(invalid_dims, collapse = ", "),
          "."
        )
      )
    }
    ## ++ ##

    ## . + Update plot selectors when a new result arrives ------
    observeEvent(rv$analysis$result, {
      req(rv$analysis$result, rv$analysis$dim_meta, rv$analysis$measures_meta)

      dim_meta    <- dplyr::filter(rv$analysis$dim_meta, .data$name %in% rv$analysis$dims)
      dim_choices <- stats::setNames(dim_meta$name, dim_meta$label)

      meas_meta    <- rv$analysis$measures_meta
      meas_choices <- stats::setNames(meas_meta$name, meas_meta$label)

      n_dims <- length(dim_choices)
      optional_choices <- c("-- None --" = "", dim_choices)
      facet_choices <- if (n_dims >= 2) optional_choices else c("-- None --" = "")

      updateSelectInput(session, "analysis_sel_measure", choices = meas_choices, selected = meas_choices[1])
      updateSelectInput(session, "plot_dim", choices = dim_choices, selected = dim_choices[1])
      updateSelectInput(session, "plot_fill", choices = optional_choices, selected = "")
      updateSelectInput(session, "plot_facet", choices = facet_choices, selected = "")

      shinyjs::enable("plot_fill")
      shinyjs::toggleState("plot_facet", condition = n_dims >= 2)
    })

    observeEvent(rv$analysis$mode, {
      req(rv$analysis$mode)
      sync_analysis_result_ui()
    })

    ## . + Make output table ------
    output$analysis_table <- DT::renderDT({
      req(rv$analysis$result, input$analysis_table_source, input$analysis_sel_measure)

      table_df <- if (identical(input$analysis_sel_measure, "area")) {
        table_display_df(input$analysis_table_source) |>
          label_analysis_table_columns()
      } else {
        table_display_df(input$analysis_table_source) |>
          dplyr::select(
            -"base_unit_count", -"item_count", dplyr::everything(), "item_count", "base_unit_count"
          ) |>
          label_analysis_table_columns()
      }

      table_out <- DT::datatable(
        table_df,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )

      numeric_cols <- names(table_df)[purrr::map_lgl(table_df, is.numeric)]
      count_cols   <- numeric_cols[endsWith(numeric_cols, "count")]
      measure_cols <- setdiff(numeric_cols, count_cols)

      if (length(measure_cols) > 0) {
        table_out <- DT::formatRound(table_out, columns = measure_cols, digits = 3, mark = ",")
      }
      if (length(count_cols) > 0) {
        table_out <- DT::formatRound(table_out, columns = count_cols, digits = 0, mark = ",")
      }

      table_out
    })

    ## ++ ##
    observeEvent(input$analysis_table_copy, {
      req(rv$analysis$result, input$analysis_table_source, input$analysis_sel_measure)

      table_df <- table_display_df(input$analysis_table_source) |>
        label_analysis_table_columns()
      txt <- paste(
        c(
          paste(names(table_df), collapse = "\t"),
          purrr::map_chr(
            seq_len(nrow(table_df)),
            \(i) paste(as.character(table_df[i, , drop = TRUE]), collapse = "\t")
          )
        ),
        collapse = "\n"
      )

      shinyjs::runjs(
        sprintf(
          "navigator.clipboard.writeText(%s);",
          jsonlite::toJSON(txt, auto_unbox = TRUE)
        )
      )
    })
    ## ++ ##

    output$analysis_table_download <- downloadHandler(
      filename = function() {
        analysis_download_name("full-table", "csv")
      },
      content = function(file) {
        utils::write.csv(
          filtered_analysis_df(input$analysis_table_source %||% "MEANS"),
          file,
          row.names = FALSE
        )
      }
    )
    ## ++ ##

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
        style = "margin-top: 0.75rem;",
        tags$small(class = "text-muted", bsicons::bs_icon("funnel"), " Dimension filters"),
        layout_column_wrap(width = "180px", fill = FALSE, !!!filter_inputs)
      )
    })

    output$analysis_plot_guidance <- renderUI({
      req(rv$analysis$result, input$plot_dim)
      validation <- get_plot_validation()

      div(
        class = if (validation$ok) "text-success" else "text-warning",
        style = "font-size: 0.9em; font-style: italic;",
        if (validation$ok) bsicons::bs_icon("check-circle") else bsicons::bs_icon("exclamation-triangle"),
        paste0(" ", validation$message)
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

    ## ++ ##
    build_analysis_plot <- function(source = c("MEANS", "TOTALS")) {
      source <- match.arg(source)

      validation <- get_plot_validation()
      if (!isTRUE(validation$ok)) {
        stop(validation$message, call. = FALSE)
      }

      make_bar_plot(
        df                = filtered_analysis_df(source),
        x_dim             = input$plot_dim,
        measure           = input$analysis_sel_measure,
        fill_col          = input$plot_fill,
        facet_col         = input$plot_facet,
        show_errbar       = isTRUE(input$plot_errbar),
        flip_coords       = isTRUE(input$plot_flip),
        wrap_labels       = isTRUE(input$plot_wrap_labels),
        hide_legend       = isTRUE(input$plot_hide_legend),
        dim_meta          = rv$analysis$dim_meta,
        measures_meta     = rv$analysis$measures_meta,
        extra_filter_vals = get_filter_vals(),
        comma_y           = identical(source, "TOTALS")
      )
    }

    analysis_report_table <- function() {
      req(input$analysis_table_source)
      table_display_df(input$analysis_table_source)
    }

    safe_file_stub <- function(x) {
      x |>
        stringr::str_replace_all("[^A-Za-z0-9]+", "-") |>
        stringr::str_replace_all("(^-+|-+$)", "") |>
        tolower()
    }
    ## ++ ##

    ## . + MEANS bar plot ----------------------------------------------------
    output$analysis_plot_means <- renderPlot({
      req(rv$analysis$result, input$plot_dim, input$analysis_sel_measure)
      validation <- get_plot_validation()
      validate(need(validation$ok, validation$message))
      build_analysis_plot("MEANS")
    })

    output$analysis_plot_means_download <- downloadHandler(
      filename = function() {
        analysis_download_name("fig-mean", "png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = build_analysis_plot("MEANS"),
          width = 10,
          height = 6,
          dpi = 300
        )
      }
    )

    ## . + TOTALS bar plot ---------------------------------------------------
    output$analysis_plot_totals <- renderPlot({
      req(rv$analysis$result, input$plot_dim, input$analysis_sel_measure)
      validation <- get_plot_validation()
      validate(need(validation$ok, validation$message))
      build_analysis_plot("TOTALS")
    })

    output$analysis_plot_totals_download <- downloadHandler(
      filename = function() {
        analysis_download_name("fig-total", "png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = build_analysis_plot("TOTALS"),
          width = 10,
          height = 6,
          dpi = 300
        )
      }
    )

    ## . + DL report ------
    output$analysis_report_download <- downloadHandler(
      filename = function() {
        req(rv$inputs$data, input$analysis_report_format)
        ext <- if (identical(input$analysis_report_format, "docx")) "docx" else "html"
        analysis_download_name("report", ext)
      },
      contentType = "application/octet-stream",
      content = function(file) {
        req(rv$analysis$result, rv$inputs$data, input$analysis_report_format)

        shinyjs::disable("analysis_report_download")
        on.exit(shinyjs::enable("analysis_report_download"), add = TRUE)

        report_dir <- tempfile(pattern = "arenalytics-report-")
        dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

        means_plot_file <- file.path(report_dir, "means-plot.png")
        totals_plot_file <- file.path(report_dir, "totals-plot.png")
        table_file <- file.path(report_dir, "analysis-table.csv")
        qmd_template <- system.file("quarto", "analysis-report.qmd", package = "arenalytics.dev")
        qmd_file <- file.path(report_dir, "analysis-report.qmd")

        if (!nzchar(qmd_template)) {
          stop("Report template not found.", call. = FALSE)
        }

        ok_copy <- file.copy(qmd_template, qmd_file, overwrite = TRUE)
        if (!isTRUE(ok_copy)) {
          stop("Could not prepare the report template.", call. = FALSE)
        }

        if (!identical(analysis_result_mode(), "area")) {
          ggplot2::ggsave(means_plot_file, plot = build_analysis_plot("MEANS"), width = 10, height = 6, dpi = 300)
        }
        ggplot2::ggsave(totals_plot_file, plot = build_analysis_plot("TOTALS"), width = 10, height = 6, dpi = 300)
        utils::write.csv(
          analysis_report_table() |> label_analysis_table_columns(),
          table_file,
          row.names = FALSE
        )

        rendered_file <- file.path(
          report_dir,
          paste0("analysis-report.", if (identical(input$analysis_report_format, "docx")) "docx" else "html")
        )
        rendered_name <- basename(rendered_file)
        summary_params <- report_summary_params()

        quarto::quarto_render(
          input = qmd_file,
          output_format = input$analysis_report_format,
          execute_params = c(summary_params, list(
            table_csv = table_file,
            means_plot = if (summary_params$has_means_plot) means_plot_file else "",
            totals_plot = totals_plot_file
          )),
          output_file = rendered_name,
          quiet = FALSE
        )

        if (!file.exists(rendered_file)) {
          stop("Report generation failed.", call. = FALSE)
        }

        ok_out <- file.copy(rendered_file, file, overwrite = TRUE)
        if (!isTRUE(ok_out)) {
          stop("Could not copy the rendered report to the download target.", call. = FALSE)
        }
      }
    )
    ## ++ ##

    ## $$$

  })

}
