# # devtools::load_all()
#
# # .ep <- "MAU_"
# rv <- list()
# rv$inputs <- list()
# # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics")
# rv$inputs$path_zip <- "data-raw/MAU_Shiny_(png_nfi_2024_upperplant) 1.zip"
# rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip)
# rv$inputs           <- fct_readzip2(.path = rv$inputs$path_zip)
# rv$inputs$data$chain_summary$analysis$pValue
# input <- list()
# input$analysis_sel_entity <- "tree"
# input$analysis_sel_dims <- c("cluster_forest_type", "dbh_up100_10cm")
# input$analysis_mode <- "area"
# #input$analysis_mode <- "other"
# rv$analysis <- list()
# rv$analysis$dims_sel <- input$analysis_sel_dims
# rv$analysis$dim_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]]
#
# result <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = input$analysis_sel_dims,
#   .cm = "fast", .lonely = "remove"
# )
# rv$insights <- list()
# dims_sel <- c("cluster_forest_type", "stratum_calc")
# result <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = rv$analysis$dims_sel,
#   .cm = "fast", .lonely = "remove"
# )
# names(result$MEANS)
# summary(result$TOTALS$area)
# if (identical(input$analysis_mode, "area")) {
#         rv$analysis$measures_meta <- tibble::tibble(
#           name = "area",
#           label = "Area"
#         )
#       } else {
#         rv$analysis$measures_meta <- rv$inputs$var_meta[[input$analysis_sel_entity]] |>
#           dplyr::filter(.data$report_type == "measure")
#       }
#
# lang      <- rv$inputs$data$chain_summary$selectedLanguage
# dim_meta  <- rv$analysis$dim_meta
# cats      <- rv$inputs$data$categories
#
# result2 <- result
#
# result2$MEANS  <- replace_dim_labels(result2$MEANS,  dim_meta, cats, lang)
# result2$TOTALS <- replace_dim_labels(result2$TOTALS, dim_meta, cats, lang)
#
# if (!identical(input$analysis_mode, "area")) {
#   result$MEANS  <- result$MEANS  |> dplyr::select(-dplyr::any_of(c("area", "JOIN_COL")))
#   result$TOTALS <- result$TOTALS |> dplyr::select(-dplyr::any_of(c("area", "JOIN_COL")))
# }
#
# # rv$analysis$result2 <- NULL
