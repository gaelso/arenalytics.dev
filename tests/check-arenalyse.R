
# devtools::load_all()
#
# source("tests/dev-fct/dev_arenalyse.R", local = T)
# source("tests/dev-fct/dev_arenalyse2.R", local = T)
#
#
# ## Dummy data, not big
# .zip <- fct_readzip2(.path = "inst/extdata/OLAP_Shiny_demo.zip") ; names(.zip)
#
# ## Big files
# # .zip <- fct_readzip2(.path = "/Users/gaelsola/Documents/FAO-2026/support-arenalytics/OLAP_Shiny_(png_nfi_2024_upperplant).zip")
#
# ## Get entity and reporting dim from chain_summary
# .entity <- .zip$chain_summary$analysis$entity
# # summary(.zip[[paste0("OLAP_", .entity)]])
# .dim <- .zip$chain_summary$analysis$dimensions
# .dim
#
# res <- fct_arenalyse(.zip = .zip, .entity = .entity, .dim = .dim)
# res2 <- fct_arenalyse2(.zip = .zip, .entity = .entity, .dim = .dim)
#
#
# df1 <- res$MEANS
# df2 <- res2$MEANS
#
# # df1 <- res$TOTALS
# # df2 <- res2$TOTALS
#
#
# df1 <- res$MEANS |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
# df2 <- res2$MEANS |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#
# identical(res$MEANS, res2$MEANS)
# identical(df1, df2)
# all.equal(df1, df2)


## Second series of tests with new function params
# rv <- list()
# rv$inputs <- list()
# # rv$inputs$path_zip <- system.file("extdata/OLAP_Shiny_demo.zip", package = "arenalytics")
# rv$inputs$path_zip <- "data-raw/MAU_Shiny_(png_nfi_2024_upperplant) 1.zip"
# rv$inputs$check_zip <- fct_checkzip(.path = rv$inputs$path_zip, .entity_prefix = .ep)
# rv$inputs      <- fct_readzip2(.path = rv$inputs$path_zip, .entity_prefix = .ep)
# input <- list()
# input$analysis_sel_entity <- "tree"
# input$analysis_sel_dims <- c("cluster_forest_type", "dbh_up100_10cm")
# result1 <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = input$analysis_sel_dims,
#   .cm = "fast", .lonely = "adjust"
# )$MEANS
# result2 <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = input$analysis_sel_dims,
#   .cm = "fast", .lonely = "remove"
# )$MEANS
# rv$insights <- list()
# dims_sel <- c("cluster_forest_type", "stratum_calc")
# result3 <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = dims_sel,
#   .cm = "fast", .lonely = "adjust"
# )$MEANS
# result4 <- fct_arenalyse(
#   .zip = rv$inputs$data, .entity = input$analysis_sel_entity, .dim = dims_sel,
#   .cm = "fast", .lonely = "remove"
# )$MEANS
#
# identical(result3, result4)
# identical(result1, result2)
# all.equal(result3, result4)
# all.equal(result1, result2)

