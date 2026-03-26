
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
