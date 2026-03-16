#
# library(tidyverse)
# library(arenalytics)
#
# zipfile <- system.file("extdata/OLAP_shiny_demo.zip", package = "arenalytics")
# if (nzchar(zipfile) && file.exists(zipfile)) {
#   zipdata <- fct_readzip2(.path = zipfile)
#   summary(zipdata$OLAP_tree$tree_biomass_ag)
# }
#
# names(zipdata)
#
# chain <- zipdata$chain_summary
# chain_var <- as_tibble(chain$resultVariables)
#
# table(chain_var$active)
#
# n_var <- chain_var |>
#   filter(active) |>
#   group_by(entity, areaBased) |>
#   summarise(n_var = n(), .groups = "drop") |>
#   mutate(areaBased = if_else(areaBased, "areaBased", "notAreaBased")) |>
#   pivot_wider(names_from = areaBased, values_from = n_var) |>
#   mutate(
#     areaBased = if_else(is.na(areaBased), 0, areaBased),
#     notAreaBased = if_else(is.na(notAreaBased), 0, notAreaBased)
#   )
#
# schema <- as_tibble(zipdata$SchemaSummary)
#
# dims <- as_tibble(zipdata$ReportDimensions)
#
# tree <- as_tibble(zipdata$OLAP_tree)
# tree
#
# bamboo <- as_tibble(zipdata$OLAP_bamboo)
# names(tree)
# summary(tree)
#
# tree_bu <- tree |> filter(OLAP_baseunit_total)
#
#
# test <- names(zipdata) |>
#   stringr::str_subset("OLAP_")
# test_labs <- test |>
#   stringr::str_remove("OLAP_")
# test2 <- test
# names(test2) <- test_labs
# test2
# tt <- setNames(test, test_labs)
# tt
#
#
#
# dims_select <- dims |> filter(entity == "tree") |> pull(dimension)
# tt <- schema |>
#   filter(parentEntity == "tree") |>
#   select(name, paste0("label_", "en"))
# tt2 <- setNames(tt[[1]], tt[[2]])
#
#
# tt <- schema |>
#   filter(parentEntity == "bamboo") |>
#   select(name, paste0("label_", "en"))
# tt2 <- setNames(tt[[1]], tt[[2]])
# names(bamboo)
