
fct_arenalyse <- function(.zip, .entity, .dim){
  ## Data must include:
  ## - chain_summary
  ## - schema_summary
  ## - report_dimensions
  ## - OLAP_*

  ## !!! FOR TESTING ONLY
  # devtools::load_all()
  # .zip <- fct_readzip2(.path = "inst/extdata/OLAP_Shiny_demo.zip") ; names(.zip)
  # .entity <- "tree"
  # # summary(.zip[[paste0("OLAP_", .entity)]])
  # .dim <- "plot_forest_type"
  ## !!!

  ## Make tibbles for dev
  .zip$chain_summary$resultVariables <- dplyr::as_tibble(.zip$chain_summary$resultVariables)
  .zip$schema_summary    <- dplyr::as_tibble(.zip$schema_summary)
  .zip$report_dimensions <- dplyr::as_tibble(.zip$report_dimensions)

  ## 1. Pkg survey options: See R/zzz.R, set with .onLoad() ####

  ## 2. Select ENTITY, done in shinyapp, passed to .entity ####

  ## 3. Select DIMENSIONS, done in shinyapp, passed to .dim ####
  ## !!! MAY NEED TO BE CHECKED against report_dimensions to ensure data

  ## 4. Get entity labels from chain_summary and schema_summary ####
  label_language <- paste0("label_", .zip$chain_summary$selectedLanguage)

  label_cols <- .zip$schema_summary |>
    dplyr::filter(.data$type == 'entity') |>
    dplyr::select(entity = "name", label = all_of(label_language))

  df_report_entities <- .zip$chain_summary$resultVariables |>
    dplyr::filter(.data$areaBased & .data$active) |>
    dplyr::select("entityPath", "entity") |>
    dplyr::distinct() |>
    dplyr::mutate(wide_table = paste0('OLAP_', .data$entity)) |>
    dplyr::left_join(label_cols, by = 'entity')

  wt_filename <- df_report_entities |>
    dplyr::filter(.data$entity == .entity) |>
    dplyr::pull(wide_table)

  ##
  ## !!! 1-4 probably to be done outside this function when ZIP data are read into the environment
  ##

  ## 5. Make wide table from OLAP ####
  ## 5.1. Get OLAP table ====
  wt <- .zip[[wt_filename]] |> dplyr::as_tibble()

  ## 5.2. Get reporting variables =====
  df_result_vars <- .zip$chain_summary$resultVariables |>
    #dplyr::select(name, type, categoryName, parentEntity = entity, label = all_of(label_language)) |>
    dplyr::select(name, type, categoryName, parentEntity = entity, label) |>
    dplyr::mutate(
      report_type = ifelse(type == 'Q', 'measure', 'dimension'),
      type        = ifelse(type == 'Q', 'numeric', 'code'),
      source      = 'chain'
      )

  ## 5.3 Get essential information about Wide Table data ====
  wt_names          <- dplyr::tibble(name = names(wt))

  ## Done in 1. into label_language
  ## label_column_name <- paste0("label_", arena.chainSummary$selectedLanguage)

  # get information for taxonomies & categories: name, type, categoryName, parentEntity
  # Note: taxonomyName is renamed to --> categoryName
  schemasum_nameinfo <- .zip$schema_summary |>
    dplyr::mutate(categoryName = ifelse(.data$taxonomyName != "", .data$taxonomyName, .data$categoryName)) |>
    dplyr::select("name", "type", "categoryName", "parentEntity", label = all_of(label_language))   |>
    dplyr::mutate(report_type2 = 'dimension', source2 = 'input')

  resvar_nameinfo <- df_result_vars |>
    dplyr::select(type2 = "type", categoryName2 = "categoryName", parentEntity2 = "parentEntity", label2="label", dplyr::everything())

  wt_names <- wt_names |>
    dplyr::left_join(schemasum_nameinfo, by = "name") |>
    dplyr::left_join(resvar_nameinfo, by = "name")


  wideTable_names <- wideTable_names                                                                 |>
    left_join( ,
              by = 'name')                                                                           |>
    left_join(df_ReportVariables |>
                select(type2 = type, categoryName2 = categoryName, parentEntity2 = parentEntity, label2=label, everything() ),
              by ='name')       |>
    mutate( type   = ifelse( is.na(type), type2, type),
            categoryName = ifelse( categoryName  == "" | is.na(categoryName), categoryName2, categoryName),
            report_type    = ifelse( report_type == "" | is.na(report_type), report_type2, report_type),
            source       = ifelse( source        == "" | is.na(source), source2, source),
            label        = ifelse( label         == "" | is.na(label), label2, label),
            parentEntity = ifelse( parentEntity  == "" | is.na(parentEntity), parentEntity2, parentEntity)) |>
    select(-type2, -categoryName2, -report_type2, -source2, -parentEntity2, -label2)                         |>
    mutate(dimension_baseunit = ifelse( parentEntity == arena.entity, FALSE, TRUE))

  wideTable_names$report_type[        wideTable_names$name == 'weight'] <- NA
  wideTable_names$dimension_baseunit[ wideTable_names$name == 'weight'] <- NA

  # tag the stratum attribute
  wideTable_names$stratum <- FALSE
  if (!is.null(arena.chainSummary$stratumAttribute)) {
    if ( arena.chainSummary$stratumAttribute != '') wideTable_names$stratum[wideTable_names$name == arena.chainSummary$stratumAttribute] <- TRUE
  }

}








#
# #************************************************************
# # Read and return df_wideTable, wideTable_names
# get_WideTable <- function( arena.entity, query1) {
#
#   arena.chainSummary     <- query1[["chainSummary"]]
#   arena.SchemaSummary    <- query1[["SchemaSummary"]]
#   arena.ReportEntities   <- query1[["ReportEntities"]]
#
#   #  print(arena.chainSummary)
#   # chainSummary.json: label column can be named e.g. "label_en","label_fr", or simply "label" if there is just one language in the survey.
#   label_column_name <- paste0("label_", arena.chainSummary$selectedLanguage)
#
#   if (!label_column_name %in% names(arena.chainSummary$resultVariables)) label_column_name = 'label'
#
#   # 1- Read Wide Table data ---------------------------------
#   widetable_file <- arena.ReportEntities$wideTable[ arena.ReportEntities$entity == arena.entity ]
#   #  print(arena.ReportEntities)
#   df_wideTable   <- read.csv( widetable_file, stringsAsFactors = F  )
#
#   # 2- Get all variables in the Arena chain -------------------------------------------------------
#   df_ReportVariables           <- arena.chainSummary$resultVariables |>
#     select(name, type, categoryName, parentEntity = entity, label= all_of(label_column_name)) |>
#     mutate(report_type = ifelse(type=='Q', 'measure', 'dimension'),
#            type        = ifelse(type=='Q', 'numeric', 'code'),
#            source      = 'chain' )
#   #           label      = trimws( str_sub(label, end=-4)))
#
#   #  print( df_ReportVariables)
#
#   # 3- DF with info about Wide Table data (wideTable_names) ---------------------------------
#   # essential information about Wide Table data
#   wideTable_names         <- as.data.frame(names( df_wideTable))
#   names( wideTable_names) <- "name"
#   label_column_name       <- paste0("label_", arena.chainSummary$selectedLanguage)
#
#   # get information for taxonomies & categories: name, type, categoryName, parentEntity
#   # Note: taxonomyName is renamed to --> categoryName
#   wideTable_names <- wideTable_names                                                                 |>
#     left_join(arena.SchemaSummary |>
#                 mutate( categoryName= ifelse(taxonomyName != "", taxonomyName, categoryName))        |>
#                 select( name, type, categoryName, parentEntity, label= all_of(label_column_name) )   |>
#                 mutate( report_type2 = 'dimension', source2 = 'input') ,
#               by = 'name')                                                                           |>
#     left_join(df_ReportVariables |>
#                 select(type2 = type, categoryName2 = categoryName, parentEntity2 = parentEntity, label2=label, everything() ),
#               by ='name')       |>
#     mutate( type   = ifelse( is.na(type), type2, type),
#             categoryName = ifelse( categoryName  == "" | is.na(categoryName), categoryName2, categoryName),
#             report_type    = ifelse( report_type == "" | is.na(report_type), report_type2, report_type),
#             source       = ifelse( source        == "" | is.na(source), source2, source),
#             label        = ifelse( label         == "" | is.na(label), label2, label),
#             parentEntity = ifelse( parentEntity  == "" | is.na(parentEntity), parentEntity2, parentEntity)) |>
#     select(-type2, -categoryName2, -report_type2, -source2, -parentEntity2, -label2)                         |>
#     mutate(dimension_baseunit = ifelse( parentEntity == arena.entity, FALSE, TRUE))
#
#   wideTable_names$report_type[        wideTable_names$name == 'weight'] <- NA
#   wideTable_names$dimension_baseunit[ wideTable_names$name == 'weight'] <- NA
#
#   # tag the stratum attribute
#   wideTable_names$stratum <- FALSE
#   if (!is.null(arena.chainSummary$stratumAttribute)) {
#     if ( arena.chainSummary$stratumAttribute != '') wideTable_names$stratum[wideTable_names$name == arena.chainSummary$stratumAttribute] <- TRUE
#   }
#
#   # 4- Add category type (Flat/Hierarchical) ---------------------------------
#   # needed to get separate hierarchical code attributes)
#   # F: flat table, H: hierarchical table, blank: not code attribute
#   wideTable_names$categoryType <- ifelse( wideTable_names$type == 'code', "F", "")
#   # check indexes of categoryNames that are hierarchical but on levels 2,3,.. These contains square brackets in SchemaSummary, column 'categoryName'.
#   i_levels                <- str_which(wideTable_names$categoryName, pattern =  '(?<=\\[).*(?=\\])')
#   if (length(i_levels) > 0) {
#     for (ix in i_levels) {
#       wideTable_names$categoryType[ix] = "H"
#       wideTable_names$categoryName[ix]  = str_sub( wideTable_names$categoryName[ix], end = -11)
#     }
#   }
#
#   wideTable_names[ is.na(wideTable_names) ] <- ''
#   #  print( wideTable_names)
#
#   return( list(df_wideTable, wideTable_names ))
# }
# #****************************************************************
#
#
#
#
#
#
# #************************************************************
# # The main function
# #************************************************************
# do_analytics <- function( arena.entity, arena.dimensions, arena.clevel, query1, query2)  {
#
#   # read arguments
#   arena.chainSummary    <- query1[[1]]
#   arena.SchemaSummary   <- query1[[2]]
#   #  df_ReportEntities     <- query1[[3]] # not needed in this function
#   df_wideTable          <- query2[[1]]
#   arena.wideTable_names <- query2[[2]]
#   arena.clevel          <- as.numeric( arena.clevel)
#
#   # initialize 'arena.analyze'
#   arena.analyze  <- list( entity = arena.entity,              # selected entity name to report, e.g. 'tree'
#                           dimensions_names_all =              # list of all possible dimension names of the selected entity
#                             (arena.wideTable_names              |>
#                                filter( report_type == "dimension") |>
#                                select( name) |> pull()),
#                           dimensions = arena.dimensions,      # list of all selected dimensions to report, of the selected entity
#                           dimensions_at_baseunit = '',        # from previous group, dimensions which belong to base unit level or above, e.g. forest_type, province, etc. but not tree_species, etc.
#                           dimensions_all_at_baseunit = FALSE, # are all 'dimensions' at the base unit level?
#                           measures = '',                      # list of Measures, e.g. tree_basal_area, tree_volume_stem, etc.
#                           stratification = FALSE,             # is this stratified sampling?
#                           strat_attribute = '',               # stratification attribute name
#                           stratum_in_dimensions = FALSE,      # is the stratification attribute in the list of report 'dimensions'?
#                           filter = ''                         # NOT USED YET
#   )
#
#
#   # 1. Base unit UUID and cluster UUID attributes in Wide Table -------------------------------------------------------
#   base_UUID_     <- paste0( arena.chainSummary$baseUnit, "_uuid")
#   cluster_UUID_  <- ifelse( arena.chainSummary$clusteringEntity != "", paste0( arena.chainSummary$clusteringEntity, "_uuid"), "")
#
#   # 2. Stratification: method & attribute-------------------------------------------------------
#   if (arena.chainSummary$samplingStrategy == 3 | arena.chainSummary$samplingStrategy == 4 )  arena.analyze$stratification <- TRUE
#   if ( arena.analyze$stratification) arena.analyze$strat_attribute <- arena.chainSummary$stratumAttribute
#
#   # 3. List of Dimensions at the base unit level --------------------------
#   arena.analyze$dimensions_at_baseunit <- filter(arena.wideTable_names, name %in% arena.analyze$dimensions & dimension_baseunit==TRUE) |>
#     select(name) |> pull()
#
#
#   # If stratum and no other base unit level attributes in Dimensions, then take stratification out for "survey"
#   # this works because weight is the expansion area, and a missing entity level attribute, such as a tree_species,
#   # will get mean and variation equal to zero in those strata where it does not exists
#   if (length( arena.analyze$dimensions_at_baseunit) == 0) {
#     arena.analyze$stratification  <- FALSE
#     arena.analyze$strat_attribute <- ""
#   }
#
#   # 4. stratum in Dimensions? -------------
#   # i.e. is that reported?
#   # add stratification attribute into list of Dimensions
#   if ( arena.analyze$stratification ) {
#     if (arena.analyze$strat_attribute %in% arena.analyze$dimensions) arena.analyze$stratum_in_dimensions <- TRUE
#
#     arena.analyze$dimensions             <- unique( c( arena.analyze$dimensions, arena.analyze$strat_attribute))
#     arena.analyze$dimensions_at_baseunit <- unique( c( arena.analyze$dimensions_at_baseunit, arena.analyze$strat_attribute))
#   }
#
#   # 5. all Dimensions at the base unit level or above? ------
#   if (length(arena.analyze$dimensions) == length(arena.analyze$dimensions_at_baseunit)) arena.analyze$dimensions_all_at_baseunit = TRUE
#
#   arena.analyze$measures <- arena.wideTable_names |> filter( report_type == "measure") |>
#     select(name) |> pull()
#
#
#   # 6. Read analysis data to new DF  ------
#   df_analysis_data <- df_wideTable                                                 |>
#     filter( OLAP_baseunit_total == arena.analyze$dimensions_all_at_baseunit)       |> # TRUE/FALSE in wide table (last column)
#     mutate( across(all_of(arena.analyze$dimensions_names_all), ~as.character(.)))  |>
#     select( -OLAP_baseunit_total)
#
#
#   # 7. Complete data (==> df_analysis_total) --------------------
#   if (arena.analyze$dimensions_all_at_baseunit) {
#     # all Dimensions are at the base unit level or above.
#     df_analysis_total <- df_analysis_data                                                       |>
#       dplyr::filter( weight > 0)                                                                |>
#       dplyr::mutate( across( any_of( arena.analyze$measures),   ~tidyr::replace_na(., 0)))      |>
#       dplyr::mutate( across( any_of( arena.analyze$dimensions), ~tidyr::replace_na(., "NoData")))
#
#   } else {  # all Dimensions are not at the base unit (there can be e.g. tree_species in Dimensions)
#     #  with tidyr::complete, generate all missing base units
#     #    https://tidyr.tidyverse.org/reference/complete.html
#     #    https://stackoverflow.com/questions/40577484/using-tidyr-complete-with-column-names-specified-in-variables
#
#     # before running 'tidyr::complete', drop out from dataframe all base unit level names, base_UUID_ and cluster_UUID_, excl. stratum attribute
#     names_to_drop <- arena.analyze$dimensions[ arena.analyze$dimensions %in% arena.analyze$dimensions_at_baseunit ]
#     if (cluster_UUID_ != "") names_to_drop <- c( names_to_drop, cluster_UUID_)
#
#     names_to_drop
#     dim_names     <- unique( c(base_UUID_, arena.analyze$dimensions))
#     dim_names     <- dim_names[ !(dim_names %in% names_to_drop)]
#     dim_names
#
#     if ( arena.analyze$stratification ) {     # run 'tidyr::complete' across strata by 'dim_names'
#       df_analysis_data[arena.analyze$strat_attribute][is.na(df_analysis_data[arena.analyze$strat_attribute])] <- "NoData"
#       df_analysis_data <- df_analysis_data                        |>
#         group_by( across( all_of(arena.analyze$strat_attribute))) |>
#         tidyr::complete(!!!syms(dim_names))                       |>
#         data.frame()
#     } else {                                  # run 'tidyr::complete' by 'dim_names'
#       df_analysis_data <- df_analysis_data                        |>
#         tidyr::complete(!!!syms(dim_names))                       |>
#         data.frame()
#     }
#
#     df_analysis_data$entity_count_[is.na(df_analysis_data$entity_count_)] <- 0
#
#     # join back previously dropped dimensions, base_UUID_, weight, exp_factor_
#     df_analysis_data <- df_analysis_data                          |>
#       select(-weight, -exp_factor_, -all_of(names_to_drop))       |>
#       left_join(df_analysis_data                                  |>
#                   dplyr::filter( !is.na(exp_factor_))             |>
#                   distinct( !! sym(base_UUID_), .keep_all = T )   |>
#                   dplyr::select( all_of(base_UUID_), all_of( names_to_drop), weight, exp_factor_),
#                 by = base_UUID_)                                  |>
#       dplyr::mutate( across( any_of( arena.analyze$measures),   ~tidyr::replace_na(., 0)))
#
#     # compute aggregated 'df_analysis_total'
#     df_analysis_total <- df_analysis_data                                               |>
#       dplyr::filter( weight > 0 )                                                       |>
#       dplyr::group_by(  across( unique( c( base_UUID_, arena.analyze$dimensions))))     |>
#       # fix MAX function... wrong!
#       dplyr::summarize( entity_count_ = max(entity_count_), across( .cols= all_of( arena.analyze$measures),
#                                                                     list( Total = ~sum( .x, na.rm = TRUE )),
#                                                                     .names = "{.col}") )                                    |>
#       data.frame() |>
#       left_join(df_analysis_data |> select( all_of(base_UUID_), weight, exp_factor_) |> distinct(),
#                 by = base_UUID_)
#
#     if (cluster_UUID_ != "") df_analysis_total <- df_analysis_total                     |>
#       left_join(df_analysis_data |> select( all_of(base_UUID_), all_of(cluster_UUID_)) |> distinct(),
#                 by = base_UUID_)
#   }
#
#   # 8. Omit data if (exp_factor_ == 0) ------
#   # there might be strata which area is set to 0 ha !
#   df_analysis_total <- df_analysis_total |> filter( exp_factor_ != 0)
#
#   # 9. Initialize cluster & weight variables for "survey" -----
#   ids_2_survey     <- NULL
#   # Set weight attribute for "survey"
#   arena.weights    <- "exp_factor_"
#
#   # 10. Compute DF for means (per hectare values) ------
#   df_analysis_mean <- df_analysis_total |>
#     dplyr::mutate( across( .cols= all_of( arena.analyze$measures),
#                            list( Mean = ~ .x / exp_factor_),
#                            .names = "{.col}"))               |>
#     data.frame()
#
#
#   # 11. Process input data for "survey" ----
#   # Cluster probability for "survey". If used, weight must be NULL.
#   # THIS METHOD IS NOT APPLIED NOW!!
#   probs_2_survey   <- NULL
#
#   # Stratification variable for "survey"
#   stratum_2_survey <- NULL
#   if (arena.analyze$stratification) {
#     stratum_2_survey <- arena.analyze$strat_attribute
#     # missing stratum code set to "", in order to report these cases too
#     df_analysis_mean[ stratum_2_survey][ is.na( df_analysis_mean[ stratum_2_survey]) ] <- ""
#   }
#
#   # 12. add serial number required by SQLite DB -----
#   df_analysis_mean$serialno <- 1:nrow(df_analysis_mean)
#   df_analysis_mean <- df_analysis_mean |>
#     select(serialno, everything())
#
#   # 13. Set up SQLite database and table ----
#   # https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-database.html
#   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#   df_analysis_mean_db <- copy_to(db, df_analysis_mean, "df_analysis_mean", temporary = FALSE)
#
#   ## compare object sizes: SQLite is much smaller
#   # object.size(df_analysis_mean)
#   # object.size(df_analysis_mean_db)
#
#   # an example how to use:
#   # df_analysis_mean_db |> select(tree_count) |> pull()
#
#   rm(df_analysis_mean);  rm(df_analysis_data)
#
#   # # 14. survey S3 object: SRS case (for efficiency of stratification) -----
#   # # if stratification, compute SRS case. Needed to compute efficiency of stratification.
#   # if ( arena.analyze$stratification ) {
#   #     design_srvyr_SRS <- df_analysis_mean_db  |>
#   #       srvyr::as_survey_design(
#   #         ids       = !!ids_2_survey,
#   #         strata    = NULL,
#   #         fpc       = NULL,
#   #         weights   = !!arena.weights,
#   #         variables = c( arena.analyze$dimensions, arena.analyze$measures, exp_factor_ ))
#   # }
#
#   # 15. survey S3 object  -----
#   design_srvyr_mean <-  df_analysis_mean_db       |>
#     srvyr::as_survey_design(
#       ids       = !!ids_2_survey,
#       strata    = !!stratum_2_survey,
#       probs     = !!probs_2_survey,  # Cluster sampling probabilities, set to NULL now
#       fpc       = NULL,              # Finite population correction
#       weights   = !!arena.weights,
#       nest      = TRUE,              # If TRUE, relabel cluster ids to enforce nesting within strata
#       variables = c( arena.analyze$dimensions, arena.analyze$measures, exp_factor_ ))
#
#
#
#   # 16. Remove stratum from Dimensions? ----
#   #     Remove it if not reported
#   if (arena.analyze$stratification & !arena.analyze$stratum_in_dimensions) {
#     arena.analyze$dimensions             <- arena.analyze$dimensions[!(arena.analyze$dimensions %in% arena.analyze$strat_attribute)]
#     arena.analyze$dimensions_at_baseunit <- arena.analyze$dimensions_at_baseunit[!(arena.analyze$dimensions_at_baseunit %in% arena.analyze$strat_attribute)]
#   }
#
#   # 17. Compute aggregated Expansion areas ------
#   # these are later in step #
#   if ( length( arena.analyze$dimensions_at_baseunit) == 0) {     # no Dimensions at the base unit, e.g. only 'tree_species' is reported
#     TotalArea_ <- df_analysis_total                              |>
#       dplyr::select( all_of( base_UUID_), area_ = exp_factor_)   |>
#       unique()                                                   |> # select unique base units (i.e. samples)
#       dplyr::select( area_)                                      |> # get total sum of areas
#       sum()
#   } else {  # by combinations of dimensions at the base unit level
#     df_AREAS <- df_analysis_total |>
#       unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>  # create a temporary column for later join
#       dplyr::select( all_of(base_UUID_), JOIN_COL, exp_factor_)  |>
#       unique()                                                   |>  # compute sum of areas by base unit level dimensions
#       summarize( area_ = sum(exp_factor_), .by = JOIN_COL)
#   }
#
#   start.time <- Sys.time()
#
#   # A test with PNG data, 1st version: 7.01 mins
#   # fastsurvey: Time difference of 4.7 mins
#   # with sqlLite database and fastsurvey: Time difference of 3.68 mins
#
#
#   # 18. survey: run survey_mean() --------------
#   # MEANS (per hectares) for selected categories
#   OUT_mean  <- design_srvyr_mean                                                |>
#     collect()                                                                   |>
#     dplyr::group_by( across( arena.analyze$dimensions ))                        |>
#     dplyr::summarize( across( any_of(arena.analyze$measures),
#                               list( ~survey_mean( ., na.rm = FALSE, vartype = c("se","ci"), proportion = FALSE, level = arena.clevel, df = Inf )))) |>
#     rename_with(.fn = ~ str_replace(.x, "_1", "_1_"), .cols = ends_with("_1"))
#
#   if ( length(arena.analyze$dimensions_at_baseunit) == 0) {
#     OUT_total <- OUT_mean                     |>
#       mutate( area_ = TotalArea_)
#   } else {
#     OUT_mean <- OUT_mean                    |>
#       # create a temporary column for joining the expansion area table
#       unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep = "**", remove = FALSE, na.rm = FALSE)
#
#     # join expansion area table
#     OUT_total <- OUT_mean                   |>
#       left_join( df_AREAS, by = 'JOIN_COL')
#   }
#
#   end.time <- Sys.time()
#   time.taken <- round(end.time - start.time,2)
#   print( time.taken )
#
#
#
#   # 19. Compute totals -----
#   OUT_total <- OUT_total                                                                    |>
#     mutate( across( contains("_1_"), ~.x * area_  , .names = "{col}"), .keep = 'all')       |>
#     rename_with(.fn = ~ str_replace(.x, "_1_", "_"),
#                 .cols = contains("_1_"))                                                    |>
#     rename_with(.fn = ~ str_sub(.x, end= -2),
#                 .cols = ends_with("_"))
#
#   # 20. Clean output tables----------------
#   # clear header texts
#   OUT_mean  <- OUT_mean                                                                     |>
#     rename_with(.fn = ~ str_replace(.x, "_1_", "_"),
#                 .cols = contains("_1_"))                                                    |>
#     rename_with(.fn = ~ str_sub(.x, end= -2),
#                 .cols = ends_with("_"))
#
#   OUT_total[ is.na(OUT_total)] <- ""
#   OUT_mean[  is.na(OUT_mean )] <- ""
#
#   # convert negative lower confidence intervals to zeros
#   OUT_mean   <- OUT_mean   |>
#     mutate( across( ends_with("_low"),  ~ if_else(.x < 0, 0, .x) ))
#   OUT_total  <- OUT_total  |>
#     mutate( across( ends_with("_low"),  ~ if_else(.x < 0, 0, .x) ))
#
#   # remove rows with only zeros in Measures. ???
#   # colsZero = ncol(OUT_mean) - rowSums(OUT_mean==0) - length(arena.analyze$dimensions) - 1
#   # colsZero =  ( colsZero == 0 )
#   # OUT_mean  <- OUT_mean  |> cbind(colsZero) |> filter(!colsZero) |> select(-colsZero)
#   # OUT_total <- OUT_total |> cbind(colsZero) |> filter(!colsZero) |> select(-colsZero)
#
#
#   # 21. Number of PSUs & SSUs -----------------------
#   # across Dimensions
#   # compute number of Primary Sampling Units (PSU) and Secondary Sampling Units (SSU), and number of items (e.g., tally trees)
#   if (length(arena.analyze$dimensions_at_baseunit) == 0) {
#     psu_ssu_counts <- df_analysis_total     |>
#       select( all_of(base_UUID_))           |>
#       unique()                              |>
#       summarize( base_unit_count = n())
#
#     psu_ssu_counts$item_count <- sum( df_analysis_total$entity_count_)
#     # swap column order
#     psu_ssu_counts <- psu_ssu_counts |> relocate( item_count)
#
#   } else {
#     psu_ssu_counts <- df_analysis_total    |>
#       unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
#       select( all_of(base_UUID_), JOIN_COL) |>
#       unique()                             |>
#       summarize( base_unit_count = n(), .by = 'JOIN_COL')
#
#     psu_ssu_counts <- df_analysis_total    |>
#       unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
#       summarize( item_count = sum(entity_count_), .by = 'JOIN_COL') |>
#       right_join( psu_ssu_counts, by = 'JOIN_COL')
#   }
#   psu_ssu_counts
#
#   # 22. Number of clusters ------------------
#   if ( cluster_UUID_ != "" )  {
#     ids_2_survey   <- cluster_UUID_
#     # there must be a more cleaver way of computing the following:
#     if (length(arena.analyze$dimensions_at_baseunit) == 0) {
#       psu_ssu_counts$cluster_count <- df_analysis_total |> select( all_of(cluster_UUID_)) |>
#         unique() |> nrow()
#     } else {
#       psu_ssu_counts <- psu_ssu_counts |>
#         left_join(df_analysis_total |>
#                     unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
#                     select(all_of(cluster_UUID_), JOIN_COL) |>
#                     unique() |>
#                     summarize(cluster_count = n(), .by = 'JOIN_COL'),
#                   by = 'JOIN_COL')
#
#       print( psu_ssu_counts)
#     }
#   }
#
#
#   # 23. Join number of base units and clusters to result tables ----
#   if ( length( arena.analyze$dimensions_at_baseunit) == 0) {
#     OUT_mean$base_unit_count  <- sum( psu_ssu_counts$base_unit_count)
#     OUT_total$base_unit_count <- sum( psu_ssu_counts$base_unit_count)
#     if (!is.null(psu_ssu_counts$cluster_count)) {
#       OUT_mean$cluster_count  <- sum( psu_ssu_counts$cluster_count)
#       OUT_total$cluster_count <- sum( psu_ssu_counts$cluster_count)
#     }
#   } else {
#     OUT_mean  <- OUT_mean  |> left_join(psu_ssu_counts, by = 'JOIN_COL') |> select(-JOIN_COL)
#     OUT_total <- OUT_total |> left_join(psu_ssu_counts, by = 'JOIN_COL') |> select(-JOIN_COL)
#   }
#
#
#   # 24. remove rows where all Dimensions are blank  -----
#   # These are blank rows with zero result values
#   # OUT_total <- OUT_total |>
#   #   mutate(total_char_length = rowSums( across( arena.analyze$dimensions, nchar))) |>
#   #   filter(  total_char_length != 0) |>
#   #   select( -total_char_length)
#   #
#   # OUT_mean <- OUT_mean  |>
#   #   mutate(total_char_length = rowSums( across( arena.analyze$dimensions, nchar))) |>
#   #   filter(  total_char_length != 0) |>
#   #   select( -total_char_length)
#
#   OUT_mean$item_count  <- NULL
#   OUT_total$item_count <- NULL
#
#   dbDisconnect( conn = db)
#
#   # 25. Write results out -----
#   strDimension <- paste( arena.dimensions, collapse = "--")
#   strDimension <- stringr::str_remove( strDimension, paste0(arena.entity,"_"))
#   fileMean     <- paste0( arena.entity, "_MEAN_(",  strDimension, ").csv" )
#   fileTotal    <- paste0( arena.entity, "_TOTAL_(", strDimension, ").csv" )
#
#   print( paste("Survey name:", arena.chainSummary$surveyName))
#   write.csv( OUT_mean,  fileMean,  row.names = F)
#   write.csv( OUT_total, fileTotal, row.names = F)
#
#   return( list("Processing ended", c( fileMean, fileTotal)))
# }
#
# #***************************************************************************
# #***************************************************************************
#
# # # MAIN PROGRAM --------------------
# # # open zip file and unzip it
# # Zip_file <- rstudioapi::selectFile(caption = "Select Zip file",
# #                                    path = getwd(),
# #                                    filter = "ZIP Files (*.zip)",
# #                                    existing = TRUE)
# #
# # if ( !is.null( Zip_file)) {
# #
# #   # returns: list( arena.chainSummary, arena.SchemaSummary, df_ReportEntities)
# #   query1                 <- get_Entities( Zip_file)
# #
# #   # returns the selected Entity name
# #   arena.entity           <- select_Entity( query1[3])
# #
# #   # returns a list of selected Dimensions
# #   arena.dimensions       <- select_Dimensions( arena.entity, query1)
# #
# #   # returns: list( df_wideTable, arena.wideTable_names)
# #   query2                 <- get_WideTable( arena.entity, query1)
# #
# #   setwd("..")
# #   process_message        <- do_analytics( arena.entity, arena.dimensions, query1, query2)
# #
# #   print( process_message)
# #
# # }
