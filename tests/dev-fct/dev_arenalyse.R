
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
  df_resultvars <- .zip$chain_summary$resultVariables |>
    #dplyr::select(name, type, categoryName, parentEntity = entity, label = all_of(label_language)) |>
    dplyr::select(name, type, categoryName, parentEntity = entity, label) |>
    dplyr::mutate(
      report_type = ifelse(type == 'Q', 'measure', 'dimension'),
      type        = ifelse(type == 'Q', 'numeric', 'code'),
      source      = 'chain'
      )

  ## 5.3 Get essential information about Wide Table data ====

  ## Done in 1. into label_language
  ## label_column_name <- paste0("label_", arena.chainSummary$selectedLanguage)

  ## Get information for taxonomies & categories: name, type, categoryName, parentEntity
  # Note: taxonomyName is renamed to --> categoryName
  ## !!! Legacy method
  # schemasum_nameinfo <- .zip$schema_summary |>
  #   dplyr::mutate(categoryName = ifelse(.data$taxonomyName != "", .data$taxonomyName, .data$categoryName)) |>
  #   dplyr::select("name", "type", "categoryName", "parentEntity", label = all_of(label_language)) |>
  #   dplyr::mutate(report_type2 = 'dimension', source2 = 'input')
  #
  # resvar_nameinfo <- df_resultvars |>
  #   dplyr::select(type2 = "type", categoryName2 = "categoryName", parentEntity2 = "parentEntity", label2="label", dplyr::everything())
  #
  # wt_names_legacy2 <- dplyr::tibble(name = names(wt)) |>
  #   dplyr::left_join(schemasum_nameinfo, by = "name") |>
  #   dplyr::left_join(resvar_nameinfo, by = "name") |>
  #   dplyr::mutate(
  #     type   = ifelse(is.na(type), type2, type),
  #     categoryName = ifelse(categoryName == "" | is.na(categoryName), categoryName2, categoryName),
  #     parentEntity = ifelse(parentEntity == "" | is.na(parentEntity), parentEntity2, parentEntity),
  #     label        = ifelse(label        == "" | is.na(label), label2, label),
  #     report_type  = ifelse(report_type  == "" | is.na(report_type), report_type2, report_type),
  #     source       = ifelse(source       == "" | is.na(source), source2, source)
  #   ) |>
  #   dplyr::select(-type2, -categoryName2, -report_type2, -source2, -parentEntity2, -label2) |>
  #   dplyr::mutate(
  #     dimension_baseunit = ifelse(parentEntity == .entity, FALSE, TRUE),
  #     report_type = ifelse(name == 'weight', NA, report_type),
  #     dimension_baseunit = ifelse(name == 'weight', NA, dimension_baseunit)
  #   )

  ## !!! Values don't overlap,
  ## 'schemasum' contains input dimensions while 'resultvars'  contains code dimensions and measures
  ## handled overlaping fields with suffix in left_join()
  ## More clear method would be slice and bind rows.
  schemasum_nameinfo <- .zip$schema_summary |>
    dplyr::mutate(categoryName = ifelse(.data$taxonomyName != "", .data$taxonomyName, .data$categoryName)) |>
    dplyr::select("name", "type", "categoryName", "parentEntity", label = all_of(label_language)) |>
    dplyr::mutate(report_type = 'dimension', source = 'input')

  wt_names <- dplyr::tibble(name = names(wt)) |>
    dplyr::left_join(schemasum_nameinfo, by = "name") |>
    dplyr::left_join(df_resultvars, by = "name", suffix = c("", "_resvar")) |>
    dplyr::mutate(
      type         = ifelse(is.na(type), type_resvar, type),
      categoryName = ifelse(categoryName == "" | is.na(categoryName), categoryName_resvar, categoryName),
      parentEntity = ifelse(parentEntity == "" | is.na(parentEntity), parentEntity_resvar, parentEntity),
      label        = ifelse(label        == "" | is.na(label), label_resvar, label),
      report_type  = ifelse(report_type  == "" | is.na(report_type), report_type_resvar, report_type),
      source       = ifelse(source       == "" | is.na(source), source_resvar, source)
      ) |>
    dplyr::select(-ends_with("_resvar")) |>
    dplyr::mutate(
      dimension_baseunit = ifelse(parentEntity == .entity, FALSE, TRUE),
      report_type = ifelse(name == 'weight', NA, report_type),
      dimension_baseunit = ifelse(name == 'weight', NA, dimension_baseunit)
      )

  ## Checks >> NOT PASSED NEW CODE GIVES report_type and source to all
  ## kept as no consequence
  # identical(wt_names_legacy, wt_names)
  # all.equal(wt_names_legacy, wt_names)

  # tag the stratum attribute
  wt_names$stratum <- FALSE
  if (!is.null(.zip$chain_summary$stratumAttribute)) {
    if (.zip$chain_summary$stratumAttribute != '') {
      wt_names <- wt_names |>
        dplyr::mutate(
          stratum = ifelse(name == .zip$chain_summary$stratumAttribute, TRUE, FALSE)
        )
    }
  }

  ## 5.4. Add category type (Flat/Hierarchical) =====
  # needed to get separate hierarchical code attributes)
  # F: flat table, H: hierarchical table, blank: not code attribute
  ## Find levels in categoryName
  wt_names <- wt_names |>
    dplyr::mutate(
      categoryNameOld = categoryName,
      categoryType = ifelse(stringr::str_detect(categoryName, pattern =  '(?<=\\[).*(?=\\])'), "H", "F"),
      categoryName = stringr::str_remove(categoryNameOld, "\\[.*")
    )

  ## Replaced with mutate call
  # wt_names$categoryName |> stringr::str_detect(pattern =  '(?<=\\[).*(?=\\])')
  # wt_names <- wt_names |> dplyr::mutate(categoryType = ifelse(type == 'code', "F", ""))
  #
  # # check indexes of categoryNames that are hierarchical but on levels 2,3,.. These contains square brackets in SchemaSummary, column 'categoryName'.
  #   i_levels                <- stringr::str_which(wt_names$categoryName, pattern =  '(?<=\\[).*(?=\\])')
  #   if (length(i_levels) > 0) {
  #     for (ix in i_levels) {
  #       wt_names$categoryType[ix] = "H"
  #       wt_names$categoryName[ix]  = stringr::str_sub(wt_names$categoryName[ix], end = -11)
  #     }
  #   }

  #   wideTable_names[ is.na(wideTable_names) ] <- ''
  #   #  print( wideTable_names)
  #
  #   return( list(df_wideTable, wideTable_names ))
  # }

  ## MAIN FUNCTION ####
  ## INPUTS LEGACY: arena.entity, arena.dimensions, arena.clevel, query1, query2
  ## INPUTS NEW:
  ## - .entity, .dims,
  ## - .zip$chain_summary, .zip$schema_summary, df_report_entities (query1)
  ## - wt, wt_names (query2)
  arena.chainSummary    <- .zip$chain_summary
  arena.SchemaSummary   <- .zip$schema_summary
  #  df_ReportEntities     <- query1[[3]] # not needed in this function
  df_wideTable          <- wt
  arena.wideTable_names <- wt_names
  # arena.clevel          <- as.numeric(arena.clevel)
  arena.clevel <- .zip$chain_summary$analysis$pValue
  arena.entity <- .entity
  arena.dimensions <- .dim

  # initialize 'arena.analyze'
  wt_names_dim <- arena.wideTable_names |>
    dplyr::filter(report_type == "dimension") |>
    dplyr::pull(name)

  arena.analyze  <- list(
    entity = arena.entity,              # selected entity name to report, e.g. 'tree'
    dimensions_names_all = wt_names_dim, # list of all possible dimension names of the selected entity
    dimensions = arena.dimensions,      # list of all selected dimensions to report, of the selected entity
    dimensions_at_baseunit = '',        # from previous group, dimensions which belong to base unit level or above, e.g. forest_type, province, etc. but not tree_species, etc.
    dimensions_all_at_baseunit = FALSE, # are all 'dimensions' at the base unit level?
    measures = '',                      # list of Measures, e.g. tree_basal_area, tree_volume_stem, etc.
    stratification = FALSE,             # is this stratified sampling?
    strat_attribute = '',               # stratification attribute name
    stratum_in_dimensions = FALSE,      # is the stratification attribute in the list of report 'dimensions'?
    filter = ''                         # NOT USED YET
  )

  # 1. Base unit UUID and cluster UUID attributes in Wide Table -------------------------------------------------------
  base_UUID_     <- paste0(arena.chainSummary$baseUnit, "_uuid")
  cluster_UUID_  <- ifelse(arena.chainSummary$clusteringEntity != "", paste0(arena.chainSummary$clusteringEntity, "_uuid"), "")

  # 2. Stratification: method & attribute-------------------------------------------------------
  if (arena.chainSummary$samplingStrategy == 3 | arena.chainSummary$samplingStrategy == 4 )  arena.analyze$stratification <- TRUE
  if (arena.analyze$stratification) arena.analyze$strat_attribute <- arena.chainSummary$stratumAttribute

  # 3. List of Dimensions at the base unit level --------------------------
  arena.analyze$dimensions_at_baseunit <- arena.wideTable_names |>
    dplyr::filter(name %in% arena.analyze$dimensions & dimension_baseunit == TRUE) |>
    dplyr::pull(name)

  # ## >> How is this different from just calling .dim or .dims and ensure it's at base unit.
  # .dim
  # .dim_atbaseunit <- arena.wideTable_names |>
  #   dplyr::filter(name %in% .dim, parentEntity != .entity) |>
  #   dplyr::pull(name)

  # If stratum and no other base unit level attributes in Dimensions, then take stratification out for "survey"
  # this works because weight is the expansion area, and a missing entity level attribute, such as a tree_species,
  # will get mean and variation equal to zero in those strata where it does not exists
  if (length(arena.analyze$dimensions_at_baseunit) == 0) {
    arena.analyze$stratification  <- FALSE
    arena.analyze$strat_attribute <- ""
  }

  # 4. stratum in Dimensions? -------------
  # i.e. is that reported?
  # add stratification attribute into list of Dimensions
  if (arena.analyze$stratification) {
    if (arena.analyze$strat_attribute %in% arena.analyze$dimensions) arena.analyze$stratum_in_dimensions <- TRUE

    arena.analyze$dimensions             <- unique(c(arena.analyze$dimensions, arena.analyze$strat_attribute))
    arena.analyze$dimensions_at_baseunit <- unique(c(arena.analyze$dimensions_at_baseunit, arena.analyze$strat_attribute))
  }

  # 5. all Dimensions at the base unit level or above? ------
  if (length(arena.analyze$dimensions) == length(arena.analyze$dimensions_at_baseunit)) arena.analyze$dimensions_all_at_baseunit <- TRUE

  arena.analyze$measures <- arena.wideTable_names |>
    dplyr::filter(report_type == "measure") |>
    dplyr::pull(name)


  # 6. Read analysis data to new DF  ------
  df_analysis_data <- df_wideTable |>
    dplyr::filter(OLAP_baseunit_total == arena.analyze$dimensions_all_at_baseunit) |> # TRUE/FALSE in wide table (last column)
    dplyr::mutate(dplyr::across(dplyr::all_of(arena.analyze$dimensions_names_all), as.character)) |>
    dplyr::select(-OLAP_baseunit_total)


  # 7. Complete data (==> df_analysis_total) --------------------
  if (arena.analyze$dimensions_all_at_baseunit) {
    # all Dimensions are at the base unit level or above.
    df_analysis_total <- df_analysis_data |>
      dplyr::filter(weight > 0) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(arena.analyze$measures),   ~tidyr::replace_na(.x, 0))) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(arena.analyze$dimensions), ~tidyr::replace_na(.x, "NoData")))

  } else {

    # all Dimensions are not at the base unit (there can be e.g. tree_species in Dimensions)
    #  with tidyr::complete, generate all missing base units
    #    https://tidyr.tidyverse.org/reference/complete.html
    #    https://stackoverflow.com/questions/40577484/using-tidyr-complete-with-column-names-specified-in-variables

    # before running 'tidyr::complete', drop out from dataframe all base unit level names, base_UUID_ and cluster_UUID_, excl. stratum attribute
    # names_to_drop <- arena.analyze$dimensions[ arena.analyze$dimensions %in% arena.analyze$dimensions_at_baseunit ]
    ## LOOKING FOR DIMS at base unit to remove them
    names_to_drop <- intersect(arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)
    #
    # if (cluster_UUID_ != "") names_to_drop <- c(names_to_drop, cluster_UUID_)
    #
    # dim_names <- unique(c(base_UUID_, arena.analyze$dimensions))
    # dim_names <- dim_names[ !(dim_names %in% names_to_drop)]
    # dim_names
    ## !!! REVERSED LOGIC, FIND NAMES TO KEEP
    dim_names <- setdiff(arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)
    dim_names <- unique(c(base_UUID_, dim_names))

    if (arena.analyze$stratification) {
      # run 'tidyr::complete' across strata by 'dim_names'
      ## !!! AMBIGUOUS df[][] not recommended, replaced with tidyeval or dplyr::across()
      # df_analysis_data[arena.analyze$strat_attribute][is.na(df_analysis_data[arena.analyze$strat_attribute])] <- "NoData"
      df_analysis_data <- df_analysis_data |>
        dplyr::mutate(
          ## rlang alternative
          # !!arena.analyze$strat_attribute := ifelse(is.na(.data[[arena.analyze$strat_attribute]]), "NoData", .data[[arena.analyze$strat_attribute]])
          dplyr::across(
            .cols = dplyr::all_of(arena.analyze$strat_attribute),
            .fns = ~tidyr::replace_na(.x, "NoData")
            )
        )
      df_analysis_data <- df_analysis_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(arena.analyze$strat_attribute))) |>
        tidyr::complete(!!!rlang::syms(dim_names)) |>
        dplyr::ungroup()
    } else {                                  # run 'tidyr::complete' by 'dim_names'
      df_analysis_data <- df_analysis_data |>
        tidyr::complete(!!!rlang::syms(dim_names))
    }

    # df_analysis_data$entity_count_[is.na(df_analysis_data$entity_count_)] <- 0
    ## Rephrased for consistency with tidyverse
    df_analysis_data <- df_analysis_data |>
      dplyr::mutate(entity_count_ = ifelse(is.na(entity_count_), 0, entity_count_))

    # join back previously dropped dimensions, base_UUID_, weight, exp_factor_
    tmp_dropped_dims <- df_analysis_data |>
      dplyr::filter(!is.na(.data$exp_factor_)) |>
      dplyr::distinct(!!rlang::sym(base_UUID_), .keep_all = T) |>
      dplyr::select(dplyr::all_of(base_UUID_), dplyr::all_of(names_to_drop), "weight", "exp_factor_")
    ## !!! CAN base_UUID_ be more than one var?

    df_analysis_data <- df_analysis_data |>
      dplyr::select(-"weight", -"exp_factor_", -dplyr::all_of(names_to_drop)) |>
      dplyr::left_join(tmp_dropped_dims, by = base_UUID_) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(arena.analyze$measures), ~tidyr::replace_na(., 0)))

    # compute aggregated 'df_analysis_total'
    tmp_analysis_core <- df_analysis_data |>
      dplyr::select(dplyr::all_of(base_UUID_), weight, exp_factor_) |>
      dplyr::distinct()

    grouping_cols <- unique(c(base_UUID_, arena.analyze$dimensions))

    df_analysis_total <- df_analysis_data |>
      dplyr::filter(weight > 0) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      # fix MAX function... wrong!
      dplyr::summarise(
        entity_count_ = max(entity_count_),
        across(
          .cols = dplyr::all_of(arena.analyze$measures),
          # .fns = list(Total = ~sum(.x, na.rm = TRUE )), ## !!! List not needed here?
          .fns = ~sum(.x, na.rm = TRUE),
          .names = "{.col}"
          ),
        .groups = "drop"
        ) |>
      dplyr::left_join(tmp_analysis_core, by = base_UUID_)

    if (cluster_UUID_ != "") {
      tmp_cluster <- df_analysis_data |>
        dplyr::select(dplyr::all_of(base_UUID_), dplyr::all_of(cluster_UUID_)) |>
        dplyr::distinct()

      df_analysis_total <- df_analysis_total |>
        dplyr::left_join(tmp_cluster, by = base_UUID_)
    }

  } ## END data complete

  # 8. Omit data if (exp_factor_ == 0) ------
  # there might be strata which area is set to 0 ha !
  df_analysis_total <- df_analysis_total |> dplyr::filter(.data$exp_factor_ != 0)

  # 9. Initialize cluster & weight variables for "survey" -----
  ids_2_survey     <- NULL
  # Set weight attribute for "survey"
  arena.weights    <- "exp_factor_"

  # 10. Compute DF for means (per hectare values) ------
  df_analysis_mean <- df_analysis_total |>
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(arena.analyze$measures),
        # .fns = list( Mean = ~ .x / exp_factor_), ## List not needed here
        .fns = ~ .x / exp_factor_,
        .names = "{.col}"
      )
    )

  # 11. Process input data for "survey" ----
  # Cluster probability for "survey". If used, weight must be NULL.
  # THIS METHOD IS NOT APPLIED NOW!!
  probs_2_survey   <- NULL

  # Stratification variable for "survey"
  stratum_2_survey <- NULL
  if (arena.analyze$stratification) {
    stratum_2_survey <- arena.analyze$strat_attribute
    # missing stratum code set to "", in order to report these cases too
    ## !!! Base code handling of calling columns by name stored as chr vector
    ## Use of df[][] considered ambiguous.
    # df_analysis_mean[stratum_2_survey][is.na(df_analysis_mean[ stratum_2_survey]) ] <- ""
    ## tidyeval alternative
    # df_analysis_mean <- df_analysis_mean |>
    #   mutate(
    #     !!arena.analyze$strat_attribute := ifelse(is.na(.data[[arena.analyze$strat_attribute]]), "", .data[[arena.analyze$strat_attribute]])
    #   )
    ## With dplyr across()
    df_analysis_mean <- df_analysis_mean |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(arena.analyze$strat_attribute),
        .fns = ~ tidyr::replace_na(.x, "")
      ))
  }

  # 12. add serial number required by SQLite DB -----
  #df_analysis_mean$serialno <- 1:nrow(df_analysis_mean)
  df_analysis_mean <- df_analysis_mean |>
    dplyr::mutate(serialno = dplyr::row_number()) |>
    dplyr::select("serialno", dplyr::everything())

  # 13. Set up SQLite database and table ----
  # https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-database.html
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  db_analysis_mean <- dplyr::copy_to(db, df_analysis_mean, "df_analysis_mean", temporary = FALSE)

  ## compare object sizes: SQLite is much smaller
  # object.size(df_analysis_mean)
  # object.size(df_analysis_mean_db)

  # an example how to use:
  # df_analysis_mean_db |> select(tree_count) |> pull()

  rm(df_analysis_mean);  rm(df_analysis_data)

  # # 14. survey S3 object: SRS case (for efficiency of stratification) -----
  # # if stratification, compute SRS case. Needed to compute efficiency of stratification.
  # if ( arena.analyze$stratification ) {
  #     design_srvyr_SRS <- df_analysis_mean_db  |>
  #       srvyr::as_survey_design(
  #         ids       = !!ids_2_survey,
  #         strata    = NULL,
  #         fpc       = NULL,
  #         weights   = !!arena.weights,
  #         variables = c( arena.analyze$dimensions, arena.analyze$measures, exp_factor_ ))
  # }

  # 15. survey S3 object  -----
  design_srvyr_mean <-  db_analysis_mean |>
    srvyr::as_survey_design(
      ids       = !!ids_2_survey,
      strata    = !!stratum_2_survey,
      probs     = !!probs_2_survey,  # Cluster sampling probabilities, set to NULL now
      fpc       = NULL,              # Finite population correction
      weights   = !!arena.weights,
      nest      = TRUE,              # If TRUE, relabel cluster ids to enforce nesting within strata
      variables = c( arena.analyze$dimensions, arena.analyze$measures, exp_factor_ )

    )



  # 16. Remove stratum from Dimensions? ----
  #     Remove it if not reported
  if (arena.analyze$stratification & !arena.analyze$stratum_in_dimensions) {
    arena.analyze$dimensions <- arena.analyze$dimensions[!(arena.analyze$dimensions %in% arena.analyze$strat_attribute)]
    arena.analyze$dimensions_at_baseunit <- arena.analyze$dimensions_at_baseunit[!(arena.analyze$dimensions_at_baseunit %in% arena.analyze$strat_attribute)]
  }

  # 17. Compute aggregated Expansion areas ------
  # these are later in step #
  if (length(arena.analyze$dimensions_at_baseunit) == 0) {
    # no Dimensions at the base unit, e.g. only 'tree_species' is reported
    TotalArea_ <- df_analysis_total |>
      dplyr::select(dplyr::all_of(base_UUID_), area_ = exp_factor_) |>
      dplyr::distinct() |> # select unique base units (i.e. samples)
      dplyr::pull(area_) |> # get total sum of areas
      sum()
  } else {
    # by combinations of dimensions at the base unit level
    df_AREAS <- df_analysis_total |>
      tidyr::unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>  # create a temporary column for later join
      dplyr::select(dplyr::all_of(base_UUID_), JOIN_COL, exp_factor_)  |>
      dplyr::distinct() |>  # compute sum of areas by base unit level dimensions
      dplyr::summarize(area_ = sum(exp_factor_), .by = JOIN_COL)
  }

  start.time <- Sys.time()

  # A test with PNG data, 1st version: 7.01 mins
  # fastsurvey: Time difference of 4.7 mins
  # with sqlLite database and fastsurvey: Time difference of 3.68 mins


  # 18. survey: run survey_mean() --------------
  # MEANS (per hectares) for selected categories
  OUT_mean  <- design_srvyr_mean |>
    dplyr::collect() |>
    dplyr::group_by(dplyr::across(arena.analyze$dimensions)) |>
    dplyr::summarize(
      dplyr::across(
        .cols = any_of(arena.analyze$measures),
        .fns = list( ~ srvyr::survey_mean(.x, na.rm = FALSE, vartype = c("se","ci"), proportion = FALSE, level = arena.clevel, df = Inf ))
        )
      ) |>
    dplyr::rename_with(.fn = ~ stringr::str_replace(.x, "_1", "_1_"), .cols = dplyr::ends_with("_1"))

  if (length(arena.analyze$dimensions_at_baseunit) == 0) {
    OUT_mean <- OUT_mean |> dplyr::mutate( area_ = TotalArea_)
  } else {
    OUT_mean <- OUT_mean |>
      # create a temporary column for joining the expansion area table
      tidyr::unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep = "**", remove = FALSE, na.rm = FALSE) |>
      # join expansion area table
      dplyr::left_join(df_AREAS, by = 'JOIN_COL')
  }

  end.time <- Sys.time()
  time.taken <- round(end.time - start.time, 2)
  print(time.taken)

  # 19. Compute totals -----
  OUT_total <- OUT_mean |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::contains("_1_"),
        .fns = ~.x * area_  ,
        .names = "{col}"
      ) #, .keep = 'all' ## !!! NOT NEEDED, default is .keep = "all
    ) |>
    dplyr::rename_with(.fn = ~ stringr::str_replace(.x, "_1_", "_"), .cols = dplyr::contains("_1_")) |>
    dplyr::rename_with(.fn = ~ stringr::str_sub(.x, end = -2)      , .cols = dplyr::ends_with("_"))

  # 20. Clean output tables----------------
  # clear header texts
  OUT_mean  <- OUT_mean |>
    dplyr::rename_with(.fn = ~ stringr::str_replace(.x, "_1_", "_"), .cols = dplyr::contains("_1_")) |>
    dplyr::rename_with(.fn = ~ stringr::str_sub(.x, end= -2)       , .cols = dplyr::ends_with("_"))

  ## !!! THIS COERCE numeric data to character: 1, NA, 3 becomes "1", "", "3" replaced with character only
  # OUT_total[is.na(OUT_total)] <- ""
  # OUT_mean[is.na(OUT_mean)] <- ""
  OUT_mean <- OUT_mean |>
    dplyr::mutate(
      dplyr::across(
       .cols = dplyr::where(is.character),
       .fns = ~tidyr::replace_na(.x, "")
      )
    )

  OUT_total <- OUT_total |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.character),
        .fns = ~tidyr::replace_na(.x, "")
      )
    )

  # convert negative lower confidence intervals to zeros
  OUT_mean <- OUT_mean |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_low"),
        .fns = ~ dplyr::if_else(.x < 0, 0,.x)
      )
    )

  OUT_total <- OUT_total |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_low"),
        .fns = ~ dplyr::if_else(.x < 0, 0, .x)
      )
    )

  # remove rows with only zeros in Measures. ???
  # colsZero = ncol(OUT_mean) - rowSums(OUT_mean==0) - length(arena.analyze$dimensions) - 1
  # colsZero =  ( colsZero == 0 )
  # OUT_mean  <- OUT_mean  |> cbind(colsZero) |> filter(!colsZero) |> select(-colsZero)
  # OUT_total <- OUT_total |> cbind(colsZero) |> filter(!colsZero) |> select(-colsZero)


  # 21. Number of PSUs & SSUs -----------------------
  # across Dimensions
  # compute number of Primary Sampling Units (PSU) and Secondary Sampling Units (SSU), and number of items (e.g., tally trees)
  if (length(arena.analyze$dimensions_at_baseunit) == 0) {
    psu_ssu_counts <- df_analysis_total |>
      dplyr::select(dplyr::all_of(base_UUID_)) |>
      dplyr::distinct() |>
      dplyr::summarize(base_unit_count = dplyr::n())

    psu_ssu_counts$item_count <- sum(df_analysis_total$entity_count_)
    # swap column order
    psu_ssu_counts <- psu_ssu_counts |> dplyr::relocate(item_count)
  } else {
    psu_ssu_counts <- df_analysis_total |>
      tidyr::unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
      dplyr::select(dplyr::all_of(base_UUID_), JOIN_COL) |>
      dplyr::distinct() |>
      dplyr::summarize(base_unit_count = dplyr::n(), .by = 'JOIN_COL')

    psu_ssu_counts <- df_analysis_total |>
      tidyr::unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
      dplyr::summarize(item_count = sum(entity_count_), .by = 'JOIN_COL') |>
      dplyr::right_join(psu_ssu_counts, by = 'JOIN_COL')
  }
  # psu_ssu_counts

  # 22. Number of clusters ------------------
  if (cluster_UUID_ != "")  {
    ids_2_survey   <- cluster_UUID_
    # there must be a more clever way of computing the following:
    if (length(arena.analyze$dimensions_at_baseunit) == 0) {
      psu_ssu_counts$cluster_count <- df_analysis_total |>
        dplyr::select(dplyr::all_of(cluster_UUID_)) |>
        unique() |>
        nrow()
    } else {
      ssu_count <- df_analysis_total |>
        tidyr::unite("JOIN_COL", arena.analyze$dimensions_at_baseunit, sep="**", remove = FALSE, na.rm = FALSE) |>
        dplyr::select(dplyr::all_of(cluster_UUID_), JOIN_COL) |>
        dplyr::distinct() |>
        dplyr::summarize(cluster_count = dplyr::n(), .by = 'JOIN_COL')

      psu_ssu_counts <- psu_ssu_counts |> left_join(ssu_count, by = 'JOIN_COL')

      print( psu_ssu_counts)
    }
  }


  # 23. Join number of base units and clusters to result tables ----
  if (length(arena.analyze$dimensions_at_baseunit) == 0) {
    OUT_mean$base_unit_count  <- sum(psu_ssu_counts$base_unit_count)
    OUT_total$base_unit_count <- sum(psu_ssu_counts$base_unit_count)

    if (!is.null(psu_ssu_counts$cluster_count)) {
      OUT_mean$cluster_count  <- sum(psu_ssu_counts$cluster_count)
      OUT_total$cluster_count <- sum(psu_ssu_counts$cluster_count)
    }
  } else {
    OUT_mean  <- OUT_mean  |> dplyr::left_join(psu_ssu_counts, by = 'JOIN_COL') #|> dplyr::select(-JOIN_COL)
    OUT_total <- OUT_total |> dplyr::left_join(psu_ssu_counts, by = 'JOIN_COL') #|> dplyr::select(-JOIN_COL)
  }


  # 24. remove rows where all Dimensions are blank  -----
  # These are blank rows with zero result values
  # OUT_total <- OUT_total |>
  #   mutate(total_char_length = rowSums( across( arena.analyze$dimensions, nchar))) |>
  #   filter(  total_char_length != 0) |>
  #   select( -total_char_length)
  #
  # OUT_mean <- OUT_mean  |>
  #   mutate(total_char_length = rowSums( across( arena.analyze$dimensions, nchar))) |>
  #   filter(  total_char_length != 0) |>
  #   select( -total_char_length)

  ## !!! Why remove?
  # OUT_mean$item_count  <- NULL
  # OUT_total$item_count <- NULL

  DBI::dbDisconnect( conn = db)

  # 25. Write results out -----
  ## !!! Changed to output list
  # strDimension <- paste( arena.dimensions, collapse = "--")
  # strDimension <- stringr::str_remove( strDimension, paste0(arena.entity,"_"))
  # fileMean     <- paste0( arena.entity, "_MEAN_(",  strDimension, ").csv" )
  # fileTotal    <- paste0( arena.entity, "_TOTAL_(", strDimension, ").csv" )
  #
  # print( paste("Survey name:", arena.chainSummary$surveyName))
  # write.csv( OUT_mean,  fileMean,  row.names = F)
  # write.csv( OUT_total, fileTotal, row.names = F)
  #
  # return( list("Processing ended", c( fileMean, fileTotal)))
  list(MEANS = OUT_mean, TOTALS = OUT_total)

}










