
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
  ## >> Legacy method
  # schemasum_nameinfo <- .zip$schema_summary |>
  #   dplyr::mutate(categoryName = ifelse(.data$taxonomyName != "", .data$taxonomyName, .data$categoryName)) |>
  #   dplyr::select("name", "type", "categoryName", "parentEntity", label = all_of(label_language)) |>
  #   dplyr::mutate(report_type2 = 'dimension', source2 = 'input')
  #
  # resvar_nameinfo <- df_resultvars |>
  #   dplyr::select(type2 = "type", categoryName2 = "categoryName", parentEntity2 = "parentEntity", label2="label", dplyr::everything())
  #
  # wt_names_legacy <- dplyr::tibble(name = names(wt)) |>
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

  ## Reversed logic >> understood that here we want schema_summary first and fill in missing values with resultVariables
  schemasum_nameinfo <- .zip$schema_summary |>
    dplyr::mutate(categoryName = ifelse(.data$taxonomyName != "", .data$taxonomyName, .data$categoryName)) |>
    dplyr::select("name", "type", "categoryName", "parentEntity", label = all_of(label_language))

  wt_names <- dplyr::tibble(name = names(wt)) |>
    dplyr::left_join(schemasum_nameinfo, by = "name") |>
    dplyr::left_join(df_resultvars, by = "name", suffix = c("", "_resvar")) |>
    dplyr::mutate(
      ## From schema_summary
      type         = ifelse(is.na(type), type_resvar, type),
      categoryName = ifelse(categoryName == "" | is.na(categoryName), categoryName_resvar, categoryName),
      parentEntity = ifelse(parentEntity == "" | is.na(parentEntity), parentEntity_resvar, parentEntity),
      label        = ifelse(label        == "" | is.na(label), label_resvar, label),
      ## From df_resultvars
      report_type  = ifelse(report_type  == "" | is.na(report_type), "dimension", report_type),
      source       = ifelse(source       == "" | is.na(source), "input", source)
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
  if (length( arena.analyze$dimensions_at_baseunit) == 0) {
    arena.analyze$stratification  <- FALSE
    arena.analyze$strat_attribute <- ""
  }

  # 4. stratum in Dimensions? -------------
  # i.e. is that reported?
  # add stratification attribute into list of Dimensions
  if ( arena.analyze$stratification ) {
    if (arena.analyze$strat_attribute %in% arena.analyze$dimensions) arena.analyze$stratum_in_dimensions <- TRUE

    arena.analyze$dimensions             <- unique( c( arena.analyze$dimensions, arena.analyze$strat_attribute))
    arena.analyze$dimensions_at_baseunit <- unique( c( arena.analyze$dimensions_at_baseunit, arena.analyze$strat_attribute))
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
      dplyr::filter( weight > 0) |>
      dplyr::mutate( across( any_of( arena.analyze$measures),   ~tidyr::replace_na(.x, 0))) |>
      dplyr::mutate( across( any_of( arena.analyze$dimensions), ~tidyr::replace_na(.x, "NoData")))

  } else {  # all Dimensions are not at the base unit (there can be e.g. tree_species in Dimensions)
    #  with tidyr::complete, generate all missing base units
    #    https://tidyr.tidyverse.org/reference/complete.html
    #    https://stackoverflow.com/questions/40577484/using-tidyr-complete-with-column-names-specified-in-variables

    # before running 'tidyr::complete', drop out from dataframe all base unit level names, base_UUID_ and cluster_UUID_, excl. stratum attribute
    # names_to_drop <- arena.analyze$dimensions[ arena.analyze$dimensions %in% arena.analyze$dimensions_at_baseunit ]
    ## LOOKING FOR DIMS at base unit to remove them
    # names_to_drop <- intersect(arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)
    #
    # if (cluster_UUID_ != "") names_to_drop <- c(names_to_drop, cluster_UUID_)
    #
    # dim_names <- unique(c(base_UUID_, arena.analyze$dimensions))
    # dim_names <- dim_names[ !(dim_names %in% names_to_drop)]
    # dim_names
    ## !!! REVERSED LOGIC, FIND NAMES TO KEEP
    dim_names <- setdiff(arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)
    dim_names <- unique(c(base_UUID_, dim_names))

    if (arena.analyze$stratification) {# run 'tidyr::complete' across strata by 'dim_names'
      ## !!! AMBIGUOUS df[][] not recommended, relaced with rlang
      # df_analysis_data[arena.analyze$strat_attribute][is.na(df_analysis_data[arena.analyze$strat_attribute])] <- "NoData"
      df_analysis_data <- df_analysis_data |>
        dplyr::mutate(
          ## !! Requires in roxygen @importFrom rlang := !! !!! sym .data
          !!arena.analyze$strat_attribute := ifelse(is.na(.data[[arena.analyze$strat_attribute]]), "NoData", .data[[arena.analyze$strat_attribute]])
        )
      df_analysis_data <- df_analysis_data                        |>
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
      dplyr::select( dplyr::all_of(base_UUID_), dplyr::all_of(names_to_drop), "weight", "exp_factor_")
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
  }



}










