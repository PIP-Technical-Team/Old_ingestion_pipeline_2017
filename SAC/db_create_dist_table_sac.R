#' Calculations of distributional statistics 
#'
#' @param cache data.table: output of `get_cache`
#' @param mean_table data.table: output of `db_create_dsm_table_sac`
#'
#' @return data.table
#' @export
db_dist_stats_sac <- function(cache, 
                              mean_table){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1. Select variables and subset for Micro data----
  
  dt_m <- cache[["micro_imputed"]] |>
    fselect(cache_id, distribution_type, 
            cpi_data_level, ppp_data_level,
            gdp_data_level, pce_data_level,
            pop_data_level, reporting_level,
            imputation_id, weight, welfare_ppp)
  
  # 2. Micro Data: Level Estimation  ----
  
  md_id_level <- dt_m |>
    roworder(cache_id, pop_data_level, welfare_ppp) |>
    _[, as.list(wrp_md_dist_stats(welfare = welfare_ppp,
                                  weight  = weight,
                                  mean = NULL)),
      by = .(cache_id, imputation_id, 
             pop_data_level, reporting_level)]|>
    fgroup_by(cache_id, 
              pop_data_level, reporting_level)|>
    collapg(fmean, cols = c("mean","median","gini",
                            "polarization","mld",
                            paste0("decile",1:10)))|>
    fungroup()|>
    frename(survey_median_ppp = median)|>
    fmutate(reporting_level = as.character(reporting_level),
            pop_data_level = as.character(pop_data_level))
  
  
  # 3. Micro and Imputed Data: National Estimation ----
  
  md_id_national <- dt_m |>
    # Gc Note: this is equivalent to having pop_data_level > 1 and D2 in cache_id:
    fsubset(reporting_level != 'national' & ppp_data_level != 'national') |>
    roworder(cache_id, imputation_id, welfare_ppp)|>
    _[, as.list(wrp_md_dist_stats(welfare = welfare_ppp, weight = weight)),
      by = .(cache_id, imputation_id)]|>
    fgroup_by(cache_id)|>
    collapg(fmean, cols = c("mean","median","gini",
                            "polarization","mld",
                            paste0("decile",1:10)))|>
    fungroup()|>
    frename(survey_median_ppp = median)|>
    fmutate(reporting_level = as.character("national"),
            pop_data_level = as.character("national")) 
  
  if(nrow(cache[["group_aggregate"]])!=0){
    
    # Select variables, subset and join mean table
    dt_jn <- cache[["group_aggregate"]] |>
      fselect(cache_id, distribution_type, imputation_id, 
              pop_data_level, reporting_level, weight, welfare) |>
      collapse::join(mean_table |> 
                       fselect(cache_id, 
                               pop_data_level, reporting_level, 
                               survey_mean_ppp, reporting_pop),
                     on=c("cache_id", 
                          "pop_data_level", "reporting_level"), 
                     validate = "m:1",
                     verbose = 0,
                     overid = 2,
                     column = list(".joyn", c("x", "y", "x & y"))) # immediate
    
    # MISSING WARNING MESSAGE
    
    dt_jn <- dt_jn|>
      fsubset(.joyn != "y")|>
      fselect(-.joyn)
    
    
    # 4. Group and Aggregate Data: Level and Area Estimation -----
    
    gd_ag_level <- dt_jn |>
      roworder(cache_id, 
               pop_data_level, reporting_level, welfare) |>
      _[, as.list(safe_wrp_gd_dist_stats(welfare = welfare,
                                         population = weight,
                                         mean = funique(survey_mean_ppp))),
        by = .(cache_id, 
               pop_data_level, reporting_level)]|>
      frename(survey_median_ppp = median)|>
      fmutate(reporting_level = as.character(reporting_level),
              pop_data_level = as.character(pop_data_level))
    
    
    setrename(gd_ag_level, gsub("deciles", "decile", names(gd_ag_level)))
    
    # 4. Aggregate Data: National estimation (synth needed) ----
    
    ag_syn <- dt_jn |>
      fsubset(distribution_type %in% c("aggregate")) |>
      roworder(cache_id, 
               pop_data_level, reporting_level, welfare) |>
      fgroup_by(cache_id, 
                pop_data_level, reporting_level)|>
      fsummarise(welfare =  wbpip:::sd_create_synth_vector(
        welfare = welfare,
        population = weight,
        mean = funique(survey_mean_ppp),
        pop = funique(reporting_pop))$welfare,
        weight = funique(reporting_pop)/100000) 
    
    # Aggregate to national
    
    ag_national <- ag_syn |> 
      fsubset(!is.na(welfare))|> # Patch to eliminate NA from IDN error
      roworder(cache_id, welfare)|>
      _[, as.list(wrp_md_dist_stats(welfare = welfare, weight = weight)),
        by = .(cache_id)]|>
      fgroup_by(cache_id)|>
      collapg(fmean, cols = c("mean","median","gini",
                              "polarization","mld",
                              paste0("decile",1:10)))|>
      fungroup()|>
      frename(survey_median_ppp = median)|>
      fmutate(reporting_level = as.character("national"),
              pop_data_level = as.character("national")) 
    
    # 5. Row bind and return ----
    
    final <- rowbind(md_id_level, md_id_national, gd_ag_level, ag_national)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(final)
    
  }
  
  final <- rowbind(md_id_level, md_id_national)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(final)
  
}

#' Create distributional statistics table
#' 
#' Add metadata, eliminate ppp/cpi, and modified other variables
#' Note: Missing treatment of quantiles from previous code
#'
#' @param dt data.table: output of `db_dist_stats_sac`
#' @param dsm_table data.table: output of `db_create_dsm_table_sac`
#' @param cache_inventory data.table
#'
#' @return data.table
#' @export
db_create_dist_table_sac <- function(dt, dsm_table, cache_inventory){
  
  dt_clean <- dt |>
    collapse::join(dsm_table|>
                     fselect("survey_id", "cache_id", "wb_region_code", "pcn_region_code",
                             "country_code", "surveyid_year", "survey_year",
                             "reporting_year", "survey_acronym", "welfare_type",
                             "cpi", "ppp", "pop_data_level", "reporting_level"),
                   on=c("cache_id", "reporting_level", "pop_data_level"),
                   validate = "1:1",
                   how = "left",
                   verbose = 0,
                   overid = 2)
  
  dt_clean[cache_inventory, 
           # I think this is a patch because  it does not check if We
           # are using the last version of dataliweb.
           on = "cache_id",
           survey_id := i.survey_id
  ]
  
  dt_clean <- dt_clean |>
    fmutate(survey_median_lcu = survey_median_ppp*ppp*cpi,
            survey_id = toupper(survey_id))|>
    fselect(-ppp, -cpi)|>
    colorder(survey_id, cache_id, wb_region_code, pcn_region_code, country_code,
             survey_acronym, surveyid_year, survey_year, reporting_year, welfare_type,
             reporting_level, survey_median_lcu, survey_median_ppp, decile1:decile10,
             mean, gini, polarization, mld, pop_data_level)  
  
  # change factors to characters
  dt_clean <- dt_clean |>
    fcomputev(is.factor, as.character, keep = names(dt_clean))
  
  return(dt_clean)
}