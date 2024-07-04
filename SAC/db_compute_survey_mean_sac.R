#' Calculate survey means
#' 
#' Calculate survey mean in local currency units (LCU) for a single survey
#' dataset.
#'
#' @param cache list: output from `get_cache` function.
#' @param gd_mean numeric: Mean to use for grouped data surveys.
#'
#' @return data.table
#' @export
db_compute_survey_mean_sac <- function(cache, 
                                       gd_mean) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ------ Prepare data -----
  
  # Select variables for metadata
  metadata_vars <- c("cache_id", "reporting_level", 
                     "survey_id", "country_code", "surveyid_year", 
                     "survey_acronym","survey_year", "welfare_type", 
                     "distribution_type","gd_type","cpi_data_level",
                     "ppp_data_level", "gdp_data_level", 
                     "pce_data_level", "pop_data_level",
                     "cpi", "ppp")
  
  # ---- Micro data no urban/rural -----
  
  dt_c <- cache[["micro_imputed"]] |>
    fgroup_by(cache_id, reporting_level,
              imputation_id)|> 
    #collapg(custom = list(fmean = c(survey_mean_lcu = "welfare")), w = weight)|>
    fsummarise(survey_mean_lcu = fmean(welfare, w = weight), 
               weight          = fsum(weight)) |>
    fgroup_by(cache_id, reporting_level)|>
    #collapg(custom = list(fmean = c(survey_mean_lcu = "survey_mean_lcu")), w = weight)|>
    fsummarise(survey_mean_lcu = fmean(survey_mean_lcu, w = weight)) |>
    fungroup()
  
  dt_meta_vars <- cache[["micro_imputed"]] |>
    get_vars(metadata_vars) |>
    funique()
  
  dt_c <- joyn::joyn(dt_meta_vars, dt_c,
                     by = c("cache_id", "reporting_level"),
                     match_type = "m:1",
                     reportvar = FALSE)
  
  #  ------ Group data -----
  
  if(nrow(cache[["group_aggregate"]])!=0){
    
    dt_g <- cache[["group_aggregate"]] |>
      joyn::joyn(gd_mean[!is.na(survey_mean_lcu)],
                 by = c(
                   "cache_id", "pop_data_level"
                 ),
                 y_vars_to_keep = "survey_mean_lcu",
                 match_type = "m:1", 
                 keep = "left", 
                 reportvar = FALSE, 
                 sort = FALSE)
    
    dt_c <- collapse::rowbind(dt_c, dt_g) 
    
  }
  
  # ----- Finalize table -----
  
  sort_vars <- c("survey_id",
                 "country_code",
                 "surveyid_year", 
                 "survey_acronym",
                 "survey_year", 
                 "welfare_type")
  
  setorderv(dt_c, sort_vars) # Order rows
  
  setcolorder(dt_c, sort_vars) # Order columns
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt_c
  
}



#' Group Data Means Table
#' 
#' Transform gdm data to daily values. 
#'
#' @param cache_inventory 
#' @param gdm 
#'
#' @return data.table/data.frame
get_groupdata_means_sac <- function(cache_inventory = cache_inventory, gdm = dl_aux$gdm){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  gd_means <- joyn::joyn(x          = cache_inventory,
                         y          = gdm,
                         by         = c("survey_id", "welfare_type"),
                         match_type = "1:m",
                         y_vars_to_keep = c("survey_mean_lcu", "pop_data_level"),
                         keep       = "left")
  
  
  gd_means <- gd_means |>
    setorderv(c("cache_id", "pop_data_level"))|>
    fselect(cache_id, pop_data_level, survey_mean_lcu)|>
    fmutate(survey_mean_lcu = survey_mean_lcu*(12/365))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(gd_means)
}