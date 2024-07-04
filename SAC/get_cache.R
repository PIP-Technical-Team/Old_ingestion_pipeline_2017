#' Cache format for SAC functions
#' 
#' This function aims to split the cache (list) in two list for micro/imputed and
#' group/aggregate data. Each list is a data.table/data.frame.
#'
#' @param cache 
#'
#' @return list
#' @export
get_cache <- function(cache) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  dt_ls <- list()
  dt_ls[["micro_imputed"]] <- lapply(cache, function(x){
    if("distribution_type" %in% names(x)){
      x |>
        select(any_of(c("welfare", "welfare_ppp", "weight", "survey_id", "cache_id", "country_code",
                        "surveyid_year", "survey_acronym", "survey_year", "welfare_type",
                        "distribution_type", "gd_type", "imputation_id", "cpi_data_level",
                        "ppp_data_level", "gdp_data_level", "pce_data_level",
                        "pop_data_level", "reporting_level",
                        "area",
                        "cpi", "ppp")))|>
        fsubset(distribution_type %in% c("micro","imputed"))  # |>
      #   ftransform(area = as.character(area))
      
      # setv(dt$area,"", "national")
      
    } else{
      return(NULL)
    }
  })|>
    rowbind(fill=TRUE)
  
  dt_ls[["group_aggregate"]] <- lapply(cache, function(x){
    if("distribution_type" %in% names(x)){
      x|>
        select(any_of(c("welfare", "welfare_ppp", "weight", "survey_id", "cache_id", "country_code",
                        "surveyid_year", "survey_acronym", "survey_year", "welfare_type",
                        "distribution_type", "gd_type", "imputation_id", "cpi_data_level",
                        "ppp_data_level", "gdp_data_level", "pce_data_level",
                        "pop_data_level", "reporting_level",
                        "area",
                        "cpi", "ppp")))|>
        fsubset(distribution_type %in% c("group","aggregate"))  # |>
      #   ftransform(area = as.character(area))
      
      # setv(dt$area,"", "national")
    }else{
      return(NULL)
    }
  })|>
    rowbind(fill=TRUE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt_ls)
  
}