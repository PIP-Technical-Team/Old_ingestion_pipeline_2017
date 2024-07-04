#' Create survey estimation table
#' 
#' Merge the survey mean and distributional statistics tables
#'
#' @param dsm_table data.table: Output of `db_create_dsm_table_sac`
#' @param dist_table data.table: Output of `db_create_dist_table_sac`
#' @param gdp_table 
#' @param pce_table 
#'
#' @return data.table
#' @export
db_create_svy_estimation_table_sac <- function(dsm_table, dist_table, gdp_table, pce_table) {
  
  # TEMP FIX: TO BE REMOVED (Diana: Do we still need it?)
  dist_table$survey_id <- toupper(dist_table$survey_id)
  dsm_table$survey_id <- toupper(dsm_table$survey_id)
  
  # Remove cols 
  dist_table$reporting_year <- NULL
  gdp_table$gdp_domain <- NULL
  pce_table$pce_domain <- NULL
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merge tables --------
  
  # Merge DSM table w/ dist stat table (full join)
  dt <- joyn::joyn(dsm_table, 
                   dist_table, 
                   match_type = "1:1",
                   by = c("cache_id","pop_data_level","reporting_level"),
                   reportvar = FALSE)
  
  # Merge with GDP
  dt <- data.table::merge.data.table(
    dt, gdp_table,
    all.x = TRUE,
    by.x = c("country_code", "reporting_year", "gdp_data_level"),
    by.y = c("country_code", "year", "gdp_data_level")
  )
  
  # Merge with PCE
  
  dt <- data.table::merge.data.table(
    dt, pce_table,
    all.x = TRUE,
    by.x = c("country_code", "reporting_year", "pce_data_level"),
    by.y = c("country_code", "year", "pce_data_level")
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Warnings --------
  
  # Remove rows with missing survey_mean_ppp
  # This shouldn't be the case
  # A problem with PHL 2009
  if (anyNA(dt$survey_mean_ppp)) {
    rlang::warn(c(
      sprintf(
        "Removing %s rows with missing `survey_mean_ppp`: ",
        fsum(is.na(dt$survey_mean_ppp))
      ),
      funique(dt[is.na(survey_mean_ppp)]$cache_id)
    ))
    dt <- dt[!is.na(survey_mean_ppp), ]
  }
  
  # Remove rows with missing ppp
  # CHN, IDN, why?
  if (anyNA(dt$ppp)) {
    rlang::warn(c(
      sprintf(
        "Removing %s rows with missing `ppp`:",
        fsum(is.na(dt$ppp))
      ),
      funique(dt[is.na(ppp)]$cache_id)
    ))
    dt <- dt[!is.na(ppp), ]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Finalize table --------
  
  # Fix and add columns
  dt$estimation_type <- "survey"
  dt$predicted_mean_ppp <- numeric(0)
  dt <- data.table::setnames(dt, 
                             c("gdp", "pce", "pcn_region_code"),
                             c("reporting_gdp", "reporting_pce", "region_code")
  )
  
  # Order final columns
  cols <- c(
    "survey_id", "cache_id", "region_code", "wb_region_code",
    "country_code", "reporting_year", "surveyid_year",
    "survey_year", "survey_time", "survey_acronym", "survey_coverage",
    "survey_comparability", "comparable_spell", "welfare_type",
    "reporting_level",
    "survey_mean_lcu", "survey_mean_ppp",
    "survey_median_ppp", "survey_median_lcu",
    "predicted_mean_ppp", "ppp", "cpi",
    "reporting_pop", "reporting_gdp",
    "reporting_pce", "pop_data_level",
    "gdp_data_level", "pce_data_level",
    "cpi_data_level", "ppp_data_level",
    "distribution_type", "gd_type",
    "is_interpolated",
    "is_used_for_line_up", "is_used_for_aggregation",
    "estimation_type",
    "display_cp"
  )
  
  dt <- fselect(dt, cols)
  
  return(dt)
}