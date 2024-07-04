#' Merge aux data to survey mean table (LCU)
#' 
#' Add population and Price Framewor information to survey mean table (LCU)
#'
#' @param dt data.table: output of `db_compute_survey_mean_sac`
#' @param pop_table data.table: population data
#' @param pfw_table data.table: Price Framework data
#'
#' @return data.table
#' @export
db_create_lcu_table_sac <- function(dt, pop_table, pfw_table) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ---- Merge with PFW ----
  
  # Select columns and merge LCU table with PFW (left join)
  dt <- joyn::joyn(dt, pfw_table|>
                     fselect(wb_region_code, pcn_region_code,
                             country_code, survey_coverage,
                             surveyid_year, survey_acronym,
                             reporting_year, survey_comparability,
                             display_cp, survey_time),
                   by = c(
                     "country_code",
                     "surveyid_year",
                     "survey_acronym"
                   ),
                   match_type = "m:1"
  )
  
  if (nrow(dt[.joyn == "x"]) > 0) {
    msg <- "We should not have NOT-matching observations from survey-mean tables"
    hint <- "Make sure PFW table is up to date"
    rlang::abort(c(
      msg,
      i = hint,
      i = "Make sure .dta data is up to date by running pipdp"
    ),
    class = "pipdm_error"
    )
  }
  
  dt <- dt|>
    fsubset(.joyn != "y")|>
    fselect(-.joyn)
  
  #--------- Merge with POP ---------
  
  pop_table$pop_domain <- NULL 
  
  # --- Reporting_pop ----
  
  dt <- joyn::joyn(dt, pop_table,
                   by = c("country_code",
                          "reporting_year = year",
                          #"area = pop_data_level"
                          "pop_data_level"
                   ),
                   match_type = "m:1"
                   #keep = "left"
  )
  
  #There is an error for the area level (see if it affects later on)
  #if (nrow(dt[(.joyn == "x" & reporting_level==area)]) > 0) { 
  if (nrow(dt[.joyn == "x"]) > 0) { 
    msg <- "We should not have NOT-matching observations from survey-mean tables"
    hint <- "Make sure POP data includes all the countries and pop data levels"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )
  }
  
  dt <- dt|>
    fsubset(.joyn != "y")|>
    fselect(-.joyn)|>
    setnames("pop", "reporting_pop")
  
  # ---- Survey_pop ----
  
  dt_svy_pop <- dt|>
    fsubset(survey_year != floor(survey_year)) |>
    rowbind(dt|> fsubset(survey_year != floor(survey_year)), idcol = "id")|>
    fmutate(year_rnd = case_when(id == 1 ~ ceiling(survey_year),
                                 id == 2 ~ floor(survey_year),
                                 .default = NA_integer_),
            diff = 1 - abs(survey_year-year_rnd))|>
    joyn::joyn(pop_table, 
               by = c("country_code", 
                      "year_rnd = year",
                      #"area = pop_data_level"
                      "pop_data_level"
               ),
               match_type = "m:1",
               keep = "left"
    )
  
  
  if (nrow(dt_svy_pop[.joyn == "x"]) > 0) {
    msg <- "We should not have NOT-matching observations from survey-mean tables"
    hint <- "Make sure POP data includes all the countries and pop data levels"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )
  }
  
  dt_svy_pop <- dt_svy_pop|>
    fgroup_by(survey_id, country_code, survey_year,
              #reporting_level, area)|>
              reporting_level)|>
    collapg(custom = list(fmean = "pop"), w = diff)|>
    frename(survey_pop = pop)|>
    fungroup()
  
  dt <- joyn::joyn(dt, dt_svy_pop,
                   by = c("survey_id", 
                          "country_code", 
                          "survey_year",
                          "reporting_level"
                          #"area"
                   ),
                   match_type = "m:1",
                   keep = "left"
  )
  
  dt <- dt|>
    fsubset(.joyn != "y")|>
    fselect(-.joyn)
  
  # ---- Finalize table ----
  
  dt <- dt |>
    ftransform(survey_pop = fifelse(is.na(survey_pop),
                                    reporting_pop, survey_pop))|>
    ftransform(reporting_pop = survey_pop)
  
  setorderv(dt, c("country_code", "surveyid_year", "survey_acronym"))
  
  setcolorder(dt, c("survey_id", "cache_id" , "country_code", 
                    "surveyid_year", "survey_acronym", "survey_year", 
                    "welfare_type", "survey_mean_lcu", "survey_pop",
                    "reporting_pop"))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
  
}

#' Create deflated survey mean table
#' 
#' Deflate survey means, add comparable spell, line_up and aggregation logicals 
#' aggregate means for aggregate surveys and fix issues with IDN
#'
#' @param lcu_table data.table: output from `db_create_lcu_table_sac`
#'
#' @return data.table
#' @export
db_create_dsm_table_sac <- function(lcu_table) {
  
  #--------- Deflate welfare mean ---------
  
  dt <- fmutate(lcu_table, survey_mean_ppp = survey_mean_lcu / ppp / cpi)
  
  #--------- Add comparable spell --------- ## 
  
  dt[, comparable_spell := ifelse(.N == 1,
                                  as.character(reporting_year),
                                  sprintf("%s - %s",
                                          data.table::first(reporting_year),
                                          data.table::last(reporting_year))),
     #by = c("country_code", "area", "survey_comparability")
     by = c("country_code", "survey_comparability")
  ] 
  
  #--------- Finalize table ---------
  
  # Add is_interpolated column
  dt$is_interpolated <- FALSE
  
  # Add is_used_for_line_up column
  
  dt <- create_line_up_check(dt)
  
  # Add is_used_for_aggregation column
  
  dt[, n_rl := .N, by = cache_id]
  
  dt$is_used_for_aggregation <- (dt$reporting_level %in% 
                                   c("urban", "rural") & 
                                   dt$n_rl == 2)
  
  dt$n_rl <- NULL
  
  # Select and order columns
  
  data_vars <- c("survey_id", "cache_id", "wb_region_code",
                 "pcn_region_code", "country_code", "survey_acronym",
                 "survey_coverage", "survey_comparability", "comparable_spell",
                 "surveyid_year", "reporting_year", "survey_year", 
                 "survey_time", "welfare_type", "survey_mean_lcu",
                 "survey_mean_ppp", "reporting_pop", "ppp",
                 "cpi", "pop_data_level", "gdp_data_level",
                 "pce_data_level", "cpi_data_level", "ppp_data_level", 
                 "reporting_level", "distribution_type",
                 "gd_type", "is_interpolated", "is_used_for_line_up",
                 "is_used_for_aggregation", "display_cp")
  
  dt <- dt |>
    fselect(data_vars)
  
  # Add aggregated mean for surveys split by Urban/Rural 
  
  if(any(dt$is_used_for_aggregation==TRUE)){
    
    # Select rows w/ non-national pop_data_level
    
    dt_sub <- dt |>
      fsubset(is_used_for_aggregation == TRUE)
    
    # Compute aggregated mean (weighted population average)
    
    dt_agg <- dt_sub |>
      fgroup_by(survey_id, cache_id) |>
      collapg(custom = list(fmean = "survey_mean_lcu",
                            fmean = "survey_mean_ppp"),
              w = reporting_pop)|>
      fmutate(ppp                     = NA,  
              cpi                     = NA,
              pop_data_level          = "national", 
              gdp_data_level          = "national",
              pce_data_level          = "national",
              cpi_data_level          = "national",
              ppp_data_level          = "national",
              reporting_level         = "national",
              is_interpolated         = FALSE,
              is_used_for_line_up     = FALSE,
              is_used_for_aggregation = FALSE)|>
      fungroup()
    
    dt_meta_vars <- dt_sub |>
      get_vars(c(names(dt_sub)[!names(dt_sub) %in% names(dt_agg)],"survey_id", "cache_id"))|>
      funique(cols = c("survey_id", "cache_id"))
    
    add_vars(dt_agg) <- dt_meta_vars|>
      fselect(-c(survey_id, cache_id))
    
    dt <- collapse::rowbind(dt_agg, dt)
  }
  
  # Sort rows
  setorderv(dt, c("survey_id", "cache_id"))
  
  # change factors to characters
  dt <- dt |>
    fcomputev(is.factor, as.character, keep = names(dt))
  
  # fix data level vars for cases like IDN 1984 (Andres code)
  dt_vars <- grep("data_level$", names(dt), value = TRUE)
  
  dt <- funique(dt, 
                cols =  c("country_code",
                          "reporting_level",
                          "welfare_type",
                          "survey_year")
  )
  
  
  dt[,
     (dt_vars) := lapply(.SD, \(.) {
       fifelse(reporting_level == "national", reporting_level, .)
     }),
     .SDcols = dt_vars
  ]
  
  return(dt)
}