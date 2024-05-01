library(fastverse)
withr::local_options(list(joyn.verbose = FALSE))
# prepare data --------
## load data ---------

py <- 2017

pipload::pip_load_all_aux(replace = TRUE)
gls <- pipfun::pip_create_globals()


setnames(gdm, 
         c("pop_data_level", "surveyid_year"), 
         c("data_level", "year"))

setnames(pop, 
         c("pop_data_level"), 
         c("data_level"))

setnames(cpi, 
         c("cpi_year"), 
         c("year"))

# setnames(ppp, 
#          c("ppp_data_level"), 
#          c("data_level"))

ppp <- ppp[ppp_year  == py & ppp_default_by_year == TRUE, 
           .(country_code, ppp_data_level, ppp)]


## find reporting level ---------
dcols <- c(
  "cpi_domain",
  "ppp_domain",
  "gdp_domain",
  "pce_domain",
  "pop_domain"
)

pfw[,
     # Find MAX domain per obs
     reporting_level := apply(.SD, MARGIN = 1, max),
     .SDcols = dcols
][, 
  # welfare type 
  wt := fcase(welfare_type == "consumption", "CON", 
              welfare_type == "income", "INC",
              default =  "")
]




## merge data ---------

gpfw <- joyn::joyn(pfw[use_groupdata == 1], gdm, 
                   by = c("country_code", "year"),
                   match_type = "1:m", 
                   reportvar = FALSE, 
                   keep = "inner") |> 
  joyn::joyn(pop,
             by = c("country_code", "data_level", "year"),
             match_type = "1:1", 
             reportvar = FALSE, 
             keep = "inner") 

gpfw[, 
     cpi_data_level := fifelse(cpi_domain == 1, "national", data_level)
     ][, 
       ppp_data_level := fifelse(ppp_domain == 1, "national", data_level)]

gpfw <- joyn::joyn(gpfw, cpi, 
             by = c("country_code", "cpi_data_level", "survey_acronym", "year"),
             match_type = "m:1", 
             reportvar = FALSE, 
             keep = "inner") |> 
  joyn::joyn( ppp, 
              by = c("country_code", "ppp_data_level"),
              match_type = "m:1", 
              reportvar = FALSE, 
              keep = "inner")


# filter data -----------


cts <- NULL
cts <- "IDN"

yrs <- 1981
yrs <- NULL

if (!is.null(cts)) {
  gpfw <- gpfw[country_code %in% cts]
}

if (!is.null(yrs)) {
  gpfw <- gpfw[year %in% yrs]
}


# Process in functional programming -----------

# define length of inventory
inv <- vector("list", nrow(gpfw))
# j <- 1
ldt <- purrr::map(cli::cli_progress_along(1:nrow(gpfw)), \(j) {
  gpfw_j <- gpfw[j]
  
  ## local cache data ------------
  dt <- pipload::pip_load_data(gpfw_j$country,
                               gpfw_j$year, 
                               verbose = FALSE)
  
  inv[[j]] <- pipload::pip_find_data(gpfw_j$country,
                               gpfw_j$year)
  
  
  area_levels <- dt[, unique(area)]
  lal         <- length(area_levels)
  if (lal == 1) {
    if (area_levels == "" || is.na(area_levels) || is.null(area_levels)) {
      area_levels <- "national"
      dt[, area := "national"]
    }
  }
  lsyn        <- vector("list", length = lal)
  names(lsyn) <- area_levels
  
  
  # i <- area_levels[1]
  for (i in area_levels) {
    
    ## Filter data -------------
    dt_area   <- dt[area == i]
    gd_type   <- sub("\\D", "", gpfw_j$gd_type) |> # remove eveything not numeric
      as.numeric() 
    
    ## clean group data ------------
    
    dt_area <- wbpip:::gd_clean_data(dt_area, 
                                     welfare     = "welfare",
                                     population  = "weight", 
                                     gd_type     = gd_type, 
                                     quiet       = TRUE)
    
    ## Create synthetic data ------------
    # empty data.table when error or warning
    emp <- data.table(country_code = gpfw_j$country,
                      year = gpfw_j$year, 
                      are = i) |> 
      _[, c("welfare", "welfare_lcu", "welfare_ppp") := NA]
    
    # computation
    ls <- tryCatch(
      expr = {
        # Your code...
        wbpip:::sd_create_synth_vector(welfare     = dt_area$welfare, 
                                       population  = dt_area$weight, 
                                       mean        = gpfw_j$survey_mean_lcu,
                                       pop         = gpfw_j$pop) |> 
          _[, `:=`(
          welfare_lcu       = welfare,
          area              = i,
          country_code      = gpfw_j$country,
          year              = gpfw_j$year, 
          distribution_type = "micro", 
          welfare_type      = gpfw_j$welfare_type
        )][, 
           welfare_ppp := wbpip:::deflate_welfare_mean(welfare_mean = welfare, 
                                                       ppp = gpfw_j$ppp, 
                                                       cpi = gpfw_j$cpi)
        ]
      }, # end of expr section
      
      error = function(e) {
        emp
      }, # end of error section
      
      warning = function(w) {
        emp
      }
    ) # End of trycatch
    
    lsyn[[i]] <- ls
  }
  
  ## Combine data ------------
  dt <- rbindlist(lsyn)
  
  return(dt)
})

# save data ---------

## inventory file -------
synth_inv <- rbindlist(inv)
cache_id <- 
  with(gpfw, {
    paste(country_code,
          surveyid_year,
          survey_acronym,
          paste0("D", reporting_level),
          wt,
          "SYNTH",
          sep = "_"
    )
  })
  

## organize and save -----------
names(ldt) <- cache_id
  

