library(fastverse)
withr::local_options(list(joyn.verbose = FALSE))
# prepare data --------
## load data ---------

py <- 2017

pipload::pip_load_all_aux(replace = TRUE)


setnames(gdm, 
         c("pop_data_level", "surveyid_year"), 
         c("data_level", "year"))

setnames(pop, 
         c("pop_data_level"), 
         c("data_level"))

setnames(cpi, 
         c("cpi_data_level", "cpi_year"), 
         c("data_level", "year"))

setnames(ppp, 
         c("ppp_data_level"), 
         c("data_level"))

ppp <- ppp[ppp_year  == py & ppp_default_by_year == TRUE, 
           .(country_code, data_level, ppp)]


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
             keep = "inner") |>
  joyn::joyn(cpi, 
             by = c("country_code", "data_level", "survey_acronym", "year"),
             match_type = "1:1", 
             reportvar = FALSE, 
             keep = "inner") |> 
  joyn::joyn( ppp, 
              by = c("country_code", "data_level"),
              match_type = "m:1", 
              reportvar = FALSE, 
              keep = "inner")


# filter data -----------

cts <- NULL
yrs <- NULL

cts <- "CHN"
yrs <- 1981


if (!is.null(cts)) {
  gpfw <- gpfw[country_code %in% cts]
}

if (!is.null(yrs)) {
  gpfw <- gpfw[year %in% yrs]
}


country <- gpfw[, country_code]
year    <- gpfw[, year]


# Process in functional programming -----------
x       <- country
y       <- year

## local cache data ------------
dt <- pipload::pip_load_data(x,y)

area_levels <- dt[, unique(area)]
lsyn        <- vector("list", length(area_levels))
names(lsyn) <- area_levels

# i <- area_levels[1]
for (i in area_levels) {
  
  ## Filter data -------------
  dt_area   <- dt[area == i]
  gpfw_area <- gpfw[data_level  == i]
  gd_type   <- sub("\\D", "", gpfw_area[, gd_type]) |> 
    as.numeric()
  
  ## clean group data ------------
  
  dt_area <- wbpip:::gd_clean_data(dt_area, 
                                   welfare     = "welfare",
                                   population  = "weight", 
                                   gd_type     = gd_type)
  
  ## Create synthetic data ------------
  ls <- wbpip:::sd_create_synth_vector(welfare     = dt_area[, welfare], 
                                       population  = dt_area[, weight], 
                                       mean  = gpfw_area[, survey_mean_lcu],
                                       pop   = gpfw_area[, pop])
  ls[, `:=`(
    welfare_lcu = welfare,
    area = i
  )][, 
     welfare_ppp := wbpip:::deflate_welfare_mean(welfare_mean = welfare, 
                                                 ppp = gpfw_area[, ppp], 
                                                 cpi = gpfw_area[, cpi])
  ]
  
  lsyn[[i]] <- ls
}

## Combine data ------------
dt <- rbindlist(lsyn)
