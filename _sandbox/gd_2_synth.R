library(fastverse)

# load data ---------
pipload::pip_load_all_aux()

gpfw <- pfw[use_groupdata == 1]

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
x       <- country
y       <- year

dt <- pipload::pip_load_data(x,y)

area_levels <- dt[, unique(area)]

lsyn <- vector("list", )










