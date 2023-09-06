# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       TEsting results
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2022-06-03
# Modification Date: 
# Script version:    01
# References:
# 
# 
# Output:             tables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(ggplot2)
library(collapse)
remotes::install_github("PIP-technical-team/pipapi@DEV")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


data_pipeline <-  "//w1wbgencifs01/pip/pip_ingestion_pipeline/pc_data/output-tfs-sync/ITSES-POVERTYSCORE-DATA"
lkups <- pipapi::create_versioned_lkups(data_pipeline, 
                                        vintage_pattern = "20230328_2017.*PROD|20230626_2017")
ctr <- "all"

# Compare two different version -----------

## survey data -----------

v1 <- "20230328_2017_01_02_PROD"
v2 <- "20230626_2017_01_02_TEST"

pip1   <- pipapi::pip(country = ctr, 
                      lkup = lkups$versions_paths[[v1]]
                       )

pip2   <- pipapi::pip(country = ctr, 
                      lkup = lkups$versions_paths[[v2]])

waldo::compare(pip1, pip2)


## lineup data ----------
pip1   <- pipapi::pip (country = ctr, 
                       fill_gaps = TRUE,
                       lkup = lkups$versions_paths[[v1]])
pip2   <- pipapi::pip (country = ctr, 
                       fill_gaps = TRUE,
                       lkup = lkups$versions_paths[[v2]])


waldo::compare(pip1, pip2)


### specific countries ------
waldo::compare(pip1[country_code == "SYR"], 
               pip2[country_code == "SYR"])


waldo::compare(pip1[country_code != "SYR"], 
               pip2[country_code != "SYR"])

cct <- "IND"

waldo::compare(pip1[country_code %in% cct], 
               pip2[country_code %in% cct])


waldo::compare(pip1[!country_code %in% cct], 
               pip2[!country_code %in% cct])



waldo::compare(pip1[country_code %in% cct], 
               pip2[country_code %in% cct])


waldo::compare(pip1[!country_code %in% cct], 
               pip2[!country_code %in% cct])

# Aggregate data ------------

agg2   <- pipapi::pip_grp_logic(povline = 2.15, 
                                lkup = lkups$versions_paths[[v2]], 
                                group_by         =  c("wb"))

## max year per region

agg2[, .SD[which.max(reporting_year)], 
     by = region_code
     ][, 
       .(region_code, reporting_year)
       ]

## values for one region ---------
agg2[reporting_year >= 2015 & region_code == "SAS"
][, 
  .(reporting_year, headcount)
]

# testing single release ----------
ctr <- "CHN"

v1 <- "20230626_2017_01_02_TEST"
df   <- pipapi::pip (country = ctr, 
                     fill_gaps = TRUE,
                     lkup = lkups$versions_paths[[v1]])



chn20 <- 
  purrr::map(.x = seq(from = 1, to = 4, by = .1), 
             .f = ~{
               pipapi::pip (country = "CHN",
                            povline = .x,
                            fill_gaps = FALSE,
                            lkup = lkups$versions_paths[[v1]])
             }) |> 
  rbindlist(use.names = TRUE)


## chart -----
ggplot(chn20[reporting_year == c(2015, 2017, 2019, 2020) 
             & poverty_line > 1.2 & poverty_line < 3.8], 
       aes(x = poverty_line, 
           y = headcount, 
           color = reporting_level)
       ) +
  geom_line() +
  facet_wrap(vars(reporting_year), 
             nrow = 2, 
             scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  )

