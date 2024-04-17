conflicts_prefer(collapse::fduplicated)
conflicts_prefer(collapse::funique)
conflicts_prefer(data.table::between)

tar_load(svy_mean_ppp_table)
tar_load(dl_dist_stats)
options(joyn.verbose    = FALSE, 
        pipload.verbose = FALSE, 
        joyn.reportvar  = ".joyn") 
# 
debugonce(db_create_dist_table)
db_create_dist_table(
  dl        = dl_dist_stats,
  dsm_table = svy_mean_ppp_table,
  crr_inv   = cache_inventory)



# debugonce(gd_dist_stats)
# debugonce(get_dist_stats_by_level)
debugonce(compute_dist_stats)
mp_dl_dist_stats(dt         = cache[1],
                 mean_table = svy_mean_ppp_table,
                 pop_table  = dl_aux$pop,
                 cache_id   = cache_ids[1], 
                 ppp_year   = py)

db_create_lcu_table

tar_load(svy_mean_lcu)
debugonce(db_create_lcu_table)
db_create_lcu_table(dl = svy_mean_lcu, 
                    pop_table = dl_aux$pop,
                    pfw_table = dl_aux$pfw)




debugonce(refy_mean_inc_group)
tar_load(svy_mean_ppp_table)
df <- refy_mean_inc_group(dsm    = svy_mean_ppp_table, 
                    gls    = gls, 
                    dl_aux = dl_aux)





debugonce(refy) # this is a copy of refy_mean_inc_group from ingestion pipeline
tar_load(svy_mean_ppp_table)
dq <- refy(dsm    = svy_mean_ppp_table, 
           gls    = gls, 
           dl_aux = dl_aux, 
           pinv   = pipeline_inventory)





tar_load(dt_old_ref_mean_pred)
tar_load(dt_refy_mean_inc_group)
dg <- 
get_ref_mean_pred(old    = dt_old_ref_mean_pred, 
                  new    = dq)




df <- 
db_create_ref_year_table(
  dsm_table = svy_mean_ppp_table,
  gdp_table = dl_aux$gdp,
  pce_table = dl_aux$pce,
  pop_table = dl_aux$pop,
  ref_years = gls$PIP_REF_YEARS,
  pip_years = gls$PIP_YEARS,
  region_code = 'pcn_region_code')


setorder(df, country_code, welfare_type, reporting_level, reporting_year)

df |> 
  fsubset(country_code == "CHN") |> 
  fselect(welfare_type, reporting_level, reporting_year, survey_year) |> 
  fsubset(reporting_year == 2020)





dv <- tar_read(svy_mean_lcu_table)


dv |> 
  fsubset(country_code == "CHN") |> 
  fselect(welfare_type, reporting_level, reporting_year, survey_year, survey_mean_lcu ) |> 
  fsubset(reporting_year == 2020)





tar_load(svy_mean_ppp_table)
tar_load(dt_refy_mean_inc_group)


dt_refy_mean_inc_group <-  
refy_mean_inc_group(dsm    = svy_mean_ppp_table, 
                    gls    = gls, 
                    dl_aux = dl_aux)

dt_ref_mean_pred <-  
  get_ref_mean_pred(old    = dt_old_ref_mean_pred, 
                    new    = dt_refy_mean_inc_group)

dt_prod_ref_estimation <- 
db_create_ref_estimation_table(
  ref_year_table = dt_ref_mean_pred, 
  dist_table     = dt_dist_stats)





tar_load(svy_mean_lcu)
debugonce(db_create_lcu_table)
svy_mean_lcu_table <-
  db_create_lcu_table(dl        = svy_mean_lcu,
                      pop_table = dl_aux$pop,
                      pfw_table = dl_aux$pfw)