# Goal: get census variables of interest, 2015-2017

## packages -----------------------------------------------

packages <- c('data.table', 'tigris', 'tidycensus', 'purrr', 'tidyr', 'ggplot2')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_class = 'sf')


## acs variables ------------------------------------------

# look at acs 5yr estimates for 2015
v15 <- load_variables(2015, "acs5", cache = TRUE)
setDT(v15)
View(v15)

# start by pulling some basics for a first model
# this document will likely be updated often as I go
counties <- c('Anoka', 'Carver', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington')

# 2018 acs not out yet
years <- list(2015, 2016, 2017)

basics <- map_dfr(
  years,
  ~ get_acs(
    geography = "block group",
    variables = c(tot_pop = "B01003_001",
                  median_age = "B01002_001",
                  median_hh_income = "B19013_001",
                  white_alone = "B02001_002"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

basics <- basics %>% pivot_wider(names_from = 'variable', values_from = c('estimate', 'moe'))
setDT(basics)
basics[, perc_only_white := estimate_white_alone/estimate_tot_pop]
head(basics)

saveRDS(basics, 'data/covariates/basic_acs.RDS')

## language spoken at home ----------------------

language <- map_dfr(
  years,
  ~ get_acs(
    geography = "block group",
    variables = c(total_517 = "B16004_002",
                  speak_only_english_517 = "B16004_003",
                  total_1864 = "B16004_024",
                  speak_only_english_1864 = "B16004_025",
                  tot_65 = "B16004_046", 
                  speak_only_english_65 = "B16004_047"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

language <- language %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(language)
language[, est_tot_over5 := estimate_total_517 + estimate_total_1864 + estimate_tot_65]
language[, est_tot_eng_only := estimate_speak_only_english_517 + estimate_speak_only_english_1864 + estimate_speak_only_english_65]
language[, perc_english_only := est_tot_eng_only/est_tot_over5]

# mostly want the last column but should save everything because of MOEs
saveRDS(language, 'data/covariates/language.RDS')

# these are pretty big moes
ggplot(language, aes(x=moe_speak_only_english_1864/estimate_speak_only_english_1864)) + geom_histogram()

## educational attainment -----------------------

education <- map_dfr(
  years,
  ~ get_acs(
    geography = "block group",
    variables = c(total_pop_25 = "B15003_001", 
                  highschool = "B15003_017", 
                  highschool_equiv = "B15003_018",
                  bachelors = "B15003_022"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

education <- education %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(education)
education[, hs_or_equiv := estimate_highschool + estimate_highschool_equiv]
education[, perc_hs := hs_or_equiv/estimate_total_pop_25]
education[, perc_bach := estimate_bachelors/estimate_total_pop_25]
saveRDS(education, 'data/covariates/education.RDS')


# native/foreign born at CENSUS TRACT
nativity <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    variables = c(total_pop = "B05012_001",
                  tot_native = "B05012_002",
                  tot_foreign = "B05012_003"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

nativity <- nativity %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(nativity)
nativity[, perc_native := estimate_tot_native/estimate_total_pop]
nativity[, perc_foreign := estimate_tot_foreign/estimate_total_pop]

saveRDS(nativity, 'data/covariates/nativity.RDS')

# housing tenure & vehicle availability ---------

ten_veh <- map_dfr(
  years,
  ~ get_acs(
    geography = "block group",
    variables = c(total_pop = "B25044_001",
                  total_own = "B25044_002",
                  own_no_veh = "B25044_003",
                  total_rent = "B25044_009",
                  rent_no_veh = "B25044_010"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

ten_veh <- ten_veh %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(ten_veh)
ten_veh[, perc_owner_occ := estimate_total_own/estimate_total_pop]
ten_veh[, perc_rent := estimate_total_rent/estimate_total_pop]
ten_veh[, tot_no_veh := estimate_own_no_veh + estimate_rent_no_veh]
ten_veh[, perc_no_veh := tot_no_veh/estimate_total_pop]

saveRDS(ten_veh, 'data/covariates/housing-and-vehicles.RDS')


# acs employment data ---------------------------

acs_emp <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    variables = c(transit_commute = "B08006_008",
                  total_commute = "B08006_001"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

acs_emp <- acs_emp %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(acs_emp)
acs_emp[, perc_transit_comm := estimate_transit_commute/estimate_total_commute]

saveRDS(acs_emp, 'data/covariates/acs-emp.RDS')

## employment data ----------------------------------------

# i'll use the employment data i cleaned here:
# https://github.com/ravenmcknight/LODES-analysis/blob/master/get-ts-data.R
# add year restrictions here

od_jobs <- readRDS('/Users/raven/Documents/projects/LODES-analysis/data/od_jobs.RDS')
setDT(od_jobs)
od_jobs <- od_jobs[year %in% years]
saveRDS(od_jobs, 'data/covariates/od_jobs.RDS')

tot_jobs <- readRDS('/Users/raven/Documents/projects/LODES-analysis/data/tot_jobs.RDS')
setDT(tot_jobs)
tot_jobs <- tot_jobs[year %in% years]
saveRDS(tot_jobs, 'data/covariates/tot_jobs.RDS')

# maybe better is rac data



# and maybe even better is wac...
wac_urls <- c('https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2017.csv.gz',
              'https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2016.csv.gz',
              'https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2015.csv.gz')

for(i in 1:length(years)){
  download.file(wac_urls[i], paste0('data/covariates/wac/mn_wac', years[i], '.csv.gz'))
}

wac_files <- list.files(path = 'data/covariates/wac/')
l <- lapply(paste0('data/covariates/wac/', wac_files), fread)

l[[1]]$year <- 2017
l[[2]]$year <- 2016
l[[3]]$year <- 2015

# this whole section could be cleaned up considerably

# aggregate blocks to bgs

for(i in 1:3){
  l[[i]][, GEOID := substr(w_geocode, 1, 12)]
}

for(i in 1:3){
  l[[i]]$createdate <- NULL
  l[[i]] <- l[[i]][, lapply(.SD, sum, na.rm=TRUE), by= c('GEOID', 'year')]
}


for(i in 1:3){
  l[[i]] <- l[[i]][GEOID %in% bgs$GEOID]
  colnames(l[[i]]) <- c('GEOID', 'year', 'w_geocode', 'w_total_jobs_here', 'w_age_29', 'w_age_30_54', 'w_age_55', 
                        'w_wage_1250', 'w_wage_1250_3333', 'w_wage_3333', 'w_ag', 'w_mine', 'w_util',
                        'w_construction', 'w_manuf', 'w_wholesale', 'w_retail', 'w_transp', 'w_info',
                        'w_finance', 'w_real_estate', 'w_prof', 'w_management', 'w_admin', 'w_educ',
                        'w_health', 'w_art', 'w_accom', 'w_other', 'w_pub', 'w_white', 'w_black', 
                        'w_amind', 'w_asian', 'w_native', 'w_two_or_more', 'w_not_hisp', 'w_hisp',
                        'w_less_hs', 'w_hs', 'w_some_college', 'w_degree', 'w_male', 'w_female', 
                        'w_firm1', 'w_firm3', 'w_firm5', 'w_firm10', 'w_firm11', 'w_size19',
                        'w_firm49', 'w_firm249', 'w_firm499', 'w_firm500')
}

wac <- rbindlist(l)
setDT(wac)

wac[, w_perc_jobs_white := w_white/w_total_jobs_here]
wac[, w_perc_jobs_men := w_male/w_total_jobs_here]
wac[, w_perc_jobs_no_college := w_hs/w_total_jobs_here]
wac[, w_perc_jobs_less40 := (w_wage_1250 + w_wage_1250_3333)/w_total_jobs_here]
wac[, w_perc_jobs_age_less30 := w_age_29/w_total_jobs_here]

saveRDS(wac, 'data/covariates/wac/all-wac.RDS')

