# Goal: get census variables of interest, 2015-2017
# I'm sure there are more refined ways to do this 

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
# use this to look at variables
View(v15)

# get counties
counties <- c('Anoka', 'Carver', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington')
bgs <- block_groups("MN", counties, 2016)

# 2018 acs not out yet
years <- list(2015, 2016, 2017)

## basics ---------------------------------------
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
basics <- basics[, .(year, GEOID, estimate_median_age, estimate_tot_pop, estimate_median_hh_income, perc_only_white)]

saveRDS(basics, 'data/covariates/basic_acs.RDS')

## age cohorts ----------------------------------

agecohorts <- map_dfr(
  years, 
  ~ get_acs(
    geography = "block group",
    table = "B01001",
    summary_var = "B01001_001",  # should have used this throughout!
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

agecohorts2 <- agecohorts %>% pivot_wider(names_from = 'variable', values_from = c('estimate', 'moe'))
setDT(agecohorts2)

# simplify to gen z, millenials, gen x, boomers+
  
genz <- paste0("estimate_B01001_", c('003', '004', '005', '006', '007', '008', '009', '010', 
          '027', '028', '029', '030', '031', '032', '033', '034'))

millenial <- paste0("estimate_B01001_", c('011', '012', '013', '035', '036', '037'))

genx <- paste0("estimate_B01001_", c('014', '015', '016', '038', '039', '040'))

boomer <- paste0("estimate_B01001_", c('017', '018', '019', '020', '021', '022', '023', '024', '025', 
            '041', '042', '043', '044', '045', '046', '047', '048', '049'))

agecohorts2$genz <- rowSums(agecohorts2[, ..genz])
agecohorts2$millenial <- rowSums(agecohorts2[, ..millenial])
agecohorts2$genx <- rowSums(agecohorts2[, ..genx])
agecohorts2$boomer <- rowSums(agecohorts2[, ..boomer])

agecohorts <- agecohorts2[, `:=`(genz = genz/summary_est, millenial = millenial/summary_est, 
                                 genx = genx/summary_est, boomer = boomer/summary_est)]
agecohorts <- agecohorts[, .(year, GEOID, summary_est, genz, millenial, genx, boomer)]

saveRDS(agecohorts, "data/covariates/agecohorts.RDS")


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
language <- language[, .(year, GEOID, perc_english_only)]

saveRDS(language, 'data/covariates/language.RDS')

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
education <- education[, .(year, GEOID, perc_hs, perc_bach)]

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
nativity <- nativity[, .(year, GEOID, perc_native, perc_foreign)]

saveRDS(nativity, 'data/covariates/tract/nativity.RDS')

# housing tenure & vehicle availability ---------

housing <- map_dfr(
  years,
  ~ get_acs(
    geography = "block group",
    variables = c(total_pop = "B25044_001",
                  total_own = "B25044_002",
                  total_rent = "B25044_009"),
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

housing <- housing %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(housing)
housing[, perc_owner_occ := estimate_total_own/estimate_total_pop]
housing[, perc_rent := estimate_total_rent/estimate_total_pop]
housing <- housing[, .(year, GEOID, perc_owner_occ, perc_rent)]

saveRDS(housing, 'data/covariates/housing.RDS')

# avg number of vehicles per household ----------

vehicles <- map_dfr(
  years, 
  ~ get_acs(
    geography = "block group",
    table = "B25044",
    summary_var = "B25044_001",  # should have used this throughout!
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

vehicles <- vehicles %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(vehicles)

zero <- paste0("estimate_B25044_", c('003', '010'))
one <- paste0("estimate_B25044_", c('004', '011'))
two <- paste0("estimate_B25044_", c('005', '012'))
three <- paste0("estimate_B25044_", c('006', '013'))
four <- paste0("estimate_B25044_", c('007', '014'))
five <- paste0("estimate_B25044_", c('008', '015'))

vehicles$zero <- rowSums(vehicles[, ..zero])
vehicles$one <- rowSums(vehicles[, ..one])
vehicles$two <- rowSums(vehicles[, ..two])
vehicles$three <- rowSums(vehicles[, ..three])
vehicles$four <- rowSums(vehicles[, ..four])
vehicles$five <- rowSums(vehicles[, ..five])

vehicles[, totalveh := zero*0 + one*1 + two*2 + three*3 + four*4 + five*5]
vehicles[, avg_veh := totalveh/summary_est]
vehicles <- vehicles[, .(year, GEOID, summary_est, zero, one, two, three, four, five, totalveh, avg_veh)]
saveRDS(vehicles, "data/covariates/vehicles.RDS")

# acs employment data ---------------------------

acs_emp <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    variables = c(transit_commute = "B08006_008",
                  worked_from_home = "B08006_017",
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
acs_emp[, perc_wfh := estimate_worked_from_home/estimate_total_commute]
acs_emp <- acs_emp[, .(year, GEOID, perc_transit_comm, perc_wfh)]

saveRDS(acs_emp, 'data/covariates/tract/acs-emp.RDS')

## unemployment? --------------------------------
# only available at tract level

unemp <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    table = "B21005",
    summary_var = "B21005_001",
    state = "MN",
    county = counties,
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

unemp <- unemp %>% pivot_wider(names_from = variable, values_from = c('estimate', 'moe'))
setDT(unemp)
unemployed <- paste0("estimate_B21005_", c('006', '011', '017', '022', '028', '033'))
unemp$unemployed <- rowSums(unemp[, ..unemployed])
unemp[, unemprate := unemployed/summary_est]
unemp <- unemp[, .(year, GEOID, summary_est, unemployed, unemprate)]
saveRDS(unemp, "data/covariates/tract/unemp.RDS")

## employment data ----------------------------------------

# i'll use the employment data i cleaned here:
# https://github.com/ravenmcknight/LODES-analysis/blob/master/get-ts-data.R
# add year restrictions here

tot_jobs <- readRDS('/Users/raven/Documents/projects/LODES-analysis/data/tot_jobs.RDS')
setDT(tot_jobs)
tot_jobs <- tot_jobs[year %in% years]
tot_jobs[year == 2017, year := as.character(3)]
tot_jobs[year == 2016, year := as.character(2)]
tot_jobs[year == 2015, year := as.character(1)]
setnames(tot_jobs, "w_bg", "GEOID")
saveRDS(tot_jobs, 'data/covariates/tot_jobs.RDS')

# wac <- this is about who works in this bg
wac_urls <- c('https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2017.csv.gz',
              'https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2016.csv.gz',
              'https://lehd.ces.census.gov/data/lodes/LODES7/mn/wac/mn_wac_S000_JT00_2015.csv.gz')

for(i in 1:length(years)){
  download.file(wac_urls[i], paste0('data/covariates/wac/', years[i], '.csv.gz'))
}

wac_files <- list.files(path = 'data/covariates/wac/')
l <- lapply(paste0('data/covariates/wac/', wac_files), fread)

l[[1]]$year <- 3
l[[2]]$year <- 2
l[[3]]$year <- 1

# this whole section should be cleaned up considerably

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
wac[, year := as.character(year)]

# lots of these could be interesting but just taking a few
wac <- wac[, .(GEOID, year, w_total_jobs_here, w_perc_jobs_white, w_perc_jobs_men, 
               w_perc_jobs_no_college, w_perc_jobs_less40, w_perc_jobs_age_less30)]

saveRDS(wac, 'data/covariates/all_wac.RDS')

