## goal: create lots of plots!

## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages
packages <- c('ggplot2', 'dplyr', 'sf', 'tigris', 'data.table')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## data ---------------------------------------------------

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
options(tigris_class = "sf")
bgs <- block_groups("MN", counties, 2016)

# unscaled covariates for mapping etc
cov <- readRDS("data/covariates/cleaned/all_covariates.RDS")
cov <- cov[year == "3"]
tohist <- cov[, .(unemprate, perc_foreign, perc_transit_comm, perc_wfh, walkability, 
                 avg_veh, perc_english_only, perc_rent, perc_hs, perc_bach, perc_no_veh,
                 estimate_median_age, estimate_median_hh_income, perc_only_white,
                 w_perc_jobs_age_less30, w_perc_jobs_white, w_perc_jobs_men, 
                 w_perc_jobs_less40, w_perc_jobs_no_college, genz, millenial,
                 genx, boomer, emp_density, pop_density, GEOID)]


## functions ----------------------------------------------
source("functions/plotting.R")

## PLOTS ##

## histograms
hist_titles <- c("Unemployment rate", "Percent foreign-born residents", "Percent of residents who commute via transit",
                 "Percent of residents who work from home", "Walkability", "Average number of vehicles per household", 
                 "Percent of residents who speak English only", "Percent of housing units occupied by renters", 
                 "Percent of residents with high school diploma \n(highest educational attainment)", 
                 "Percent of residents with bachelors degree \n(highest educational attainment)", 
                 "Percent of households with no vehicle", "Median age", "Median household income", 
                 "Percent of residents white alone", "Percent of jobs for employees under age 30", 
                 "Percent of jobs for white employees", "Percent of jobs for men", "Percent of jobs making less than $40,000", 
                 "Percent of jobs for employees without \ncollege degrees", "Percent of population age 0-25", 
                 "Percent of population age 26-40", "Percent of population age 41-55", "Percent of population age 55+", 
                 "Employment density", "Population density")

for(i in 1:length(hist_titles)){
  makeHist(tohist, names(tohist)[i], hist_titles[i])
}

## maps
tomap <- cov[perc_wfh < 0.3, .(unemprate, perc_foreign, perc_transit_comm, perc_wfh, walkability, 
                 avg_veh, perc_english_only, perc_rent, perc_hs, perc_bach, perc_no_veh,
                 estimate_median_age, estimate_median_hh_income, perc_only_white,
                 w_perc_jobs_age_less30, w_perc_jobs_white, w_perc_jobs_men, 
                 w_perc_jobs_less40, w_perc_jobs_no_college, genz, millenial,
                 genx, boomer, log_emp_dens = log(emp_density), log_pop_density = log(pop_density), 
                 GEOID)]
# sf
sfdat <- dplyr::left_join(bgs, tomap)

map_titles <- c("Unemployment rate", "Percent foreign-born residents", "Percent of residents who commute via transit",
                 "Percent of residents who work from home", "Walkability", "Average number of vehicles per household", 
                 "Percent of residents who speak English only", "Percent of housing units occupied by renters", 
                 "Percent of residents with high school diploma \n(highest educational attainment)", 
                 "Percent of residents with bachelors degree \n(highest educational attainment)", 
                 "Percent of households with no vehicle", "Median age", "Median household income", 
                 "Percent of residents white alone", "Percent of jobs for employees under age 30", 
                 "Percent of jobs for white employees", "Percent of jobs for men", "Percent of jobs making less than $40,000", 
                 "Percent of jobs for employees without \ncollege degrees", "Percent of population age 0-25", 
                 "Percent of population age 26-40", "Percent of population age 41-55", "Percent of population age 55+", 
                 "Employment density (logged)", "Population density (logged)")


legend_titles <- c("rate", "percent", "percent", "percent", "area", "average", rep("percent", 5),
                   "years", "usd", rep("percent", 10), "density", "density")

for(i in 1:length(map_titles)){
  makeMap(sfdat, names(tomap)[i], map_titles[i], legend_titles[i])
}

## something a little different for indicators
