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
tomap <- cov[, .(unemprate, perc_foreign, perc_transit_comm, perc_wfh, walkability, 
                 avg_veh, perc_english_only, perc_rent, perc_hs, perc_bach, 
                 estimate_median_age, estimate_median_hh_income, perc_only_white,
                 w_perc_jobs_age_less30, w_perc_jobs_white, w_perc_jobs_men, 
                 w_perc_jobs_less40, w_perc_jobs_no_college, genz, millenial,
                 genx, boomer, emp_density, pop_density, GEOID)]

# sf
sfdat <- dplyr::left_join(bgs, tomap)


## functions ----------------------------------------------
source("functions/plotting.R")

## PLOTS ##

## histograms
hist_titles <- c("Unemployment rate", "Percent foreign-born residents", "Percent of residents who commute via transit",
                 "Percent of residents who work from home", "Walkability", "Average number of vehicles per household", 
                 "Percent of residents who speak English only", "Percent of housing units occupied by renters", 
                 "Percent of residents with high school diploma", "Percent of residents with bachelors degree", 
                 "Median age", "Median household income", "Percent of residents white alone", 
                 "Percent of jobs for employees under age 30", "Percent of jobs for white employees", 
                 "Percent of jobs for men", "Percent of jobs making less than $40,000", 
                 "Percent of jobs for employees without college degrees", "Percent of population age 0-25", 
                 "Percent of population age 26-40", "Percent of population age 41-55", "Percent of population age 55+", 
                 "Employment density", "Population density")

for(i in 1:length(hist_titles)){
  makeHist(tomap, names(tomap)[i], hist_titles[i])
}

## maps
legend_titles <- c("rate", "percent", "percent", "percent", "isochrone area", "average number", "percent", 
                   "percent", "percent", "percent", "years", "usd", rep("percent", 10), "jobs/sqmi", "people/sqmi")

for(i in 1:length(hist_titles)){
  makeMap(sfdat, names(tomap)[i], hist_titles[i], legend_titles[i])
}
