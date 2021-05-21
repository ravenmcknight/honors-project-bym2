# Goal: export the data needed for each model I'll fit

## packages -------------------------------------

packages <- c('data.table', 'ggplot2', 'lubridate')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## data -----------------------------------------

ridership <- readRDS('data/covid/aggregations/apc_daily_bg.RDS')
setDT(ridership)
ridership[, year := year(ymd(date_key))]
ridership[, wday := wday(ymd(date_key), label = TRUE)]
# get 2018 -- used 2017 for honors
ridership <- ridership[year == 2020]
# and set to match w covariates
ridership[, year := as.character(4)]
# remove weekends
ridership <- ridership[wday != "Sat" & wday != "Sun"]
# avg weekday
ridership <- ridership[, .(daily_boards = mean(daily_boards, na.rm = T),
                           daily_stops = mean(daily_stops, na.rm = T)), keyby = .(GEOID)]


## SCALED ##
cov <- readRDS('data/covariates/cleaned/all_covariates_scaled_ind.RDS')
setDT(cov)

# need year == '3' for lehd and year == '4' for acs
lehd<- c("workers", "emp_density", "w_total_jobs_here", "w_perc_jobs_white", "w_perc_jobs_men",
          "w_perc_jobs_no_college", "w_perc_jobs_less40", "w_perc_jobs_age_less30", "GEOID")
lehd2 <- c("workers", "emp_density", "w_total_jobs_here", "w_perc_jobs_white", "w_perc_jobs_men",
          "w_perc_jobs_no_college", "w_perc_jobs_less40", "w_perc_jobs_age_less30")

#### NOTE: this is a little tricky depending on the years of acs vs lodes data you have
#### this has to be modified whenever you change years
emp <- cov[year == "3", ..lehd]
notemp <- cov[year == "4", !..lehd2]

cov <- emp[notemp, on = .(GEOID)]

mod_dat <- ridership[cov, on = .(GEOID)]

saveRDS(mod_dat, 'data/covid/modeling-dat/mod_dat.RDS')

## UNSCALED ##
cov <- readRDS('data/covariates/cleaned/all_covariates_ind.RDS')
setDT(cov)

#### NOTE: this is a little tricky depending on the years of acs vs lodes data you have
#### this has to be modified whenever you change years
emp <- cov[year == "3", ..lehd]
notemp <- cov[year == "4", !..lehd2]

cov <- emp[notemp, on = .(GEOID)]

mod_dat <- ridership[cov, on = .(GEOID)]
saveRDS(mod_dat, 'data/covid/modeling-dat/mod_dat_unscaled.RDS')

