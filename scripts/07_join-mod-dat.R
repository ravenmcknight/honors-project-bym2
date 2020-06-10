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

ridership <- readRDS('data/aggregations/apc_daily_bg.RDS')
setDT(ridership)
ridership[, year := year(ymd(date_key))]
ridership[, wday := wday(ymd(date_key), label = TRUE)]
# get 2017
ridership <- ridership[year == 2017]
# and set to match w covariates
ridership[, year := as.character(3)]
# remove weekends
ridership <- ridership[wday != "Sat" & wday != "Sun"]
# avg weekday
ridership <- ridership[, .(daily_boards = mean(daily_boards, na.rm = T),
                           daily_stops = mean(daily_stops, na.rm = T)), keyby = .(GEOID)]


cov <- readRDS('data/covariates/cleaned/all_covariates_scaled.RDS')
setDT(cov)
cov <- cov[year == "3"]

mod_dat <- cov[ridership, on = .(GEOID)]

saveRDS(mod_dat, 'data/modeling-dat/mod_dat.RDS')
