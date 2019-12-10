# Goal: export the data needed for each model I'll fit

library(data.table)
library(tigris)
library(ggplot2)
library(lubridate)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

mod_dat <- readRDS('data/modeling-dat/basic_mod_dat.RDS')
setDT(mod_dat)
mod_dat[, year := year(ymd(date_key))]

## linear regression -------------

lm_dat <- mod_dat[year == 2017]
lm_dat <- lm_dat[wday != 6 & wday != 7]
lm_dat <- lm_dat[, .(daily_boards = mean(daily_boards, na.rm = T), 
                     daily_stops = mean(daily_stops, na.rm = T)), keyby = .(GEOID)]
lm_dat <- unique(lm_dat)

cov <- readRDS('data/modeling-dat/ag_2017_scaled_mod.RDS') # using scaled but not logged
setDT(cov)

lm_dat <- merge(lm_dat, cov, on = 'GEOID', all.x = TRUE)
lm_dat[daily_boards == 0, daily_boards := 0.01]
lm_dat[daily_stops == 0, daily_stops := 0.01]

lm_dat[, log_daily_boards := log(daily_boards)]
lm_dat[, log_daily_stops := log(daily_stops)]

saveRDS(lm_dat, 'data/modeling-dat/lm_dat_scaled.RDS')

## poisson regression ------------

p_dat <- lm_dat[, -c('log_daily_boards', 'log_daily_stops')]
p_dat[, daily_boards := as.integer(daily_boards)]
p_dat[, daily_stops := as.integer(daily_stops)]

saveRDS(p_dat, 'data/modeling-dat/p_dat_scaled.RDS')

# unscaled for easier prediction?
