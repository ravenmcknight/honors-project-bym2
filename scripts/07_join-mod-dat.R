# Goal: export the data needed for each model I'll fit

library(data.table)
library(ggplot2)
library(lubridate)

mod_dat <- readRDS('data/modeling-dat/basic_mod_dat.RDS')
setDT(mod_dat)
mod_dat[, year := year(ymd(date_key))]
mod_dat <- mod_dat[year == 2017]

cov <- readRDS('data/modeling-dat/ag_2017_scaled_mod.RDS') # using scaled but not logged
setDT(cov)

lm_dat <- mod_dat[wday != 6 & wday != 7]
lm_dat <- lm_dat[, .(daily_boards = mean(daily_boards, na.rm = T), 
                     daily_stops = mean(daily_stops, na.rm = T)), keyby = .(GEOID)]
lm_dat <- unique(lm_dat)

lm_dat <- merge(lm_dat, cov, by = 'GEOID', all.x = TRUE)

saveRDS(lm_dat, 'data/modeling-dat/lm_dat_scaled.RDS')

## poisson regression ------------




saveRDS(lm_dat, 'data/modeling-dat/p_dat_scaled.RDS')
