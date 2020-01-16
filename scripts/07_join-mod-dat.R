# Goal: export the data needed for each model I'll fit

library(data.table)
library(tigris)
library(ggplot2)
library(lubridate)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

mod_dat <- readRDS('data/modeling-dat/basic_mod_dat_ann17.RDS')
setDT(mod_dat)


cov <- readRDS('data/modeling-dat/ag_2017_scaled_mod.RDS') # using scaled but not logged
setDT(cov)

lm_dat <- merge(mod_dat, cov, on = 'GEOID', all.x = TRUE)
lm_dat[year_boards == 0, year_boards := 0.01]
lm_dat[year_stops == 0, year_stops := 0.01]

lm_dat[, log_year_boards := log(year_boards)]
lm_dat[, log_year_stops := log(year_stops)]

saveRDS(lm_dat, 'data/modeling-dat/lm_dat_scaled.RDS')

## poisson regression ------------

p_dat <- lm_dat[, -c('log_year_boards', 'log_year_stops')]


saveRDS(p_dat, 'data/modeling-dat/p_dat_scaled.RDS')
