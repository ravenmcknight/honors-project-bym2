# Goal: export the data needed for each model I'll fit

library(data.table)
library(tigris)
library(ggplot2)

options(tigris_class = "sf")

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

mod_dat <- readRDS('data/modeling-dat/basic_mod_dat.RDS')
setDT(mod_dat)

## linear regression -------------

lm_dat <- mod_dat[year == 2017]
lm_dat <- lm_dat[wday != 6 & wday != 7]
lm_dat <- lm_dat[, .(daily_boards = mean(daily_boards, na.rm = T), 
                     daily_stops = mean(daily_stops, na.rm = T)), keyby = .(GEOID)]
lm_dat <- unique(lm_dat)

lm_dat[, daily_boards_per_stop := daily_boards/daily_stops]

cov <- readRDS('data/modeling-dat/ag_2017_scaled_mod.RDS')
