library(ggplot2)
library(sf)
library(tigris)
library(dplyr)
library(viridis)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2017)

sfdat <- left_join(bgs, mod_dat, on = 'GEOID')

post <- readRDS("~/Documents/honors/honors-project/final-fits/poisson_fit.RDS")
post <- rstan::extract(post)
f <- post$f   



t <- ggplot(sfdat) + 
  geom_sf(aes(fill = log(daily_boards)), color = "grey", lwd = .1) +
  theme_minimal() +
  scale_fill_viridis(na.value = "transparent") +
  labs(fill = "Boardings", title = "Average Weekday Boardings, 2017", subtitle = "Log scale, enumerated by block group")

ggsave(plot = t, "~/Documents/honors/honors-project/chapters/big_map.png", scale = 5)
