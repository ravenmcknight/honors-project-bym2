## data and packages ------------------

library(data.table)
library(tidyverse)
library(sf)
library(spdep)
library(tigris)

options(tigris_class = "sf")
# get morans I


mod_dat <- readRDS('data/covid/modeling-dat/mod_dat.RDS')
setDT(mod_dat)

vars <- c("emp_density", "walkability", "perc_no_veh", "age18to34",
          "college", "lightrail", "hospital", "airport",
          "estimate_median_hh_income", "perc_rent",
          "perc_only_white")
mod_dat <- na.omit(mod_dat, cols = c("daily_boards", "daily_stops", vars))
mod_dat <- mod_dat[daily_boards != 0]
xdat <- mod_dat[, ..vars]

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties)
sfdat <- left_join(bgs, mod_dat)


fit <- readRDS("fits/covid/covid_bymfit_nov25.RDS")
fit2 <- readRDS("fits/covid/pois_theta.RDS")


## residuals --------------------------

# get predictive distributions
yrep <- as.matrix(fit, pars = "y_sim")
yrep2 <- as.matrix(fit2, pars = "y_sim")

spatial_values <- data.table(predy = colMeans(yrep), 
                             lower = apply(yrep, 2, quantile, 0.025), 
                             upper = apply(yrep, 2, quantile, 0.975),
                             truey = mod_dat$daily_boards, 
                             GEOID = mod_dat$GEOID)
spatial_values[, resid := predy - truey]

nonspatial_values <- data.table(predy = colMeans(yrep2),
                                lower = apply(yrep2, 2, quantile, 0.025),
                                upper = apply(yrep, 2, quantile, 0.975),
                                truey = mod_dat$daily_boards,
                                GEOID = mod_dat$GEOID)
nonspatial_values[, resid := predy - truey]

## true y in 95% CI

nrow(spatial_values[truey < upper & truey > lower])/nrow(spatial_values)
nrow(nonspatial_values[truey < upper & truey > lower])/nrow(spatial_values)

# wow both are 100% coverage intervals
# that's what you get when you model errors I guess


## morans i ---------------------------
# original data
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

sfdat2 <- sfdat %>% filter(st_geometry_type(.) != "GEOMETRYCOLLECTION") %>%
  st_cast("MULTIPOLYGON")  %>%
  mutate(daily_boards = replace_na(daily_boards, 0)) %>%
  mutate(num_nb = lengths(st_queen(.)))

sfdat2 <- sfdat2 %>%
  filter(num_nb > 0)

# create  Queens contiguity matrix
spatmatrix <- poly2nb(sfdat2)
# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix)

moran.mc(x = sfdat2$daily_boards, listw, alternative = "greater", nsim = 9999)
# I = 0.14 p = 0.001, strong evidence of weak clustering
# if include, note that morans I is not ideal measure of spatial clustering for routes which are linear

# morans I for nonspatial mod resids
sfdat_nsr <- left_join(bgs, nonspatial_values)

sfdat_nsr2 <- sfdat_nsr %>% filter(st_geometry_type(.) != "GEOMETRYCOLLECTION") %>%
  st_cast("MULTIPOLYGON")  %>%
  mutate(resid = replace_na(resid, 0)) %>%
  mutate(num_nb = lengths(st_queen(.)))

sfdat_nsr2 <- sfdat_nsr2 %>%
  filter(num_nb > 0)

# create  Queens contiguity matrix
spatmatrix_nsr <- poly2nb(sfdat_nsr2)
# create a neighbours list with spatial weights
listw_nsr <- nb2listw(spatmatrix_nsr)

moran.mc(x = sfdat_nsr2$resid, listw_nsr, alternative = "greater", nsim = 9999)
# I = 0.11 p = 0.001, strong evidence of weak clustering


# morans I for spatial mod resids
sfdat_sr <- left_join(bgs, spatial_values)

sfdat_sr2 <- sfdat_sr %>% filter(st_geometry_type(.) != "GEOMETRYCOLLECTION") %>%
  st_cast("MULTIPOLYGON")  %>%
  mutate(resid = replace_na(resid, 0)) %>%
  mutate(num_nb = lengths(st_queen(.)))

sfdat_sr2 <- sfdat_sr2 %>%
  filter(num_nb > 0)

# create  Queens contiguity matrix
spatmatrix_sr <- poly2nb(sfdat_sr2)
# create a neighbours list with spatial weights
listw_sr <- nb2listw(spatmatrix_sr)

moran.mc(x = sfdat_sr2$resid, listw_sr, alternative = "greater", nsim = 9999)
# CSR! I = -0.06, p = 0.7


## residual maps

ggplot(sfdat_nsr2) +
  geom_sf(aes(fill = resid), lwd = 0) +
  scale_fill_distiller(type = "div", palette = "PiYG", limits = c(-6, 6)) +
  theme_light() +
  labs(title = "Residuals, non-spatial model") +
  theme(legend.position = "non")
ggsave("img/map/ns_resid.png")

left <- ggplot(sfdat_nsr2) +
  geom_sf(aes(fill = resid), lwd = 0.05) +
  scale_fill_distiller(type = "div", palette = "PiYG", limits = c(-6, 6)) +
  theme_light() +
  labs(title = "Non-spatial model residuals",
       subtitle = "Downtown Minneapolis")  + theme(legend.position = "none") +
  ylim(44.95, 45) +
  xlim(93.3, 93.2)
ggsave("img/map/ns_dt_resid.png")

right <- ggplot(sfdat_sr2) +
  geom_sf(aes(fill = resid), lwd = 0.05) +
  scale_fill_distiller(type = "div", palette = "PiYG", limits = c(-6, 6)) +
  theme_light() +
  labs(title = "Spatial model residuals",
       subtitle = "Downtown Minneapolis",
       fill = "residuals") +
  ylim(44.95, 45) +
  xlim(93.3, 93.2)
ggsave("img/map/s_dt_resid.png")


library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(right)
right <- right + theme(legend.position = "none")

comp <- grid.arrange(left, right, legend, nrow = 1, widths = c(2.3, 2.3, 0.8))
ggsave(plot = comp, "img/map/residual_comp.png")
