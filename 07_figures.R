library(ggplot2)
library(sf)
library(tigris)
library(dplyr)
library(viridis)
library(extrafont)
library(colorspace)

extrafont::loadfonts()
extrafont::font_import()

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

xdat <- readRDS('data/modeling-dat/ag-2017-dat.RDS')
mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2017)

sfdat <- left_join(bgs, mod_dat, on = 'GEOID', all.x = TRUE)
sfdat2 <- left_join(bgs, xdat, on = 'GEOID')



t <- ggplot(sfdat) + 
  geom_sf(aes(fill = log(daily_boards)), color = "grey", lwd = .1) +
  theme_minimal() +
  scale_fill_viridis(na.value = "transparent") +
  labs(fill = "Boardings", title = "Average Weekday Boardings, 2017", subtitle = "Log scale, enumerated by block group")

ggsave(plot = t, "~/Documents/honors/honors-project/chapters/big_map.png", scale = 5)

y <- round(na.omit(mod_dat[,1:24])$daily_boards)
pois <- data.frame(pois = rpois(n=10000, lambda = 1))

png("~/Desktop/response.png", units="in", width=18, height=12, res=300)
ggplot(as.data.frame(y), aes(x=y)) +
  geom_histogram(fill = "#fd849f") +
  xlim(0, 200) + ylim(0, 300) +
  theme_minimal() + 
  labs(y = "count", x = "daily rides", title = "Response Variable") +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), 
        text = element_text(family = "Quattrocento Sans", size = 32)) 
dev.off()

set.seed(1061)
png("~/Desktop/pois.png", units="in", width=12, height=12, res=300)
ggplot(data = data.frame(pois = rpois(n=10000, lambda = 1)), aes(x=pois)) +
  geom_histogram(fill = "#fd849f", bins = 8) +
  theme_minimal() +
  labs(x = "X", y = "count", title = "X ~ Poisson(1)") +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), text = element_text(family = "Quattrocento Sans", size = 32)) 
dev.off()

png("~/Desktop/bgs.png", units="in", width=12, height=12, res=300)
ggplot(sfdat) +
  geom_sf(color = "#0b3344", fill = "transparent") + 
  theme_minimal() +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), text = element_text(family = "Quattrocento Sans", size = 32)) 
dev.off()  

cities <- st_read("shp_bdry_census2010counties_ctus/Census2010CTUs.shp")
msp <- cities %>%
  dplyr::filter(CTU_NAME == "Minneapolis"|CTU_NAME == "St. Paul")
png("~/Desktop/zoomrides.png", units="in", width=12, height=12, res=400)
ggplot(sfdat) +
  #geom_sf(color = "#0b3344", lwd = 0.1, fill = "transparent") +
  geom_sf(color = "#0b3344", lwd = 0.1, aes(fill = log(daily_boards))) + 
  geom_sf(data = msp, color = "#ffba58", fill = "transparent", lwd = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), 
        text = element_text(family = "Quattrocento Sans", size = 28)) +
  scale_fill_viridis(na.value = "transparent", name = "Log(Boardings)")
dev.off()


png("~/Desktop/renting.png", units="in", width=12, height=12, res=400)
ggplot(sfdat2) +
  #geom_sf(color = "#0b3344", lwd = 0.1, fill = "transparent") +
  geom_sf(color = "#0b3344", lwd = 0.1, aes(fill = perc_rent*100)) + 
 # geom_sf(data = msp, color = "#ffba58", fill = "transparent", lwd = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), 
        text = element_text(family = "Quattrocento Sans", size = 28)) +
  scale_fill_continuous_sequential(na.value = "transparent", palette = "Purples", name = "%") +
  labs(title = "Percent Renting")
dev.off()

png("~/Desktop/nonwhite.png", units="in", width=12, height=12, res=400)
ggplot(sfdat2) +
  #geom_sf(color = "#0b3344", lwd = 0.1, fill = "transparent") +
  geom_sf(color = "#0b3344", lwd = 0.1, aes(fill = 100 - (perc_only_white*100))) + 
  # geom_sf(data = msp, color = "#ffba58", fill = "transparent", lwd = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), 
        text = element_text(family = "Quattrocento Sans", size = 28)) +
  scale_fill_continuous_sequential(na.value = "transparent", name = "%", palette = "Purples") +
  labs(title = "Percent Nonwhite")
dev.off()


png("~/Desktop/pop.png", units="in", width=12, height=12, res=400)
ggplot(sfdat2) +
  #geom_sf(color = "#0b3344", lwd = 0.1, fill = "transparent") +
  geom_sf(color = "#0b3344", lwd = 0.1, aes(fill = log(pop_density))) + 
  # geom_sf(data = msp, color = "#ffba58", fill = "transparent", lwd = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(colour= "#0b3344" ), axis.title = element_text(color = "#0b3344"), 
        plot.title = element_text(color = "#0b3344", hjust = 0.5), 
        text = element_text(family = "Quattrocento Sans", size = 28), legend.title = element_text(color = "white")) +
  scale_fill_continuous_sequential(na.value = "transparent", name = "% 100", palette = "Purples") +
  labs(title = "Log(Population Density)")
dev.off()

## ppcheck

library(bayesplot)


post <- readRDS("~/Documents/honors/honors-project/final-fits/poisson_fit.RDS")
post <- rstan::extract(post)
f <- post$f   
samp <- matrix(0, nrow=4000, ncol=1495)

for(i in 1:nrow(samp)){
  samp[i, ] <- rpois(1495, exp(f[i, ]))
}

setDT(as.data.frame(samp))


bayesplot::color_scheme_set("purple")

png("~/Desktop/test.png", units="in", width=12, height=9, res=300)
pp_check(y, samp[sample(1495, 50), ], ppc_dens_overlay) +
  theme_minimal() +
  xlim(0, 200) + ylim(0, 0.025) +
  theme(text = element_text(family = "Quattrocento Sans", size = 32), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Observed vs Fitted", x = "rides", y = "density")
dev.off()

png("~/Desktop/observed.png", units="in", width=12, height=9, res=300)
ggplot(as.data.frame(y)) +
  geom_density(aes(x=y), color = "#56114d", fill = "transparent") +
  theme_minimal() +
  xlim(0, 200) + ylim(0, 0.025) +
  theme(text = element_text(family = "Quattrocento Sans", size = 32), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Observed", x = "rides", y = "density")
dev.off()

post2 <- readRDS("~/Documents/honors/honors-project/final-fits/poisson_theta.RDS")
post2 <- rstan::extract(post2)
f2 <- post2$f
samp2 <- matrix(0, nrow=4000, ncol=1495)

for(i in 1:nrow(samp2)){
  samp2[i, ] <- rpois(1495, exp(f2[i, ]))
}

setDT(as.data.frame(samp2))

png("~/Desktop/goodfit.png", units="in", width=12, height=9, res=300)
pp_check(y, samp2[sample(1495, 50), ], ppc_dens_overlay) +
  theme_minimal() +
  xlim(0, 200) + ylim(0, 0.025) +
  theme(text = element_text(family = "Quattrocento Sans", size = 32), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = '"Good" Fit', x = "rides", y = "density")
dev.off()


color_scheme_view()
custom_purp <- c("#e5cce5", "#FFFFFF",
                      "#a64ca6", "#800080",
                      "#660066", "#400040")

bayesplot::color_scheme_set(custom_purp)
png("~/Desktop/observed.png", units="in", width=12, height=9, res=300)

pp_check(y, samp2[sample(1495, 50), ], ppc_dens_overlay) +
  theme_minimal() +
  xlim(0, 200) + ylim(0, 0.025) +
  theme(text = element_text(family = "Quattrocento Sans", size = 32), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Observed', x = "rides", y = "density")
dev.off()

library(ggmap) 
library(ggspatial)
nc_map <- get_map(location = "Saint Paul, MN", zoom = 7)


nb <- bgs %>%
  dplyr::filter(GEOID %in% c(271230364004, 271230365002, 271230364002, 271230365001))

png("~/Desktop/nb.png", units="in", width=12, height=12, res=300)
ggplot(nb) +
  annotation_map_tile(zoom = 14, type = "stamenbw") +
  geom_sf(color = "#fa8495", fill = "transparent", lwd = 5) +
  theme_void()
dev.off()

library(leaflet)
leaflet(nb) %>%
  addTiles() %>%
  addPolygons()
