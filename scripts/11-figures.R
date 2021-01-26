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
mod <- readRDS('data/modeling-dat/mod_dat_unscaled.RDS')
# multiply by 100 for labeling percents
# just maps I might actually use
mod[perc_no_veh == 0, perc_no_veh := 0.01]
mod[age18to34 == 0, age18to34 := 0.01]
mod[, `:=` (perc_rent = 100*perc_rent, w_perc_jobs_age_less30 = w_perc_jobs_age_less30*100,
            w_perc_job_white = w_perc_jobs_white*100, under40 = under40*100, rail = lightrail, 
            age18to34 = age18to34*100, perc_no_veh = perc_no_veh*100, perc_only_white = perc_only_white*100,
            perc_english_only = perc_english_only*100, genz = genz*100, millenial = millenial*100,
            genx = genx*100, boomer = boomer*100, log_veh = log(perc_no_veh), log_age = log(age18to34))]

mod[, `:=` (rail = as.factor(rail), college = as.factor(college), hospital = as.factor(hospital),
            airport = as.factor(airport))]

tohist <- mod[, .(unemprate, perc_foreign, perc_transit_comm, perc_wfh, walkability, 
                 avg_veh, perc_english_only, perc_rent, perc_hs, perc_bach, perc_no_veh,
                 estimate_median_age, estimate_median_hh_income, perc_only_white,
                 w_perc_jobs_age_less30, w_perc_jobs_white, w_perc_jobs_men, 
                 w_perc_jobs_less40, w_perc_jobs_no_college, genz, millenial,
                 genx, boomer, emp_density, pop_density, under40, age18to34, GEOID)]


## functions ----------------------------------------------
source("functions/plotting.R")

## PLOTS ##

## histograms
hist_titles <- c("Unemployment rate", "Percent foreign-born residents", "Percent of residents who commute via transit",
                 "Percent of residents who work from home", "Walkability", "Average number of vehicles per household", 
                 "Percent of residents who speak English only", "Percent of housing units occupied by renters", 
                 "Percent of residents with high school diploma \n(highest educational attainment)", 
                 "Percent of residents with bachelors degree \n(highest educational attainment)", 
                 "Percent of households with no vehicle", "Median age", "Median household income", 
                 "Percent of residents white alone", "Percent of jobs worked by employees under age 30", 
                 "Percent of jobs worked by white employees", "Percent of jobs worked by men", "Percent of jobs making less than $40,000", 
                 "Percent of jobs worked byemployees without \ncollege degrees", "Percent of population age 0-25", 
                 "Percent of population age 26-40", "Percent of population age 41-55", "Percent of population age 55+", 
                 "Employment density", "Population density", "Percent of population age 40 and under", "Percent of population age 18-34")
subtitle <- c(rep("By Census Block Group, 2018", 14), rep("By Census Block Group, 2017", 5), rep("By Census Block Group, 2018", 10))

for(i in 1:length(hist_titles)){
  makeHist(tohist, names(tohist)[i], hist_titles[i], subtitle[i])
}

## maps
tomap <- mod[, .(unemprate, perc_foreign, perc_transit_comm, perc_wfh, walkability, 
                 avg_veh, perc_english_only, perc_rent, perc_hs, perc_bach, perc_no_veh,
                 estimate_median_age, estimate_median_hh_income, perc_only_white,
                 w_perc_jobs_age_less30, w_perc_jobs_white, w_perc_jobs_men, 
                 w_perc_jobs_less40, w_perc_jobs_no_college, genz, millenial,
                 genx, boomer, log_emp_dens = log(emp_density), log_pop_density = log(pop_density), 
                 under40, age18to34, log_age, log_veh, GEOID)]
# sf
sfdat <- dplyr::left_join(bgs, tomap)

map_titles <- c("Unemployment rate", "Percent foreign-born residents", "Percent of residents who commute via transit",
                 "Percent of residents who work from home", "Walkability", "Average number of vehicles per household", 
                 "Percent of residents who speak English only", "Percent of housing units occupied by renters", 
                 "Percent of residents with high school diploma \n(highest educational attainment)", 
                 "Percent of residents with bachelors degree \n(highest educational attainment)", 
                 "Percent of households with no vehicle", "Median age", "Median household income", 
                 "Percent of residents white alone", "Percent of jobs worked by employees under age 30", 
                 "Percent of jobs worked by white employees", "Percent of jobs worked by men", "Percent of jobs making less than $40,000", 
                 "Percent of jobs worked by employees without \ncollege degrees", "Percent of population age 0-25", 
                 "Percent of population age 26-40", "Percent of population age 41-55", "Percent of population age 55+", 
                 "Employment density (logged)", "Population density (logged)", "Percent of population age 40 and under", 
                "Percent of population age 18-34", "Percent of population age 18-34 (logged)", "Percent of households with no vehicle (logged)")


legend_titles <- c("rate", "percent", "percent", "percent", "area", "average", rep("percent", 5),
                   "years", "usd", rep("percent", 10), "log \ndensity", "log \ndensity", "percent", "percent", 
                   "log \npercent", "log \npercent")

for(i in 1:length(map_titles)){
  makeMap(sfdat, names(tomap)[i], map_titles[i], legend_titles[i], subtitle[i])
}

## INDICATORS ##

sfdat2 <- dplyr::left_join(bgs, mod)

ggplot(sfdat2) +
  geom_sf(aes(fill = college), lwd = 0.1) +
  theme_light() +
  scale_fill_manual(values = c("gray99", "#33638dff"), labels = c("no", "yes")) +
  labs(title = "Block groups containing a college or university")
ggsave("img/map/college.png")

ggplot(sfdat2) +
  geom_sf(aes(fill = hospital), lwd = 0.1) +
  theme_light() +
  scale_fill_manual(values = c("gray99", "#33638dff"), labels = c("no", "yes")) +
  labs(title = "Block groups containing a hospital")
ggsave("img/map/hospital.png")

ggplot(sfdat2) +
  geom_sf(aes(fill = rail), lwd = 0.1) +
  theme_light() +
  scale_fill_manual(values = c("gray99", "#33638dff"), labels = c("no", "yes")) +
  labs(title = "Block groups containing a rail station")
ggsave("img/map/rail.png")

ggplot(sfdat2) +
  geom_sf(aes(fill = airport), lwd = 0.1) +
  theme_light() +
  scale_fill_manual(values = c("gray99", "#33638dff"), labels = c("no", "yes")) +
  labs(title = "Block groups containing MSP airport")
ggsave("img/map/airport.png")

ggplot(sfdat2 %>% dplyr::filter(daily_boards != 0)) +
  geom_sf(data = bgs, lwd = 0.1, fill = "gray99") +
  geom_sf(aes(fill = log(ceiling(daily_boards))), lwd = 0) +
  theme_light() +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "Average weekday boardings, logged", 
       subtitle = "By Census Block Group, 2018",
       fill = "log \nboardings")
ggsave("img/map/boardings.png")

tocorr <- tomap %>%
  dplyr::select(-GEOID)
tocorr$daily_boards <- mod$daily_boards
tocorr$daily_stops <- mod$daily_stops
corrplot(cor(tocorr, use = "complete.obs"), type = "upper", order="hclust", tl.col="black")
corrplot(cor(tocorr, use = "complete.obs"), method = "number", type = "upper", order="hclust", tl.col="black")

