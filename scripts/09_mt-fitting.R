## Goal: fit models for mt!

## packages -----------------------------------------------
packages <- c('rstan', 'dplyr', 'data.table', 'shinystan', 'ggplot2',
              'bayesplot', 'tigris', 'spdep', 'sf')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')
options(mc.cores = parallel::detectCores())
rstan_options(autowrite = T)

## data -----------------------------------------

mod_dat <- readRDS('data/modeling-dat/mod_dat.RDS')
setDT(mod_dat)

vars <- c("emp_density", "w_perc_jobs_white", "w_perc_jobs_age_less30", 
         "walkability", "avg_veh", "genz", "millenial", "genx", "boomer", 
         "pop_density", "college", "hospital", "lightrail", "airport", 
         "perc_wfh", "estimate_median_hh_income")

mod_dat <- na.omit(mod_dat, cols = c("daily_boards", "daily_stops", vars))

xdat <- mod_dat[, ..vars]
setDT(xdat)

###### FITTING ######

## overdispersed poisson regression -------------

y <- round(mod_dat$daily_boards)
E <- mod_dat$daily_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
# ah factors have to be numeric
x <- as.matrix(ncol = K, xdat[, `:=` (college = as.numeric(college), hospital = as.numeric(hospital),
                                     airport = as.numeric(airport), lightrail = as.numeric(lightrail))])

# data for basic poisson
standat1 <- list(y = y, E = E, N = N, K = K, x = x)

poisson_theta <- "stan/poisson_theta.stan"

poisson_theta_fit <- stan(poisson_theta, data = standat1, iter = 100, verbose = T, seed = 1997)
saveRDS(poisson_theta_fit, "fits/mt/poisson_theta.RDS")