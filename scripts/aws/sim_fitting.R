## packages

library(rstan)
library(dplyr)
library(data.table)
library(shinystan)
library(ggplot2)
library(bayesplot)
options(mc.cores = parallel::detectCores())
rstan_options(autowrite = T)

## data
mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')
mod_dat <- na.omit(mod_dat)
setDT(mod_dat)

xdat <- mod_dat[, -c('GEOID', 'daily_boards', 'daily_stops', 'sqkm', 'estimate_tot_pop', 'daily_alights', 'num_interpolated', 'num_routes', 'daily_activity')]

xdat <- xdat[, 1:4]
setDT(xdat)
E <- mod_dat$daily_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

# goal: test whether horseshoe code is working



y_sim <- c()

for(i in 1:N){
  lambda <- exp(1 + .1 * x[i, 1] + .05*x[i, 2] + 0*x[i, 3] + 0*x[i, 4])
  y_sim[i] <- rpois(n = 1, lambda =  E[i] * lambda)
}

horseshoe_dat <- list(y = y_sim, E = E, x = x, K = K, N = N,
                      scale_icept = 2, scale_global = .002,
                      nu_global = 1, nu_local = 1,
                      slab_scale = 1, slab_df = 24)
poisson_hrs <- "~/Documents/honors/honors-project/stan/final-stan/poisson_horseshoe.stan"
poisson_hrs_fit <- stan(poisson_hrs, data = horseshoe_dat, iter = 2000, verbose = T, 
                        control = list(max_treedepth = 14, adapt_delta = 0.99))
