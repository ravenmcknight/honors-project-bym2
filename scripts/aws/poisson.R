## packages

library(rstan)
library(dplyr)
library(data.table)
options(mc.cores = parallel::detectCores())
rstan_options(autowrite = T)

## data
mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')
mod_dat <- na.omit(mod_dat)
setDT(mod_dat)

xdat <- mod_dat[, -c('GEOID', 'year_boards', 'year_stops', 'sqkm', 'estimate_tot_pop', 'year_alights', 'num_interpolated', 'num_routes', 'year_activity')]

## regular poisson regression -------------------

y <- mod_dat$year_boards
E <- mod_dat$year_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

standat1 <- list(y = y, E = E, x = x, K = K, N = N)

horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N, 
                      scale_icept = 10, scale_global = 0.2, 
                      nu_global = 1, nu_local = 1,
                      slab_scale = 2, slab_df = 4)

## basic poisson --------------------------------

poisson <- "~/Documents/honors/honors-project/stan/poisson.stan"
poisson_fit <- stan(poisson, data = standat1, warmup = 100, iter = 200, verbose = T)

saveRDS(poisson_fit, '~/Documents/honors/honors-project/final-fits/poisson_fit.RDS')

## add horseshoe priors -------------------------

poisson_hrs <- "~/Documents/honors/honors-project/stan/poisson_horseshoe.stan"
poisson_hrs_fit <- stan(poisson_hrs, data = horseshoe_dat, warmup = 100, iter = 200, verbose = T)


## add overdispersion parameter -----------------

poisson_theta <- "~/Documents/honors/honors-project/stan/poisson_theta.stan"
poisson_theta_fit <- stan(poisson_theta, data = standat1, iter = 2000, verbose = T)

 #shinystan::launch_shinystan(poisson_theta_fit)

## add horseshoe priors ------------------------0

poisson_horseshoe <- "~/Documents/honors/honors-project/stan/poisson_theta_horseshoe.stan"



poisson_horseshoe_fit <- stan(poisson_horseshoe, data = horseshoe_dat, iter = 200, verbose = T, 
                              control = list(adapt_delta = 0.99))

## try some simulated data

ysim <- rpois(1495, E * exp(0 + x * (seq(1, 19, by = 1)/10)))

horseshoe_dat_sim <- list(y = ysim, E = E, x = x, K = K, N = N, 
                          scale_icept = 10, scale_global = 1, 
                          nu_global = 1, nu_local = 1,
                          slab_scale = 2, slab_df = 4)

poisson_horseshoe_fit_sim <- stan(poisson_horseshoe, data = horseshoe_dat_sim, iter = 2000, verbose = T, 
                              control = list(adapt_delta = 0.99))

## test vehtari's code


tru_beta <- c(rep(0, 10), rep(5, 9))


ysim2 <- c()
for(i in 1:1495){
  ysim2[i] <- rpois(n = 1, abs(sum(x[i, ] * t(tru_beta))))
}

vehtari_dat <- list(y = ysim2, E = E, x = x, K = K, N = N, 
                    scale_icept = 10, scale_global = 0.2, 
                    nu_global = 1, nu_local = 1,
                    slab_scale = 2, slab_df = 2)

vehtari <- "~/Documents/honors/honors-project/stan/vehtari_poisson.stan"

vehtari_fit <- stan(vehtari, data = vehtari_dat, iter = 2000, verbose = T, 
                    control = list(adapt_delta = 0.99))
