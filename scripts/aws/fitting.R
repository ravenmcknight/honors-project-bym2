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

## regular poisson regression -------------------

y <- round(mod_dat$daily_boards)
E <- mod_dat$daily_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

standat1 <- list(y = y, E = E, x = x, K = K, N = N)


# original fit
# horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N, 
#                       scale_icept = 2, scale_global = 0.2, 
#                       nu_global = 1, nu_local = 1,
#                       slab_scale = 2, slab_df = 4) 

# new params
# horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N, 
#                       scale_icept = 2, scale_global = 0.2, 
#                       nu_global = 1, nu_local = 1,
#                       slab_scale = 1/2, slab_df = 1) 

# new params 2
# horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N, 
#                       scale_icept = 2, scale_global = 1, 
#                       nu_global = 1, nu_local = 1,
#                       slab_scale = 1/2, slab_df = 1) 

# new params 3
horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N,
                      scale_icept = 2, scale_global = 0.002,
                      nu_global = 1, nu_local = 1,
                      slab_scale = 1, slab_df = 24)

## basic poisson --------------------------------

poisson <- "~/Documents/honors/honors-project/stan/final-stan/poisson.stan"
poisson_fit <- stan(poisson, data = standat1, iter = 10000, verbose = T)

saveRDS(poisson_fit, '~/Documents/honors/honors-project/final-fits/poisson_fit.RDS')
#poisson_fit <- readRDS("~/Documents/honors/honors-project/final-fits/poisson_fit.RDS")

post <- rstan::extract(poisson_fit)
lat <- post$f
samp <- matrix(0, nrow=4000, ncol=1495)

for(i in 1:nrow(samp)){
  samp[i, ] <- rpois(1495, exp(lat[i, ]))
}

setDT(as.data.frame(samp))


pois <- pp_check(y, samp[1:100, ], ppc_dens_overlay) + 
  theme_minimal() +
  labs(title = "Poisson Actual versus fitted") + xlim(0, 200)

## add horseshoe priors -------------------------

poisson_hrs <- "~/Documents/honors/honors-project/stan/final-stan/poisson_horseshoe.stan"
poisson_hrs_fit <- stan(poisson_hrs, data = horseshoe_dat, iter = 10000, verbose = T,
                        control = list(adapt_delta = 0.99, max_treedepth = 15))
saveRDS(poisson_hrs_fit, '~/Documents/honors/honors-project/final-fits/poisson_hrs_fit_new_params_3.RDS')

#poisson_hrs_fit <- readRDS('~/Documents/honors/honors-project/final-fits/poisson_hrs_fit.RDS')

hrs_post <- rstan::extract(poisson_hrs_fit)
hrs_lat <- hrs_post$f
hrs_samp <- matrix(0, nrow=20000, ncol=1495)

for(i in 1:nrow(hrs_samp)){
  hrs_samp[i, ] <- rpois(1495, exp(hrs_lat[i, ]))
}

setDT(as.data.frame(hrs_samp))


hrs <- pp_check(y, hrs_samp[1:100, ], ppc_dens_overlay) + 
  theme_minimal() +
  labs(title = "Poisson Horseshoe Actual versus fitted") + xlim(0, 200)

# More or less same performance (expected)
gridExtra::grid.arrange(pois, hrs, ncol=2)

## add overdispersion parameter -----------------

poisson_theta <- "~/Documents/honors/honors-project/stan/final-stan/poisson_theta.stan"

xdat2 <- xdat[, c(3, 8, 10, 14) := NULL]
x2 <- as.matrix(ncol = K, xdat2)
K2 <- ncol(xdat2)

standat2 <- list(y = y, E = E, x = x2, K = K2, N = N)


poisson_theta_fit <- stan(poisson_theta, data = standat2, iter = 10000, verbose = T)
saveRDS(poisson_theta_fit, "~/Documents/honors/honors-project/final-fits/poisson_theta.RDS")


theta_post <- rstan::extract(poisson_theta_fit)
theta_lat <- theta_post$f
theta_samp <- matrix(0, nrow=20000, ncol=1495)

for(i in 1:nrow(theta_samp)){
  theta_samp[i, ] <- rpois(1495, exp(theta_lat[i, ]))
}

setDT(as.data.frame(theta_samp))


theta <- pp_check(y, theta_samp[sample(100), ], ppc_dens_overlay) + 
  theme_minimal() +
  labs(title = "Poisson Theta Actual versus fitted") + xlim(0, 200)
