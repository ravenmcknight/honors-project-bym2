## packages

library(rstan)
library(dplyr)
options(mc.cores = parallel::detectCores())

## data
mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')

# prep for stan
xdat <- mod_dat %>%
  dplyr::select(-c(GEOID, daily_boards_per_stop, sqkm, perc_transit_comm))
xdat <- na.omit(xdat)

y <- xdat$daily_boards
E <- xdat$daily_stops

N <- nrow(xdat)

K <- 27
x <- as.matrix(xdat)

standat1 <- list(y = y, E = E, x = x, K = K, N = N)

## fitting

# simplest
poisson <- "~/Documents/honors/honors-project/stan/poisson.stan"
poisson_fit <- stan(poisson, data = standat1, warmup = 1000, iter = 2000, verbose = T)

# simple lasso
poisson_lasso <- "~/Documents/honors/honors-project/stan/poisson_lasso.stan"
poisson_fit_LASSO <- stan(poisson_theta, data = standat1, warmup = 100, iter = 200, verbose = T)

# the model code
poisson_theta_lasso <- "~/Documents/honors/honors-project/stan/poisson_theta_lasso.stan"

poisson_theta_fit_LASSO <- stan(poisson_theta_lasso, data = standat1, warmup = 100, iter = 200, verbose = T)
