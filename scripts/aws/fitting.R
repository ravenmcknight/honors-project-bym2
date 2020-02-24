## packages

library(rstan)
library(dplyr)
library(data.table)
library(shinystan)
library(ggplot2)
library(bayesplot)
library(tigris)
library(spdep)
library(sf)
options(mc.cores = parallel::detectCores())
options(tigris_use_cache = TRUE)
rstan_options(autowrite = T)
options(tigris_class = "sf")

## data
mod_dat <- readRDS('~/Documents/honors/honors-project/data/modeling-dat/p_dat_scaled.RDS')
mod_dat <- na.omit(mod_dat)
setDT(mod_dat)

xdat <- mod_dat[, -c('GEOID', 'daily_boards', 'daily_stops', 'sqkm', 'estimate_tot_pop', 'daily_alights', 
                     'num_interpolated', 'num_routes', 'daily_activity', 'w_total_jobs_here')]

## regular poisson regression -------------------

y <- round(mod_dat$daily_boards)
E <- mod_dat$daily_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

standat1 <- list(y = y, E = E, x = x, K = K, N = N)

horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N,
                      scale_icept = 2, scale_global = .002,
                      nu_global = 1, nu_local = 1,
                      slab_scale = 1, slab_df = 24)

## basic poisson --------------------------------

poisson <- "~/Documents/honors/honors-project/stan/final-stan/poisson.stan"
poisson_fit <- stan(poisson, data = standat1, iter = 10000, verbose = T, seed = 1997)

saveRDS(poisson_fit, '~/Documents/honors/honors-project/final-fits/poisson_fit_more.RDS')

## add horseshoe priors -------------------------

poisson_hrs <- "~/Documents/honors/honors-project/stan/final-stan/poisson_horseshoe.stan"
poisson_hrs_fit <- stan(poisson_hrs, data = horseshoe_dat, iter = 10000, verbose = T,
                        control = list(adapt_delta = 0.99, max_treedepth = 15), seed = 1997)
#saveRDS(poisson_hrs_fit, '~/Documents/honors/honors-project/final-fits/poisson_hrs_fit.RDS')

#so... it's run with no divergences before, but with a predictor i don't want to include.... i think i'll use this with a disclaimer

## add overdispersion parameter -----------------

# for computational efficiently, we'll drop the "0" covariates now
xdat[, 8]
xdat[, 1]

xdat2 <- xdat[, -c(1, 8)]
K2 <- ncol(xdat2)
x2 <- as.matrix(xdat2, ncol = K2)

standat2 <- list(y = y, E = E, x = x2, K = K2, N = N)

poisson_theta <- "~/Documents/honors/honors-project/stan/final-stan/poisson_theta.stan"


poisson_theta_fit <- stan(poisson_theta, data = standat1, iter = 20000, verbose = T, seed = 1997)
saveRDS(poisson_theta_fit, "~/Documents/honors/honors-project/final-fits/poisson_theta_all.RDS")

## and the bym ! --------------------------------

bym2 <- "~/Documents/honors/honors-project/stan/final-stan/bym2.stan"
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2017)

bg_dat <- left_join(mod_dat, bgs, on = 'GEOID')
bg_dat <- st_as_sf(bg_dat)
neighborhood <- poly2nb(bg_dat)
source("nb_data_funs.R")

nbs <- nb2graph(neighborhood)
N2 <- nbs$N
node1 <- nbs$node1
node2 <- nbs$node2
N_edges <- nbs$N_edges
y2 <- round(bg_dat$daily_boards)
E2 <- bg_dat$daily_stops

adj_matrix <- sparseMatrix(i = node1, j = node2, x = 1, symmetric = TRUE)
Q <- Diagonal(nbs$N, rowSums(adj_matrix)) - adj_matrix
Q_pert <- Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv <- inla.qinv(Q_pert, constr = list(A = matrix(1, 1, nbs$N), e = 0))

scaling_factor <- exp(mean(log(diag(Q_inv))))

bym_dat <- list(y = y2, E = E2, x = x, K = K, 
                N = N2, node1 = node1, node2 = node2, N_edges = N_edges, scaling_factor = scaling_factor)

bym_fit <- stan(bym2, data = bym_dat, iter = 10000, verbose = T, seed = 1997, 
                control = list(max_treedepth = 12))
saveRDS(bym_fit, "~/Documents/honors/honors-project/final-fits/bym.RDS")

## bym with horseshoe?? -------------------------

bymhrs <- "~/Documents/honors/honors-project/stan/bym_horseshoe.stan"
bymhrs_dat <- list(y = y, E = E, x = x, K = K, 
                   N = N2, node1 = node1, node2 = node2, N_edges = N_edges, scaling_factor = scaling_factor, 
                   scale_icept = 2, scale_global = .002, nu_global = 1, nu_local = 1, slab_scale = 1, slab_df = 24)

bymrs <- stan(bymhrs, data = bymhrs_dat, iter = 50, verbose = T, seed = 1997, 
               control = list(max_treedepth = 15, adapt_delta = 0.99))

launch_shinystan(bymrs)
