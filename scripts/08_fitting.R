## Goal: fit models!

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

xdat <- mod_dat[, .(perc_foreign, walkability, perc_rent, perc_english_only,
                    perc_hs, perc_bach, estimate_median_age, estimate_median_hh_income,
                    perc_only_white, w_perc_jobs_white, w_perc_jobs_men, 
                    w_perc_jobs_no_college, w_perc_jobs_less40, w_perc_jobs_age_less30, 
                    emp_density, pop_density, daily_stops, perc_transit_comm)]

###### FITTING ######
## this is as it appears in my honors ##

## regular poisson regression -------------------

y <- round(mod_dat$daily_boards)
E <- mod_dat$daily_stops

N <- nrow(mod_dat)

K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

# data for basic poisson
standat1 <- list(y = y, E = E, x = x, K = K, N = N)

# and for horseshoe models
horseshoe_dat <- list(y = y, E = E, x = x, K = K, N = N,
                      scale_icept = 1, scale_global = .002,
                      nu_global = 1, nu_local = 2,
                      slab_scale = 1, slab_df = 1)

## basic poisson --------------------------------
# generally avoid rerunning for a) reproducibility and b) time

poisson <- "stan/poisson.stan"
#poisson_fit <- stan(poisson, data = standat1, iter = 10000, verbose = T, seed = 1997)

#saveRDS(poisson_fit, 'fits/poisson.RDS')

## add horseshoe priors -------------------------

poisson_hrs <- "stan/poisson_horseshoe.stan"
#poisson_hrs_fit <- stan(poisson_hrs, data = horseshoe_dat, iter = 10000, verbose = T,
#                        control = list(adapt_delta = 0.99, max_treedepth = 15), seed = 1997)
#saveRDS(poisson_hrs_fit, 'fits/poisson_horseshoe.RDS')


## add overdispersion parameter -----------------

poisson_theta <- "stan/poisson_theta.stan"

#poisson_theta_fit <- stan(poisson_theta, data = standat1, iter = 20000, verbose = T, seed = 1997)
#saveRDS(poisson_theta_fit, "~fits/poisson_theta.RDS")

## and the bym2!! -------------------------------

bym2 <- "stan/bym2.stan"
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2017)

bg_dat <- left_join(mod_dat, bgs, on = 'GEOID')
bg_dat <- st_as_sf(bg_dat)
neighborhood <- poly2nb(bg_dat)
source("functions/nb_data_funs.R")

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

#bym_fit <- stan(bym2, data = bym_dat, iter = 10000, verbose = T, seed = 1997, 
#                control = list(max_treedepth = 12))
#saveRDS(bym_fit, "fits/bym2.RDS")

## bym with horseshoe?? -------------------------

bymhrs <- "stan/bym2_horseshoe.stan"
# would change these parameters to reflect horseshoe above if i was going to write about this model seriously
# mostly this model is not worth it
bymhrs_dat <- list(y = y, E = E, x = x, K = K, 
                   N = N2, node1 = node1, node2 = node2, N_edges = N_edges, scaling_factor = scaling_factor, 
                   scale_icept = 2, scale_global = .002, nu_global = 1, nu_local = 1, slab_scale = 1, slab_df = 24)

#bymhrs <- stan(bymhrs, data = bymhrs_dat, iter = 50, verbose = T, seed = 1997, 
#              control = list(max_treedepth = 15, adapt_delta = 0.99))
#saveRDS(bymhrs, "fits/bym2_horseshoe.RDS)