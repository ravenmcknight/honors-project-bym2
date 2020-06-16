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

poisson_theta <- "stan/poisson_theta_ysim.stan"

poisson_theta_fit <- stan(poisson_theta, data = standat1, iter = 100, verbose = T, seed = 1997, init_r = 1/10)
#saveRDS(poisson_theta_fit, "fits/mt/poisson_theta.RDS")


## bym2 -----------------------------------------
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

bym_fit <- stan(bym2, data = bym_dat, iter = 100, verbose = T, seed = 1997, 
                control = list(max_treedepth = 12))
#saveRDS(bym_fit, "fits/bym2.RDS")