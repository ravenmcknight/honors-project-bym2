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

mod_dat <- readRDS('data/2019/modeling-dat/mod_dat.RDS')
setDT(mod_dat)

vars <- c("emp_density", "w_perc_jobs_white", "w_perc_jobs_age_less30",
          "walkability", "avg_veh", "under40",
          "college", "lightrail", "hospital", "airport",
          "pop_density", "estimate_median_hh_income", "perc_rent")

## bym2 -----------------------------------------
bym2 <- "stan/bym2_ysim.stan"
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2018)

vars <- c("emp_density", "walkability", "perc_no_veh", "age18to34",
           "college", "lightrail", "hospital", "airport",
           "estimate_median_hh_income", "perc_rent",
           "perc_only_white")

mod_dat <- na.omit(mod_dat, cols = c("daily_boards", "daily_stops", vars))
mod_dat <- mod_dat[daily_boards != 0]

xdat <- mod_dat[, ..vars]
setDT(xdat)

xdat[, `:=` (college = as.numeric(as.character(college)), hospital = as.numeric(as.character(hospital)),
              lightrail = as.numeric(as.character(lightrail)), airport = as.numeric(as.character(airport)))]

bg_dat <- left_join(mod_dat, bgs, on = 'GEOID')
bg_dat <- st_as_sf(bg_dat)
neighborhood <- poly2nb(bg_dat)
source("functions/nb_data_funs.R")

nbs <- nb2graph(neighborhood)
N <- nbs$N
node1 <- nbs$node1
node2 <- nbs$node2
N_edges <- nbs$N_edges
y <- ceiling(bg_dat$daily_boards)
E <- bg_dat$daily_stops
K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

adj_matrix <- sparseMatrix(i = node1, j = node2, x = 1, symmetric = TRUE)
Q <- Diagonal(nbs$N, rowSums(adj_matrix)) - adj_matrix
Q_pert <- Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv <- inla.qinv(Q_pert, constr = list(A = matrix(1, 1, nbs$N), e = 0))

scaling_factor <- exp(mean(log(diag(Q_inv))))

bym_dat <- list(y = y, E = E, x = x, K = K, N = N, 
                 node1 = node1, node2 = node2, N_edges = N_edges, 
                 scaling_factor = scaling_factor)

bym_fit <- stan(bym2, data = bym_dat2, verbose = F, iter = 4000, seed = 2020, 
                control = list(max_treedepth = 15), init_r = 1/10)
saveRDS(bym_fit, "fits/2019/bym2_disagf.RDS")


# and overdispersed poisson regression with same data for comparison
pois <- "stan/poisson_theta_ysim.stan"
pois_dat <- list(y = y, E = E, x = x2, K = K, N = N)


pois_fit <- stan(pois, data = pois_dat, verbose = T, iter = 4000, seed = 2020, 
                 control = list(max_treedepth = 15, adapt_delta = 0.9), init_r  = 1/10)
saveRDS(pois_fit, "fits/2019/pois_theta.RDS")