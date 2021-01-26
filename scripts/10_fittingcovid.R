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

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties)

mod_dat <- readRDS('data/covid/modeling-dat/mod_dat.RDS')
setDT(mod_dat)

vars <- c("emp_density", "walkability", "perc_no_veh", "age18to34",
          "college", "lightrail", "hospital", "airport",
          "estimate_median_hh_income", "perc_rent",
          "perc_only_white")

mod_dat <- na.omit(mod_dat, cols = c("daily_boards", "daily_stops", vars))
mod_dat <- mod_dat[daily_boards != 0]

bg_dat <- left_join(mod_dat, bgs, on = 'GEOID')
bg_dat <- st_as_sf(bg_dat) 
neighborhood <- poly2nb(bg_dat)

## REMOVE UNCONNECTED SUBGRAPHS

# first, remove bgs with 0 neighbors
setDT(bg_dat)
for(i in 1:nrow(bg_dat)){
  bg_dat[i, neighbors := list(neighborhood[[i]])]
  bg_dat[i, num_neighbors := length(unlist(neighbors))]
  
  if(neighborhood[[i]] == 0){
    bg_dat[i, num_neighbors := 0]
  }
}

bg_dat <- bg_dat[num_neighbors != 0]

# I'm sure there's a better way to do this but I just grabbed geoids from QGIS
subgraph_geoids <- c(271630712063, 271630712062, 271630712071, 271630712073, 
                     271630704052, 271630707011, 271630706011, 271630706012, 
                     271630705012, 271630706022, 271630707032, 271630705024,
                     271630706023, 271630705023, 270370608281, 270370608122, 
                     270370608113, 270370608242, 270370608062, 270370608241,
                     270530275013, 270530275011, 270530272031, 270530272011, 
                     270530272033, 270530276024, 270530276013)

bg_dat <- bg_dat[!GEOID %in% subgraph_geoids]

bg_dat <- st_as_sf(bg_dat)

saveRDS(bg_dat, "data/covid/bg_dat_continuous_surface.RDS")
neighborhood <- poly2nb(bg_dat)


source("functions/nb_data_funs.R")



nbs <- nb2graph(neighborhood)
N <- nbs$N
node1 <- nbs$node1
node2 <- nbs$node2
N_edges <- nbs$N_edges
y <- ceiling(bg_dat$daily_boards)
E <- bg_dat$daily_stops

setDT(bg_dat)
xdat <- bg_dat[, ..vars]
setDT(xdat)

xdat[, `:=` (college = as.numeric(as.character(college)), hospital = as.numeric(as.character(hospital)),
             lightrail = as.numeric(as.character(lightrail)), airport = as.numeric(as.character(airport)))]


K <- ncol(xdat)
x <- as.matrix(ncol = K, xdat)

adj_matrix <- sparseMatrix(i = node1, j = node2, x = 1, symmetric = TRUE)
Q <- Diagonal(nbs$N, rowSums(adj_matrix)) - adj_matrix
Q_pert <- Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv <- inla.qinv(Q_pert, constr = list(A = matrix(1, 1, nbs$N), e = 0))

scaling_factor <- exp(mean(log(diag(Q_inv))))

bym_dat2 <- list(y = y, E = E, x = x, K = K, N = N, 
                node1 = node1, node2 = node2, N_edges = N_edges, 
                scaling_factor = scaling_factor)

bym_fit <- stan("stan/bym2_ysim.stan", data = bym_dat2, verbose = F, iter = 4000, seed = 2020, 
                control = list(max_treedepth = 15), init_r = 1/10)
saveRDS(bym_fit, "fits/covid/covid_bymfit_nov25.RDS")


# and overdispersed poisson regression with same data for comparison
pois <- "stan/poisson_theta_ysim.stan"
pois_dat <- list(y = y, E = E, x = x, K = K, N = N)


pois_fit <- stan(pois, data = pois_dat, verbose = T, iter = 4000, seed = 2020, 
                 control = list(max_treedepth = 15, adapt_delta = 0.9), init_r  = 1/10)
saveRDS(pois_fit, "fits/covid/pois_theta.RDS")
