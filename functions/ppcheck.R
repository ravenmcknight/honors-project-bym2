library(bayesplot)
library(rstan)
library(loo)
library(data.table)
library(ggplot2)

 
post <- poisson_fit
n_obs = 1495
n_iter = 20000
y <- na.omit(p_dat_scaled)$daily_boards


## get posterior draws

  samp <- rstan::extract(post)
  samp2 <- as.array(post)
  f <- samp$f
  
  yrep <- matrix(0, nrow=n_iter, ncol=n_obs)
  
  for(i in 1:nrow(yrep)){
    yrep[i, ] <- rpois(n_obs, exp(f[i, ]))
  }
  
  setDT(as.data.frame(yrep))
  
  ppoverlay <- pp_check(y, yrep[sample(100), ], ppc_dens_overlay) + 
    theme_minimal() 
  
  ppoverlay + xlim(0, 200) + ylim(0, 0.025) + 
    labs(title = "Observed versus simulated ridership for Model 1")
  
  ## get traceplot
  
  trace <- mcmc_trace(samp2, pars = c("beta_0"))
  
  ## get parcoord
  
  np <-  nuts_params(post)
  
  pcoord <- mcmc_parcoord(
    samp2,
    regex_pars = "beta",
    transform = function(x) {(x - mean(x)) / sd(x)},
    size = 0.25,
    alpha = 0.1,
    np = np
  )
  
  ## loo
  
  loglik <- as.matrix(post, pars = "log_lik")
  loo <- loo(loglik)
  psis <- psislw(-loglik)
  pit <- rstantools::loo_pit(object = yrep, y=y, lw = as.matrix(psis$lw_smooth))
  
  unifs <- matrix(runif(length(pit) * 100), nrow = 100)
  coords <- coord_cartesian(ylim = c(0, 2), xlim = c(0.1, 0.9))
  
  ppc_dens_overlay(pit, unifs) + legend_none() + coords
  
  
  ## quadrant
  
  ## map
  
  


