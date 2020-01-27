library(bayesplot)
library(rstan)
library(loo)
library(data.table)
library(ggplot2)

## for testing

post <- readRDS("~/Documents/honors/honors-project/final-fits/poisson_fit.RDS")
n_obs = 1495
n_iter = 20000

# function will take stanfit, observed y, number observations, number iterations, pars for trace, 

ppcheck <- function(post, y, n_obs, n_iter){

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
    theme_minimal() +
    labs(title = "Poisson Actual versus fitted") 
  
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
  
  
  ## map
}

