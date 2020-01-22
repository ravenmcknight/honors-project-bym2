data {
  // number obs
  int<lower=0> N;
  // response
  int<lower=0> y[N];       
  // "offset" (number of stops)
  vector<lower=0>[N] E;     
  // number of covariates
  int<lower=1> K;
  // covariates
  matrix[N, K] x;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  // intercept
  real beta_0;      
  // covariates
  vector[K] beta;          
}
transformed parameters{
  // latent function variables
  vector[N] f;
  f = log_E + beta_0 + x*beta;
}
model {
  /// model 
  y ~ poisson_log(f); 
  // prior on betas
  beta_0 ~ normal(0.0, 3);
  beta ~ normal(0.0, 1);
}
