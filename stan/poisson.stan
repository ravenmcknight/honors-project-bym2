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
  vector[K] betas;          
}
model {
  /// model 
  y ~ poisson_log(log_E + beta_0 + x*betas); 
  // prior on betas
  beta_0 ~ normal(0.0, 1);
  betas ~ normal(0.0, 1);
}
generated quantities {
  vector[N] eta = beta_0 + x*betas;
  vector[N] lambda = exp(eta);
}
