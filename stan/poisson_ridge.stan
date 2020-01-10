data {
  int<lower=0> N;
  int<lower=0> y[N];              // count outcomes
  vector<lower=0>[N] E;           // exposure
  int<lower=1> K;
  matrix[N, K] x;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta_0;                // intercept
  vector[K] betas;          // covariates
  
  real<lower = 0> lambda_s;
  real sigma_s; 
}
model {
  y ~ poisson_log(log_E + beta_0 + x*betas);  
  beta_0 ~ normal(0.0, 1);
  betas ~ normal(0.0, square(sigma_s)/lambda_s);
  lambda_s ~ cauchy(0.0, 1);
}
generated quantities {
  vector[N] eta = beta_0 + x*betas;
  vector[N] lambda = exp(eta);
}
