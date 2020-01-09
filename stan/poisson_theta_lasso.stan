data {
  int<lower=0> N;
  int<lower=0> y[N];              // count outcomes
  vector<lower=0>[N] E;           // offset
  int<lower=1> K;
  matrix[N, K] x;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta_0;                // intercept
  vector[K] betas;          // covariates
  
  vector[N] theta;  
  real<lower=0> sigma_y; 
  real sigma_s; 
  real tau_j;
  real<lower=0> lambda_s;
}
model {
  y ~ poisson_log(log_E + beta_0 + x*betas + theta * sigma_y);  
  beta_0 ~ normal(100, 10);
  betas ~ normal(0, square(sigma_s) * square(tau_j));
  tau_j ~ exponential(square(lambda_s)/2);
  lambda_s ~ cauchy(0.0, 1);
  theta ~ normal(0.0, 1);
  sigma_y ~ normal(0.0, 1);
}
generated quantities {
  vector[N] eta = beta_0 + x*betas + theta * sigma_y;
  vector[N] lambda = exp(eta);
}
