data {
  int<lower=0> N;                  // number of observations
  int<lower=0> y[N];              // count outcomes
  vector<lower=0>[N] E;         // offset (number of times a bus stops)
  int<lower=1> K;               // number of covariates
  matrix[N, K] x;               // covariate matrix
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta_0;                // intercept
  vector[K] betas;          // covariates
  
  real tau_j; 
  real<lower=0> lambda_s;   // lower=0 to force half-cauchy
}
model {
  y ~ poisson_log(log_E + mu + x*betas);  
  beta_0 ~ normal(0.0, 50);
  betas ~ normal(0.0, square(sigma_s) * square(tau_j));
  tau_j ~ exponential(square(lambda_s)/2);
  lambda_s ~ cauchy(0.0, 1);
}
generated quantities {
  vector[N] eta = mu + x*betas;
  vector[N] lambda = exp(eta);
}
