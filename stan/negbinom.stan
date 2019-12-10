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
  real mu;                // intercept
  vector[K] betas;          // covariates
  real reciprocal_scale;
}
transformed parameters {
  vector[N] eta;
  vector[N] lambda;
  real scale;
  eta = mu + x*betas;
  scale = 1. / reciprocal_scale;
  lambda = exp(eta);
}
model {
  reciprocal_scale ~ cauchy(0., 5);
  mu ~ normal(0.0, 50.0); 
  betas ~ normal(0.0, 1.0);
  y ~ neg_binomial_2_log(log_E + lambda, scale);
}
generated quantities {
  vector[N] y_rep;
  for(n in 1:N){
    y_rep[n] = neg_binomial_2_log_rng(log_E[n] + lambda[n], scale);
  }
}
