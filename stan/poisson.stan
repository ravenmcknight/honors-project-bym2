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
}
model {
  y ~ poisson_log(log_E + mu + x*betas);  
  mu ~ normal(0.0, 1);
  betas ~ normal(0.0, 1);
}
generated quantities {
  vector[N] eta = log_E + mu + x*betas;
  vector[N] lambda = exp(eta);
  vector[N] y_rep;
  for(n in 1:N)
    y_rep[n] = poisson_log_rng(log_E[n] + mu + x[n]*betas);
}
