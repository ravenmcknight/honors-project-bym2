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
}
model {
  beta_0 ~ normal(0.0, 1);
  betas ~ normal(0.0, 1);
  y ~ poisson_log_glm(X, beta_0, betas);  
}
generated quantities {
  vector[N] eta = beta_0 + x*betas;
  vector[N] lambda = exp(eta);
}
