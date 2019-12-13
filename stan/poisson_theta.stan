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
  real<lower=0> mu;                // intercept
  vector[K] betas;          // covariates
  
  vector[N] theta;  
  real<lower=0> sigma; 
  real<lower=0> lambda_beta;
  real<lower=0> tau;
}
model {
  y ~ poisson_log(log_E + mu + x*betas + theta * sigma);  
  mu ~ normal(0.0, 15); // should be able to give this something more informative
  betas ~ normal(0.0, lambda_beta^2*tau^2);
  lambda_beta ~ cauchy(0, 1);
  tau ~ cauchy(0, 1);
  theta ~ normal(0.0, 1);
  sigma ~ normal(0.0, 1);
}
generated quantities {
  vector[N] eta = mu + x*betas + theta * sigma;
  vector[N] lambda = exp(eta);
}
