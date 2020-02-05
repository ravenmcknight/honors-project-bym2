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
  // prior sd for intercept
  real<lower=0> scale_icept;
  // scale for half t for tau
  real<lower=0> scale_global;
  // df for tau, lambda
  real<lower=1> nu_global;
  real<lower=1> nu_local;
  // slab scale, df for reg horseshoe
  real<lower=0> slab_scale;
  real<lower=0> slab_df;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  // intercept
  real beta_0;
  // parameterization from vehtari
  vector[K] z;
  real<lower=0> aux1_global;
  real<lower=0> aux2_global;
  vector<lower=0>[K] aux1_local;
  vector<lower=0>[K] aux2_local;
  real<lower=0> caux;
}
transformed parameters {
  // global shrinkage
  real<lower=0> tau;
  // local shrinkage
  vector<lower=0>[K] lambda;
  vector<lower=0>[K] lambda_tilde; 
  // slab scale
  real<lower=0> c;
  // regression coefficients
  vector[K] beta;
  // latent function variables
  vector[N] f;
  
  lambda = aux1_local .* sqrt(aux2_local);
  tau = aux1_global * sqrt(aux2_global) * scale_global;
  c = slab_scale * sqrt(caux); 
  lambda_tilde = sqrt(c^2 * square(lambda) ./ (c^2 + tau^2 * square(lambda)));
  beta = z .* lambda_tilde*tau;
  f = log_E + beta_0 + x*beta;
}
model {
  // half t and inverse gamma priors
  z ~ normal(0, 1);
  aux1_local ~ normal(0, 1);
  aux2_local ~ inv_gamma(0.5*nu_local, 0.5*nu_local);
  aux1_global ~ normal(0, 1);
  aux2_global ~ inv_gamma(0.5*nu_global, 0.5*nu_global);
  caux ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
  beta_0 ~ normal(0, scale_icept);
  // and the model 
  y ~ poisson_log(f);
}
generated quantities {
  vector[N] log_lik; 
    for(i in 1:N) log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta_0 + x[i,]*beta);
}

