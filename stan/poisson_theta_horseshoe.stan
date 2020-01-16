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
  // prior for standard deviation for intercept
  real<lower=0> scale_icept;
  // scale for half-t for tau
  real<lower=0> scale_global;
  // df for half-t for tau, lambda
  real<lower=1> nu_global;
  real<lower=1> nu_local;
  // slab scale & slab df for reg horseshoe
  real<lower=0> slab_scale;
  real<lower=0> slab_df;
} 
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  // intercept
  real beta_0;      
  // overdispersion

  
  vector[K] z;
  // global shrinkage parameter
  real<lower=0> tau;
  // local shrinkage parameter
  vector<lower=0>[K] lambda;
  // for computation:
  real<lower=0> caux;
}
transformed parameters{
  // local shrinkage calculation
  vector<lower=0>[K] lambda_tilde;
  // slab scale
  real<lower=0> c;
  // coeff
  vector[K] betas;
  // "lambda" for poisson
  vector[N] f;

  c = slab_scale * sqrt(caux);
  lambda_tilde = sqrt(c^2 * square(lambda) ./ (c^2 + tau^2*square(lambda)));
  betas = z .* lambda_tilde*tau;
  f = log_E + beta_0 + x*betas;
}
model {
  // half-t and inverse-gamma priors 
  z ~ normal(0, 1);
  lambda ~ student_t(nu_local, 0, 1);
  tau ~ student_t(nu_global, 0, scale_global);
  caux ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
  beta_0 ~ normal(0, scale_icept);
  // model
  y ~ poisson_log(f);  
}
