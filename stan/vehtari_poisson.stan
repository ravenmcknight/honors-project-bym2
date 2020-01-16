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
  
  // priors for intercept
  real<lower=0> scale_icept;
  // priors for half-t for tau, lambda
  real<lower=0> scale_global;
  real<lower=1> nu_global;
  real<lower=1> nu_local;
  // priors for horseshoe
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
  vector[N] theta;  
  vector[K] z;
  // global shrinkage
  real<lower=0> tau;
  // local shrinkage
  vector<lower=0>[K] lambda;
  real<lower=0> caux;
}
transformed parameters{
  // truncated local shrinkage parameter
  vector<lower=0>[K] lambda_tilde;
  //slab scale
  real<lower=0> c;
  // coeff
  vector[K] beta;
  // latent function
  vector[N] f;
  
  c = slab_scale * sqrt(caux);
  lambda_tilde=sqrt(c^2*square(lambda)./(c^2+tau^2*square(lambda))); 
  
  beta = z .* lambda_tilde*tau;
  f = beta_0 + x*beta;
}
model {
  // model
  y ~ poisson_log(log_E + beta_0 + x*beta + theta);  
  // normal priors on everything for now
  beta_0 ~ normal(0.0, scale_icept);
  theta ~ normal(0.0, 1);
  z ~ normal(0,1);     
  lambda ~ student_t(nu_local,0,1);     
  tau ~ student_t(nu_global,0,scale_global);     
  caux ~ inv_gamma(0.5*slab_df,0.5*slab_df);    
}
