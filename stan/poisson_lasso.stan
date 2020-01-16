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
  
  // from jrnold
  
  // prior on beta_0
  real<lower=0> scale_beta_0;
  // half-student-t prior on tau
  real<lower=0> df_tau;
  real<lower=0> scale_tau;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  // intercept
  real beta_0z;  
  // covariates
  vector[K] betas;          
  
  // from jrnold
  
  // hyper-parameters of coefficients
  real<lower=0> tau_j; 
  vector<lower = 0>[K] lambda_s2;
}
transformed parameters{
  //from jrnold
  vector[N] mu;
  real beta_0;
  vector[K] beta;
  
  beta_0 = scale_beta_0 * beta_0z;
  beta = tau_j*sqrt(lambda_s2) .* betas;
  mu = log_E + beta_0 + x*beta;
}
model {
  // hyperpriors
  tau_j ~ student_t(df_tau, 0, scale_tau);
  lambda_s2 ~ exponential(0.5);
  
  // priors
  beta_0z ~ normal(0, 1);
  betas ~ normal(0, 1);
  
  // likelihood
  y ~ poisson_log(mu);  
}
