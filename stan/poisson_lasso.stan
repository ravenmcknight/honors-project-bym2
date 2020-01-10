data {
  int<lower=0> N;                  // number of observations
  int<lower=0> y[N];              // count outcomes
  vector<lower=0>[N] E;         // offset (number of times a bus stops)
  int<lower=1> K;               // number of covariates
  matrix[N, K] x;               // covariate matrix
  
  // from jrnold
  real<lower=0> scale_alpha;
  real<lower=0> df_tau;
  real<lower=0> scale_tau;
  real<lower=0> rate_sigma;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real alpha_z;                // intercept
  vector[K] betas;          // covariates
  
  real<lower=0> tau_j; 
  vector<lower=0>[K] lambda_s2;   // lower=0 to force half-cauchy
  real<lower=0> sigma_s; 
  

}
transformed parameters{
  vector[N] mu;
  real alpha;
  vector[K] beta;
  
  alpha = scale_alpha * alpha_z;
  beta = tau_j*sqrt(lambda_s2) .* betas;
  mu = alpha + x*betas;
}
model {
  // hyperpriors
  tau_j ~ student_t(df_tau, 0, scale_tau  * sigma_s);
  lambda_s2 ~ exponential(0.5);
  
  // priors
  alpha_z ~ normal(0, 1);
  betas ~ normal(0, 1);
  sigma_s ~ exponential(rate_sigma);
  
  // likelihood
  y ~ poisson_log(log_E + alpha_z + x*betas);  
}
generated quantities {
  vector[N] eta = alpha_z + x*betas;
  vector[N] lambda = exp(eta);
}
