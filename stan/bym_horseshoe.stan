data {
  int<lower=0> N;
  int<lower=0> N_edges;
  int<lower=1, upper=N> node1[N_edges];  // node1[i] adjacent to node2[i]
  int<lower=1, upper=N> node2[N_edges];  // and node1[i] < node2[i]

  int<lower=0> y[N];              // count outcomes
  vector<lower=0>[N] E;           // exposure
  int<lower=1> K;                 // num covariates
  matrix[N, K] x;                 // design matrix

  real<lower=0> scaling_factor; // scales the variance of the spatial effects
  
  //horseshoe
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
  real beta0;            // intercept

  real<lower=0> sigma;        // overall standard deviation
  real<lower=0, upper=1> rho; // proportion unstructured vs. spatially structured variance

  vector[N] theta;       // heterogeneous effects
  vector[N] phi;         // spatial effects
  
  // horseshoe
  vector[K] z;
  real<lower=0> aux1_global;
  real<lower=0> aux2_global;
  vector<lower=0>[K] aux1_local;
  vector<lower=0>[K] aux2_local;
  real<lower=0> caux;
}
transformed parameters {
  vector[N] convolved_re;

  // horseshoe
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
  
  // variance of each component should be approximately equal to 1
  convolved_re =  sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
  
  f = log_E + beta0 + x * beta + convolved_re * sigma;
}
model {
  y ~ poisson_log(f);  // co-variates

  // This is the prior for phi! (up to proportionality)
  target += -0.5 * dot_self(phi[node1] - phi[node2]);

  beta0 ~ normal(0.0, scale_icept);
  theta ~ normal(0.0, 1.0);
  sigma ~ normal(0, 1.0);
  rho ~ beta(0.5, 0.5);
  // soft sum-to-zero constraint on phi)
  sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)
  
  //horseshoe
    // half t and inverse gamma priors
  z ~ normal(0, 1);
  aux1_local ~ normal(0, 1);
  aux2_local ~ inv_gamma(0.5*nu_local, 0.5*nu_local);
  aux1_global ~ normal(0, 1);
  aux2_global ~ inv_gamma(0.5*nu_global, 0.5*nu_global);
  caux ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
  beta0 ~ normal(0, scale_icept);
}
generated quantities {
  vector[N] log_lik; 
    for(i in 1:N) log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0 + x[i, ] * beta + convolved_re * sigma);
}
