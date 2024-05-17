data {
  int<lower=0> N; // Number of observatinons
  int<lower=0, upper=1> Y[N]; // Binary outcome variable 
  int<lower=0> K; // Number of predictors
  matrix[N,K] X; // Predictor matrix
}

parameters {
  real beta_0; // Intercept 
  vector[K] beta_raw; // Raw regression coefficients for non-centered parameterization
  vector<lower=0>[K] lambda;    // Local shrinkage parameters for horseshoe
  real<lower=0> tau;            // Global shrinkage parameter for horseshoe
}

transformed parameters {
  vector[K] beta; //Transformed regression coefficients
  beta = beta_raw .* (tau * lambda);
}


model {
  // Horseshoe Priors
  beta_0 ~ normal(0, 10);       // Less informative prior on the intercept
  tau ~ cauchy(0, 1);           // Global shrinkage - half-Cauchy
  lambda ~ cauchy(0, 1);        // Local shrinkage - half-Cauchy
  beta_raw ~ normal(0, 1); // Non-centered parameterization
  
  // Log-likelihood using bernoulli_logit
  for (i in 1:N) {
    Y[i] ~ bernoulli_logit(beta_0 + X[i] * beta);
  }
}

generated quantities { // Generate predicted probabilities
  int<lower=0, upper=1> y_pred[N];
  for (n in 1:N) {
    y_pred[n] = bernoulli_rng(inv_logit(beta_0 + X[n] * beta));
  }
}

