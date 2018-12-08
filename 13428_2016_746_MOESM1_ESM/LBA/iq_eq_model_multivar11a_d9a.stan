//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.

//v11d9a: transpose y
//v10d9a: allow an arbitrary number of variables.
//v9a: allow for calculation of the main value in a simple non-centered parameterization
//v8: Simplify down, remove the regression component since we don't need it for the joint model.
//v5: trying to build from  example in stan reference manual page 155.

data{
  int N_SUB;
  //K (number of outcome variables)
  int<lower=1> K_VAR;// = 2; #we have two variables for each observation.
  vector[N_SUB] y[K_VAR];
  
  vector[K_VAR] y_mu_prior;
  vector[K_VAR] y_sd_prior;
  
  // vector [N_SUB] iq_score;
  // vector [N_SUB] eq_score;
  
}

transformed data{
  vector[K_VAR] zeros = rep_vector(0,K_VAR);

  // for (s in 1:N_SUB){
  //   //put our two scores to measure the covariance in a single covariance matrix.
  //   y[s] = [iq_score[s],eq_score[s]]';
  // }
}

parameters{
  vector[K_VAR] y_mu;
  cholesky_factor_corr[K_VAR] L_Omega;
  vector<lower=0>[K_VAR] L_sigma;
  
}

transformed parameters{
  matrix[K_VAR, K_VAR] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  matrix [K_VAR,K_VAR] Sigma = L_Sigma * L_Sigma';
}

model{
  vector[N_SUB] y_var[K_VAR];
  //vector[K_VAR] mu[N_SUB];
  for (k in 1:K_VAR){
    y_mu[k] ~ normal(y_mu_prior[k],y_sd_prior[k]);
  }
  
  //separate out calculation of means from calculation of variance.
  for (k in 1:K_VAR){
    y_var[k] = y[k] - y_mu[k];//(should we also divide by each value's SD, so we get *standardized* covariance? I'm not sure)
  }
  
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0,2.5); #these yield standard deviations of each individual value.
  y_var ~ multi_normal_cholesky(zeros,L_Sigma);
  
  
  
}

generated quantities{
  vector[K_VAR] SD = L_sigma;
}

