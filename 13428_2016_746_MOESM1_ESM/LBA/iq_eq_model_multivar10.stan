//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.

//v10: measure covariance between an arbitrary number of variables rather than just two. Use some of the terminology from joint modeling 
//v9: what if the variables aren't centered?
//v8: Simplify down, remove the regression component since we don't need it for the joint model.

data{
  int N_SUB;
  int<lower=1> K_VARS;//(number of outcome variables)
  vector[2] y[N_SUB];
}
// 
// transformed data{
//   //int<lower=1> N = N_SUB;
//   
//   // for (s in 1:N_SUB){
//   //   //put our two scores to measure the covariance in a single covariance matrix.
//   //   y[s] = [iq_score[s],eq_score[s]]';
//   // }
// }

parameters{
  vector[K_VARS] beta;
  cholesky_factor_corr[K_VARS] L_Omega;
  vector<lower=0>[K_VARS] L_sigma;
  
}

transformed parameters{
  matrix[K_VARS, K_VARS] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  matrix [K_VARS,K_VARS] Sigma = L_Sigma * L_Sigma';
  
}

model{
  vector[K_VARS] mu[N_SUB];
  
  to_vector(beta) ~ normal(100,50);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0,2.5); #these yield standard deviations of each individual value.
  y ~ multi_normal_cholesky(beta,L_Sigma);
}

