//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.

//v9: what if the variables aren't centered?
//v8: Simplify down, remove the regression component since we don't need it for the joint model.

data{
  int N_SUB;
  vector [N_SUB] iq_score;
  vector [N_SUB] eq_score;
  
}

transformed data{
  vector[2] y[N_SUB];
  #K (number of outcome variables)
  int<lower=1> K = 2; #we have two variables for each observation.
  int<lower=1> N = N_SUB;
  
  //translate these scores back to mean_zero
  //real iq_score_mean=mean(iq_score);
  //real eq_score_mean=mean(eq_score);
  //vector[N_SUB] iq_score_m0 = iq_score-iq_score_mean;
  //vector[N_SUB] eq_score_m0 = eq_score-mean(eq_score);
  
  for (s in 1:N_SUB){
    //put our two scores to measure the covariance in a single covariance matrix.
    y[s] = [iq_score[s],eq_score[s]]';
  }
}

parameters{
  vector[K] beta;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] L_sigma;
  
}

transformed parameters{
  matrix[K, K] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  matrix [K,K] Sigma = L_Sigma * L_Sigma';
  
}

model{
  vector[K] mu[N];
  
  to_vector(beta) ~ normal(100,50);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0,2.5); #these yield standard deviations of each individual value.
  y ~ multi_normal_cholesky(beta,L_Sigma);
}

