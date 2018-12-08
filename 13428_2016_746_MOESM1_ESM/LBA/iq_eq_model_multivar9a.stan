//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.

//v8: Simplify down, remove the regression component since we don't need it for the joint model.

data{
  int N_SUB;
  vector [N_SUB] iq_score;
  vector [N_SUB] eq_score;
  #N=N_SUB
  
}

transformed data{
  #K (number of outcome variables)
  int<lower=1> K = 2; #we have two variables for each observation.
  vector[K] y[N_SUB];
  
  int<lower=1> N = N_SUB;
  vector[K] zeros = rep_vector(0,K);

  //translate these scores back to mean_zero
  // real iq_score_mean=mean(iq_score);
  // real eq_score_mean=mean(eq_score);
  // vector[N_SUB] iq_score_m0 = iq_score-iq_score_mean;
  // vector[N_SUB] eq_score_m0 = eq_score-mean(eq_score);
  
  for (s in 1:N_SUB){
    //put our intercepts into the X matrix
    //x[s][1]=1;
    
    //put our two scores to measure the covariance in a single covariance matrix.
    y[s] = [iq_score[s],eq_score[s]]';
  }
}

parameters{
  vector[K] y_mu;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] L_sigma;
  
}

transformed parameters{
  matrix[K, K] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  matrix [K,K] Sigma = L_Sigma * L_Sigma';
  
  
}

model{
  vector[K] y_var[N_SUB];
  vector[K] mu[N];
  
  to_vector(y_mu) ~ normal(100,50);
  
  for (s in 1:N_SUB){
    y_var[s] = y[s] - y_mu;
  }
  
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0,2.5); #these yield standard deviations of each individual value.
  y_var ~ multi_normal_cholesky(zeros,L_Sigma);
  
  
  
}

