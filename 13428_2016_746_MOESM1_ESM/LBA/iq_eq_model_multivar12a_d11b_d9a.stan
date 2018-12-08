//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.

//v12a_d11b_d9b: Fixed gross error in the standardization.
//v11d9b: Additional attempt at standardizing variables. This time, we don't try to estimate the deviation, we just control for it. Also no longer estimating mean of ThetaDeltas. We just move straight to standardizing them.
//v10d9a: allow an arbitrary number of variables.
//v9a: allow for calculation of the main value in a simple non-centered parameterization
//v8: Simplify down, remove the regression component since we don't need it for the joint model.
//v5: trying to build from  example in stan reference manual page 155.

data{
  int N_SUB;
  //K (number of outcome variables)
  int<lower=1> K_VAR;// = 2; #we have two variables for each observation.
  vector[K_VAR] y[N_SUB];
  
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
  vector[K_VAR] y_var[N_SUB];
  vector[K_VAR] mu[N_SUB];
  vector[K_VAR] y_sd;
  for (k in 1:K_VAR){
    y_mu[k] ~ normal(y_mu_prior[k],y_sd_prior[k]);
    y_sd[k] = sd(y[:,k]);
  }//the estimation here is not actually used to calculate the covariance matrix.
  
  //separate out calculation of means from calculation of variance.
  for (s in 1:N_SUB){
    //y_var[s] = (y[s] - mean(y[s]))./sd(y[s]);//(should we also divide by each value's SD, so we get *standardized* covariance? I'm not sure)
    y_var[s,:] = (to_vector(y[s]) - y_mu)./y_sd; //(should we also divide by each value's SD, so we get *standardized* covariance? I'm not sure)
  }
  
  L_Omega ~ lkj_corr_cholesky(1);
  L_sigma ~ cauchy(0,1); #these yield standard deviations of each individual value.
  y_var ~ multi_normal_cholesky(zeros,L_Sigma);
  
  
  
}

generated quantities{
  vector[K_VAR] SD = L_sigma;
  #https://blogs.sas.com/content/iml/2010/12/10/converting-between-correlation-and-covariance-matrices.html
  matrix[K_VAR,K_VAR] y_corr;
  matrix[K_VAR,K_VAR] inv_mat_var;
  

  //inv_mat_var = rep_matrix(((1) ./ sqrt(diagonal(Sigma))),K_VAR);
  inv_mat_var = rep_matrix(((1) ./ sqrt(diagonal(Sigma))),K_VAR);
  y_corr = inv_mat_var' .* Sigma .* inv_mat_var;  
  // inv_mat_var = (1) ./ sqrt(diagonal(Sigma));
  // y_corr = rep_matrix(inv_mat_var,num_elements(inv_mat_var)) * Sigma * rep_matrix(inv_mat_var,num_elements(inv_mat_var))';
}

