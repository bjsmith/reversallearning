data{
  int covar_size;
  matrix[covar_size,covar_size] covar;
}
transformed data{
  vector[covar_size] zeros = rep_vector(0,covar_size);
}

parameters {
  real<lower=0> sigma_diag_gamma;
  real<lower=0> cholesky_val;
  cholesky_factor_corr[covar_size] L_Omega;
  vector<lower=0>[covar_size] L_sigma;
  
  //would normally be data, 
  //but since I'm sampling from the prior I've made this a parameter instead.
  
}

transformed parameters {
  matrix[covar_size, covar_size] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix[covar_size, covar_size] Sigma = L_Sigma * L_Sigma';
}
model{
  
  sigma_diag_gamma ~ normal(0,3);
  cholesky_val ~ normal(0,3);
  L_Omega ~ lkj_corr_cholesky(cholesky_val); //parameter was 4 in manual but using 1 because our variables are individually normally distributed
  L_sigma ~ cauchy(0,sigma_diag_gamma); //gamma was 2 in manual but using 1 because our variables are normally distributed
  covar ~ multi_normal_cholesky(zeros,L_Sigma);
  
}

