//what's the best cholesky value and sigma_diag_gamma for a quite range of possible matrices?
data{
  int covar_size;
  int N;
  //matrix[covar_size,covar_size] covar;
  vector[covar_size] modeled_data[N];
  vector[covar_size] modeled_data2[N];
}
transformed data{
  vector[covar_size] zeros = rep_vector(0,covar_size);
}

parameters {
  real<lower=0> sigma_diag_gamma;
  real<lower=0> cholesky_val;
  cholesky_factor_corr[covar_size] L_Omega;
  vector<lower=0>[covar_size] L_sigma;
  cholesky_factor_corr[covar_size] L_Omega2;
  vector<lower=0>[covar_size] L_sigma2;
  
  //would normally be data, 
  //but since I'm sampling from the prior I've made this a parameter instead.
  
}

transformed parameters {
  matrix[covar_size, covar_size] L_Sigma = diag_pre_multiply(L_sigma2, L_Omega2);
  matrix[covar_size, covar_size] Sigma = L_Sigma * L_Sigma';
  
  matrix[covar_size, covar_size] L_Sigma2 = diag_pre_multiply(L_sigma2, L_Omega2);
  matrix[covar_size, covar_size] Sigma2 = L_Sigma2 * L_Sigma2';
}
model{
  
  sigma_diag_gamma ~ normal(0,3);
  cholesky_val ~ normal(0,3);
  L_Omega ~ lkj_corr_cholesky(cholesky_val); //parameter was 4 in manual but using 1 because our variables are individually normally distributed
  L_sigma ~ cauchy(0,sigma_diag_gamma); //gamma was 2 in manual but using 1 because our variables are normally distributed
  
  L_Omega2 ~ lkj_corr_cholesky(cholesky_val); //parameter was 4 in manual but using 1 because our variables are individually normally distributed
  L_sigma2 ~ cauchy(0,sigma_diag_gamma); //gamma was 2 in manual but using 1 because our variables are normally distributed
  
  modeled_data ~ multi_normal_cholesky(zeros,L_Sigma2);
  modeled_data2 ~ multi_normal_cholesky(zeros,L_Sigma2);
  
}

