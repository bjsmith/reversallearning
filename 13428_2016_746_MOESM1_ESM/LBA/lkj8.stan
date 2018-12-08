data{
   int<lower=1> N;
   int<lower=1> R;
   int<lower=0> x[N]; // dummy info.
}
transformed data{
  int<lower=1> TD_N = R;
  int<lower=1> LENGTH = N;
  vector[TD_N] zeros = rep_vector(0,TD_N);
}
parameters {
  real lambda;
}
model {
  x ~ poisson_log(lambda);  
}
generated quantities{
  corr_matrix[TD_N] L_Omega;
  corr_matrix[TD_N] Sigma;
  corr_matrix[TD_N] Sigma2;
  // vector<lower=0>[TD_N] L_sigma;
  // 
  // vector[TD_N] cholesky_variance[LENGTH];
  // matrix[TD_N, TD_N] L_Sigma;
  // matrix[TD_N, TD_N] Sigma;
  
  //vector[TD_N] td_var[LENGTH];
  
  //predict the variance from the remaining information.
  L_Omega = lkj_corr_cholesky_rng(TD_N, 1);
  Sigma = multiply_lower_tri_self_transpose(L_Omega);
  // for (r in 1:TD_N){
  //   for(c in 1:TD_N){
  //     if(r>=c){
  //       Sigma2[r,c] = L_Omega[r,c];
  //       Sigma2[c,r] = L_Omega[r,c];
  //     }
  // 
  //   }
  // }
  
  //Sigma = cholesky_decompose(L_Omega);
  // for (i in 1:TD_N){
  //   L_sigma[i] = fabs(cauchy_rng(0,1)); //these yield standard deviations of each individual value.
  // }
  // 
  // L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  // Sigma = L_Sigma * L_Sigma';
  
  // for (r in 1:TD_N){
  //   for (c in 1:TD_N){
  //     Sigma_general[r,c] = L_Sigma
  //   }
  // }
  
  
  ////////////\end{joint model machinery}
  // for (i in 1:LENGTH){
  //   cholesky_variance[i,] = multi_normal_cholesky_rng(zeros,L_Sigma);
  // }
  //td_var ~ multi_normal_cholesky(zeros,L_Sigma);
}
