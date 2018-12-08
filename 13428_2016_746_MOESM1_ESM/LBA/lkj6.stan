data{
   int<lower=1> N;
   int<lower=1> D_N;
   int<lower=1> UD_N;
   matrix[N,D_N] Deltas;
   matrix[N,UD_N] UnconnectedData;
   
}

transformed data{
  int TD_N;
  int T_N=2;
  vector[T_N+D_N] zeros;

  // int<lower=1> TD_N = 4;
  // int<lower=1> LENGTH = N;
  TD_N=D_N+T_N;
  print(TD_N);
  zeros = rep_vector(0,TD_N);
  
}

parameters {
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;


  real<lower=0> alpha;
  vector[TD_N] cholesky_variance[N];
}

transformed parameters{
  matrix[TD_N, TD_N] L_Sigma;
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
}

model {
  matrix[N,TD_N] theta_delta;
  matrix[N,T_N] theta;
  matrix[TD_N, TD_N] Sigma;
  vector[TD_N] cholesky_var[N];


  vector[TD_N] td_mean;
  vector[TD_N] td_sd;
  
  Sigma = L_Sigma * L_Sigma';
  theta[,1]=alpha*UnconnectedData[,1]+Deltas[,1];
  theta[,2]=alpha*UnconnectedData[,2]+Deltas[,2];
  
  
  //copy to theatadeltamatrix.
  theta_delta[,1]=theta[,1];
  theta_delta[,2]=theta[,2];
  theta_delta[,3]=Deltas[,1];
  theta_delta[,4]=Deltas[,2];
  theta_delta[,5]=Deltas[,3];
  theta_delta[,6]=Deltas[,4];
  theta_delta[,7]=Deltas[,5];
  
  
  //create an array dataset; generate it from fixed parameters;
  //give it some correlation, and see if we can recover it using this matrix or not.
  
  alpha~normal(0,1);
  //alpha is the extent to the Thetas are made up of entirely new data. The higher the alpha value, the more it's new data.
  //if our cholesky matrix has a strong bias toward decorrelating the variables, alpha will be deviate strongly from 1, but if there is no bias, it will have a mean of 1 and will look like a tidy half-normal distribution.
  

  
  // //normalize.
  for (tdi in 1:TD_N){
    td_mean[tdi] = mean(theta_delta[:,tdi]);
    td_sd[tdi] = sd(theta_delta[:,tdi]);
  }
  //
  //standardize the variance.
  for (i in 1:N){
    cholesky_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
  }


  //predict the variance from the remaining information.
  // L_Omega = lkj_corr_cholesky_rng(TD_N, 1);
  L_Omega ~ lkj_corr_cholesky(1);
  // for (i in 1:TD_N){
  //   L_sigma[i] = fabs(cauchy_rng(0,1)); //these yield standard deviations of each individual value.
  // }
  L_sigma ~ cauchy(0,1);
  
  
  // for (i in 1:LENGTH){
  //   cholesky_variance[i,] = multi_normal_cholesky_rng(zeros,L_Sigma);
  // }
  cholesky_var ~ multi_normal_cholesky(zeros,L_Sigma);
  // td_var ~ multi_normal_cholesky(zeros,L_Sigma);
  
  
}


