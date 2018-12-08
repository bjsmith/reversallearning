data {
  int<lower=0> N; // number of observations
  int<lower=0> x[N]; // outcome variable
  int R;
}
parameters {
  real lambda;
}
model {
  x ~ poisson_log(lambda);  
}
generated quantities {
  cov_matrix[R] Sigma; 
  corr_matrix[R] Omega; 
  vector<lower=0>[R] sigma; 
  
  for (i in 1:R){
    sigma[i] = fabs(normal_rng(0,3));
  }
  
  Omega = lkj_corr_cholesky_rng(R,1);

  Sigma = quad_form_diag(Omega, sigma); 
  
}

