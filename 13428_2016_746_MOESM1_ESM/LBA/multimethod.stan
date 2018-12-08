data {
  int<lower=0> N; // number of observations
  int<lower=0> x[N]; // outcome variable
  int R;
}
transformed data{
  matrix[R,R] identity;
  identity = diag_matrix(rep_vector(1, R));
}
parameters {
  real lambda;
}
model {
  x ~ poisson_log(lambda);  
}
generated quantities {
  cov_matrix[R] inv_wishart; 
  cov_matrix[R] wishart; 
  matrix[R,R] lkj_corr; 

  inv_wishart = inv_wishart_rng(3.0,identity);
  wishart = wishart_rng(3.0,identity);
  lkj_corr = lkj_corr_cholesky_rng (R,0.5);

}

