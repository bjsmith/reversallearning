data {
  int<lower=0> N; // number of observations
  int<lower=0> x[N]; // outcome variable
  int R;
}
transformed data{
  matrix[R,R] allones;
  allones = rep_matrix(1, R,R);
}
parameters {
  real lambda;
}
model {
  x ~ poisson_log(lambda);  
}
generated quantities {
  cov_matrix[R] inv_wishart3; 
  cov_matrix[R] inv_wishart4;
  cov_matrix[R] inv_wishart5;
  cov_matrix[R] inv_wishart6;

  inv_wishart3 = inv_wishart_rng(3.0,allones);
  inv_wishart4 = inv_wishart_rng(4.0,allones);
  inv_wishart5 = inv_wishart_rng(5.0,allones);
  inv_wishart6 = inv_wishart_rng(6.0,allones);
  

}

