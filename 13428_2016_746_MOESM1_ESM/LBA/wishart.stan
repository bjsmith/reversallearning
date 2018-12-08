data {
  int<lower=0> N; // number of observations
  int<lower=0> x[N]; // outcome variable
  int R;
}
transformed data{
  matrix[R,R] identity;
  matrix[R,R] ones;
  matrix[R,R] half_identity;
  identity = diag_matrix(rep_vector(1, R));
  //ones = rep_matrix(1,R,R);
  for (i in 1:R){
    for (j in 1:R){
      if(i==j){
        half_identity[i,j]=1;
      }else{
        half_identity[i,j] = 0.5;
      }
    }
  }
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
  cov_matrix[R] inv_wishart5_hi;

//  inv_wishart3 = inv_wishart_rng(3.0,identity);
  inv_wishart4 = inv_wishart_rng(4.0,identity);
  inv_wishart5 = inv_wishart_rng(5.0,identity);
  inv_wishart6 = inv_wishart_rng(6.0,identity);
  inv_wishart6 = inv_wishart_rng(7.0,identity);
  
  inv_wishart5_hi = inv_wishart_rng(6.0,half_identity);
  

}

