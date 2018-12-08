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
  //corr_matrix[R] Omega001;
  corr_matrix[R] Omega01;
  corr_matrix[R] Omega0;
  corr_matrix[R] Omega1;
  corr_matrix[R] Omega2;
  // corr_matrix[R] Omega5;
  // corr_matrix[R] Omega10;
  // corr_matrix[R] Omega50;
  //Omega001 = lkj_corr_rng(R,.01);
  Omega01 = lkj_corr_rng(R,.5);
  Omega0 = lkj_corr_rng(R,.9);
  Omega1 = lkj_corr_rng(R,1);
  Omega2 = lkj_corr_rng(R,2);
  // Omega5 = lkj_corr_rng(R,5);
  // Omega10 = lkj_corr_rng(R,10);
  // Omega50 = lkj_corr_rng(R,50);
}

