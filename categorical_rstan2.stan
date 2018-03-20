data {
  int N ;
  int y[N] ;
  real x[N];
}

parameters {
  //parameter we have to estimate - what's the bias on this?
  real norm_mu;
}

transformed parameters{
  real x_prob[N];
  x_prob = Phi_approx(x);
  print(x);
  print(x_prob);
  print("----");
  
  // for (n in 1:N){
  //   x_prob[N] = Phi_approx(x[N]);
  // }
}

model {
  
  norm_mu ~ normal(0,5);
  
  x ~ normal(norm_mu,1);
  y ~ categorical_logit(to_vector(x_prob));
}
