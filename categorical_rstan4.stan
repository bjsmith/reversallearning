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
  
  
  
  // for (n in 1:N){
  //   x_prob[N] = Phi_approx(x[N]);
  // }
}

model {
  real x_prob[N];
  norm_mu ~ normal(0,5);
  
  
  
  
  x ~ normal(norm_mu,1);
  for (n in 1:N){
     x_prob[n] = Phi_approx(x[n]);
  }
  print(x[1]);
  print(x_prob[1]);
  print("----");
  y ~ categorical_logit(to_vector(x_prob));
}
