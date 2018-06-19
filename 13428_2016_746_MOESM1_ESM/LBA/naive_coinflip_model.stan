#written to test the properties of a transform from inv_logit to normal.
data{
  int n_tosses;
  int tosses[n_tosses];
}

parameters {
  real pr_heads_norm;
  
}

transformed parameters {
  real<lower=0,upper=1> pr_heads_tr = inv_logit(pr_heads_norm);
}

model {
  #can do a sample statement with output being n_heads
  print(pr_heads_tr);
  pr_heads_norm ~ normal(0,1);
  tosses ~ bernoulli(pr_heads_tr);
}






