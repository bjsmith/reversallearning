data{
  int LENGTH;
  matrix[LENGTH,2] RT;
  int NUM_CHOICES;
}

parameters {
  real<lower=0> k;
  real<lower=0> A;
  real<lower=0> tau;
  vector<lower=0>[NUM_CHOICES] v;
}

transformed parameters {
  real s;
  s = 1;
}

model {
  k ~ normal(.5,1)T[0,];
  A ~ normal(.5,1)T[0,];
  tau ~ normal(.5,.5)T[0,];
  for(n in 1:NUM_CHOICES){
    v[n] ~ normal(2,1)T[0,];
  }
  RT ~ lba(k,A,v,s,tau);
}

generated quantities {
  vector[2] pred;
  pred = lba_rng(k,A,v,s,tau);
}






