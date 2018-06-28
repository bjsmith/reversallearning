#written to test the properties of a transform from inv_logit to normal.
#we have a second variable, income, which records money earned at each toss for some reason. It follows a bernoulli distribution.
data{
  int n_tosses;
  int tosses[n_tosses];
  int tosses_coin2[n_tosses];
  #real income[n_tosses];
  
}

parameters {
  real pr_heads_norm;
  real pr_heads_norm_coin2;
  #real income_norm;
}

transformed parameters {
  real<lower=0,upper=1> pr_heads_tr = inv_logit(pr_heads_norm);
  real<lower=0,upper=1> pr_heads_tr_coin2 = inv_logit(pr_heads_norm_coin2);
  #real<lower=0> income_norm_tr = exp(income_norm);
}

model {
  #can do a sample statement with output being n_heads
  print(pr_heads_tr);
  pr_heads_norm ~ normal(0,1);
  pr_heads_norm_coin2 ~ normal(0,1);
  #income ~ normal(income_norm_tr,1);
  tosses ~ bernoulli(pr_heads_tr);
  tosses_coin2 ~ bernoulli(pr_heads_tr_coin2);
  
  
  
}






