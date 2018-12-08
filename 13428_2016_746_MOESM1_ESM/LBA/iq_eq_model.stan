//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.
//we'll start with independent distributions and then build up.

data{
  int N_SUB;
  vector [N_SUB] iq_score;
  vector [N_SUB] eq_score;
  
}

parameters{
  real mu_iq;
  real mu_eq;
  real<lower=0> var_iq;
  real<lower=0> var_eq;
}

model{
  mu_iq~normal(100,50);
  mu_eq~normal(100,50);
  var_iq~cauchy(0,5);
  var_eq~cauchy(0,5);
  iq_score ~normal(mu_iq,var_iq);
  eq_score ~normal(mu_eq,var_eq);
  
}
