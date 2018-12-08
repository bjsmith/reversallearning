//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.
//we'll start with independent distributions and then build up.
//https://groups.google.com/forum/#!msg/stan-users/fhghq6Rxbqs/-dG0apK0NV4J
data{
  int N_SUB;
  vector [N_SUB] iq_score;
  vector [N_SUB] eq_score;
  
}

transformed data{
  //translate these scores back to mean_zero
  real iq_score_mean=mean(iq_score);
  real eq_score_mean=mean(eq_score);
  vector[N_SUB] iq_score_m0 = iq_score-iq_score_mean;
  vector[N_SUB] eq_score_m0 = eq_score-mean(eq_score);
  vector[2] zeros = rep_vector(0,2);
  row_vector[2] scores_m0[N_SUB];
  cov_matrix[2] identity = diag_matrix(rep_vector(1.0,2));
  for (s in 1:N_SUB){
    scores_m0[s,1]=iq_score_m0[s];
    scores_m0[s,2]=eq_score_m0[s];
  }
  
  
}

parameters{
  //vector[2] mu_iqeq;
  cov_matrix[2] var_iqeq;
}

transformed parameters{
  row_vector [2] mu_iqeq = [iq_score_mean, eq_score_mean];
  row_vector [2] sd_iqeq = [sqrt(var_iqeq[1,1]),sqrt(var_iqeq[2,2])];
}

model{
  //mu_iqeq~normal(100,50);
  
  var_iqeq ~ inv_wishart(2,identity);
  
  for (s in 1:N_SUB){
    scores_m0[s] ~ multi_normal(zeros,var_iqeq);
  }
  
  
}


