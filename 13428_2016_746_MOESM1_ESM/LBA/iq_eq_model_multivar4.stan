//this is a pretend model where we have two normally distributed variables and we're estimating the covariance of those variables.
//the two pretend variables are IQ and EQ.
//if we want to look hierarchically then we could look at multiple measurements of IQ and EQ for each individual
//we want to estimate the distribution of IQ and EQ in this population
//and also the covariance of IQ and EQ in this population.
//we'll start with independent distributions and then build up.
//https://groups.google.com/forum/#!msg/stan-users/fhghq6Rxbqs/-dG0apK0NV4J
//V4:reparameterized as described in the stan manual p 348.
//their example is a bit different because they're trying to estimate betas from known mus and sigmas but
//let's see how far we can run with it.
//see also p252
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
  //vector[2] zeros = rep_vector(0,2);
  //vector[2] beta[N_SUB];
  //vector[2] alpha[N_SUB];
  vector[2] beta;
  vector[2] alpha;
  //cov_matrix[2] identity = diag_matrix(rep_vector(1.0,2));
  
  // for (s in 1:N_SUB){
  //   beta[s,1]=iq_score_m0[s];
  //   beta[s,2]=eq_score_m0[s];
  // }
  
  
}

parameters{
  
  //cov_matrix[2] var_iqeq;
  
  cov_matrix[2] Sigma;
  
}

transformed parameters{
  matrix [2, 2] L;
  vector [2] mu;
  //row_vector [2] mu_iqeq = [iq_score_mean, eq_score_mean];
  //row_vector [2] sd_iqeq = [sqrt(var_iqeq[1,1]),sqrt(var_iqeq[2,2])];

  //beta = mu + L * alpha; //from the textbook; rearrange to get the value we're estimating in our case.
  //beta - L * alpha = mu
  
  //mu_iqeq = scores_m0 - L * scores_m0_norm;
  
  L = cholesky_decompose(Sigma);
  for (s in 1:N_SUB){
    mu = beta[s] - L * alpha[s];
  }
  // mu = beta - L * alpha;
}

model{
  //var_iqeq ~ inv_wishart(2,identity);
  for (s in 1:N_SUB){
    //scores_m0[s] ~ multi_normal(zeros,var_iqeq);
    alpha[s] ~ normal(0, 1);
    //implies: beta ~ multi_normal(mu, Sigma)
  }
}


