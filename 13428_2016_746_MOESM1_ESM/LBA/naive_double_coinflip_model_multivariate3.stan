#inspired by https://turnermbcn.files.wordpress.com/2018/05/palestrobahgsederberg2018.pdf

#written to test the properties of a transform from inv_logit to normal.
#we have a second variable, income, which records money earned at each toss for some reason. It follows a bernoulli distribution.
data{
  int n_tosses;
  int tosses[n_tosses];
  int tosses_coin2[n_tosses];
  
}

parameters {
  #'DeltaTheta', a single vector storing deltas and thetas.
  vector[2] pr_heads_norm;
  // 
  // vector[2] phi;
  // cov_matrix[2] Omega;
  // #vector[2] phi0;
  // #cov_matrix[2] s0;
  // real I0;
  // cov_matrix[2] n0;
  
}

transformed parameters {
  real<lower=0,upper=1> pr_heads_tr;
  real<lower=0,upper=1> pr_heads_tr_coin2;
  #matrix[2,2] Sigma;
  
  
  pr_heads_tr = inv_logit(pr_heads_norm[1]);
  pr_heads_tr_coin2  = inv_logit(pr_heads_norm[2]);
  #convert omega to Sigma for convenience
  #Sigma = inverse(Omega);#NB: stan strongly recommends against using a matrix inverse operation.
}

model {
  // tosses ~ bernoulli(pr_heads_tr);
  // tosses_coin2 ~ bernoulli(pr_heads_tr_coin2);
  // pr_heads_norm ~ multi_normal([0, 0],Omega);
  #pr_heads_norm ~ multi_normal([0, 0],[[1,0],[0,1]]);
  pr_heads_norm~normal(0,1);
  #prior on hyperparameter
  #phi ~multi_normal(phi0,s0);
  // Omega ~ wishart(I0,n0);#NB: see discussion in Stan manual, page 351, about re-parameterizing a Wishart distribution into something more efficient.
  // print(I0);
  // print(n0);
  // print(Omega);
}
