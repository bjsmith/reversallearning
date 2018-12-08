#inspired by https://turnermbcn.files.wordpress.com/2018/05/palestrobahgsederberg2018.pdf
#and https://stats.stackexchange.com/questions/161763/how-do-i-use-stan-to-fit-a-covariance-matrix
#written to test the properties of a transform from inv_logit to normal.
#we have a second variable, income, which records money earned at each toss for some reason. It follows a bernoulli distribution.
data{
  int n_tosses;
  int tosses[n_tosses];
  int tosses_coin2[n_tosses];
  
}

transformed data{
  matrix[2,2] identity=diag_matrix(rep_vector(1.0,2));
  
  //assume zero means.
  vector[2] zeros = rep_vector(0,2);
}

parameters {
  #'DeltaTheta', a single vector storing deltas and thetas.
  vector[2] pr_heads_norm;
  cov_matrix[2] cov1;
  
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
  
  tosses ~ bernoulli(pr_heads_tr);
  tosses_coin2 ~ bernoulli(pr_heads_tr_coin2);
  // pr_heads_norm ~ multi_normal([0, 0],Omega);
  cov1~ inv_wishart(2.0,identity);
  pr_heads_norm ~ multi_normal(zeros,cov1);
  
}
