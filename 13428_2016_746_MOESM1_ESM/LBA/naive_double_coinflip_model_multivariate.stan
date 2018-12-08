#written to test the properties of a transform from inv_logit to normal.
#we have a second variable, income, which records money earned at each toss for some reason. It follows a bernoulli distribution.
data{
  int n_tosses;
  int tosses[n_tosses];
  int tosses_coin2[n_tosses];
  
}

parameters {
  
  real covar_term;
  vector[2] joint_phi;
  cov_matrix[2] joint_Sigma;
  vector[2] z_pr_heads_norm;
}

transformed parameters {
  real<lower=0,upper=1> pr_heads_tr;
  real<lower=0,upper=1> pr_heads_tr_coin2;
  vector[2] pr_heads_norm;
  #matrix[2,2] joint_sigma=[[1,covar_term],[covar_term,1]];
  #real<lower=0> income_norm_tr = exp(income_norm);
  pr_heads_norm = joint_phi + joint_Sigma * z_pr_heads_norm;
  pr_heads_tr = inv_logit(pr_heads_norm[1]);
  pr_heads_tr_coin2  = inv_logit(pr_heads_norm[2]);
}

model {
  #can do a sample statement with output being n_heads
  //print(pr_heads_tr);
  // pr_heads_norm[1] ~ normal(0,1);
  // pr_heads_norm[2] ~ normal(0,1);
  #income ~ normal(income_norm_tr,1);
  tosses ~ bernoulli(pr_heads_tr);
  tosses_coin2 ~ bernoulli(pr_heads_tr_coin2);
  
  #I want to know for the sake of argument: to what degree do these two covary?
  #will this design tell us?
  #covar_term~normal(0,1);
  #joint_phi~normal(0,1);

  #https://groups.google.com/forum/#!topic/stan-users/xOKOkitZ_Zw
  #pr_heads_norm ~ multi_normal(joint_phi,joint_Sigma);
  z_pr_heads_norm ~ normal(0,1);
  
  
}






