data{
  
}
parameters{
  real expected_value_norm;
  //real beta_norm;
  real<lower=0,upper=5> beta;
}

transformed parameters{
  real expected_value;
  
  vector[2] expected_value_vector;
  
  
  expected_value = inv_logit(expected_value_norm)*2-1;
  expected_value_vector=[expected_value, -expected_value]';
 // beta = exp(beta_norm);
  
  
}
model{
  int response1;
  int response2;
  response1 = 1;
  response2 = 2;
  // int choice1[2];
  // int choice2[2];
  // choice1[1]=1;
  // choice1[2]=0;
  // choice2[1]=0;
  // choice2[2]=1;
  
  // int response[2];
  // response = response_param;
  expected_value_norm ~ normal(0,1);
  //beta_norm ~ normal(0,3);
  beta ~ uniform(0,5);
  // print(expected_value_vector);
  // print(beta);
  response1 ~ categorical_logit(expected_value_vector*beta);
  response2 ~ categorical_logit(expected_value_vector*beta);
  
}
generated quantities{
  
  // real response_likelihood_choice_1[2];
  // real response_likelihood_choice_2[2];
  
  real categorical_outcome1;
  real categorical_outcome2;
  real log_probability_difference;
  real probability_difference;
  
  int response1;
  int response2;
  response1 = 1;
  response2 = 2;
  
  
  categorical_outcome1 = categorical_logit_lpmf(response1|expected_value_vector*beta);
  categorical_outcome2 = categorical_logit_lpmf(response2|expected_value_vector*beta);
  log_probability_difference= categorical_outcome2-categorical_outcome1;
  probability_difference=exp(categorical_outcome2)-exp(categorical_outcome1);;
  
  print("LP(categorical_outcome):[", categorical_outcome1, ", ",categorical_outcome2 , "]");
}

