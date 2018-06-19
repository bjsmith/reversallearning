functions{
  real array2d_max(real[,] arr){
       
       real vec[dims(arr)[1]];
       
       for (i in 1:dims(arr)[1]){
         vec[i] = max(arr[i,]);
       }
       return(max(vec));
     }
}
#written to test the properties of a transform from inv_logit to normal.
data{
  int n_tosses;
  int tosses[n_tosses];
  real useless_array[2,3];
}

parameters {
  real pr_heads_norm;
  
  
}

transformed parameters {
  real<lower=0,upper=1> pr_heads_tr = inv_logit(pr_heads_norm);
  real useless_array_max;
  useless_array_max=array2d_max(useless_array);
}

model {
  print("pr_heads_norm",pr_heads_norm);
  print("pr_heads_tr",pr_heads_tr);
  #can do a sample statement with output being n_heads
  
  pr_heads_norm ~ normal(0,1);
  tosses ~ bernoulli(pr_heads_tr);
  
}






