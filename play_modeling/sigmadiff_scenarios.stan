//https://www.rdocumentation.org/packages/rstan/versions/2.17.3/topics/stan_rdump
data {
  int<lower = 0> N;
  int<lower = 0> G;
  vector[N] y;
  int y_group [N];
  int group_size_max;
}

transformed data{
  int group_size [G] =rep_array(0,G);
  int group_members [G,group_size_max];//store the members of each group.
  for (n in 1:N){
    //count the members of each group.
    group_size[y_group[n]] = group_size[y_group[n]] + 1;//tally of the number of members of each group
    group_members[y_group[n],group_size[y_group[n]]]=n;//record of the members of each group.
  }
  //print the group members to check we're doing this right.
  //print(group_members);
  //this checked out, by the way :-)
}

parameters {
  vector[2] mu;
  positive_ordered[2] sigma;
  real<lower=0, upper=1> theta;
}

model {
  
  sigma ~ cauchy(0,5);
  mu ~ normal(2, 2);
  theta ~ beta(5, 5);
  
   
  for (g in 1:G){
    int group_indices[group_size[g]] = group_members[g,1:group_size[g]];
    real scenario1 = normal_lpdf(y[group_indices] | mu[1], sigma[1]);
    real scenario2 = normal_lpdf(y[group_indices] | mu[2], sigma[2]);
    
    target += log_mix(theta,
                      scenario1,
                      scenario2);
  }
    
}
