
//v2: this incorporates reward prediction error as the only behavioral model parameter for which covariance is calculated.
//v3: this incorporates changes to priors that I have experimented with in the rl models.
//v4: ??? ran something on Friday 29 but not sure what it was.
//v5: additional changes including a refinement of alpha_pr adding a correlation matrix to go with the covariance matrix. Can compare with rl_single_exp_joint_v6.stan.
//v6 fixed up a substantial error in joint model standardization present in v5.
//v7 allowed an arbitrary number of deltas.
//v8 Fixed up a substantial error with the construction of the reward prediction error calculation.
//v9 Replaces RPE with simple correct response. This effectively makes it a "parallel model" because the behavioral parameters and joint parameters are independent in this model.
//    Also have changed the estimation of mean ThetaDeltas to simple calculation of the mean, because we're not actually interested in knowing them
//    means of these values.
//v10 goes back to RPE, but keeps the change to avoid estimation of mean ThetaDeltas, because this seems unnecessary.
//v11 adds expected value into the model.
//priorsamplejoint: just amples from the hyper joint matrix because that's all I'm itnnteresited in exploring for the moment.
//priorsamplejointv6: taken out the sampling of dummy data; we're just doing the covariance matrix.
data{
   int<lower=1> LENGTH;
   int<lower=2> NUM_CHOICES;
   // real<lower=0> A;
   // vector[LENGTH] response_time;
   // int response[LENGTH];
   // int required_choice[LENGTH];//the choice which would be reinforced on each round.
   // int cue[LENGTH];
   //   
  //////neural model
  int DELTA_N;
  int THETA_N;
  // matrix[LENGTH,DELTA_N] neural_data;
  real prior_sigma;
  real prior_cholesky;
  
  // ////////////\begin{joint model machinery}
  // vector[THETA_N+DELTA_N] td_mu_prior;
  // vector[THETA_N+DELTA_N] td_sd_prior;
  
  ////////////\end{joint model machinery}   
}
transformed data{


  int TD_N=DELTA_N+THETA_N;
//   
   vector[TD_N] zeros = rep_vector(0,TD_N);

}

parameters {
  
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  
}

transformed parameters {
  
  matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix[TD_N, TD_N] Sigma = L_Sigma * L_Sigma';
  
}

model {
  //predict the variance from the remaining information.
  L_Omega ~ lkj_corr_cholesky(prior_cholesky);
  L_sigma ~ cauchy(0,prior_sigma); //these yield standard deviations of each individual value.
  //td_var_std ~ multi_normal_cholesky(zeros,L_Sigma);
  ////////////\end{joint model machinery}

    
}

