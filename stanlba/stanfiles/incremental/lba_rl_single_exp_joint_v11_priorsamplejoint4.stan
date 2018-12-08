
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
  real prior_cauchygamma;
  real prior_cholesky;
  
  // ////////////\begin{joint model machinery}
  // vector[THETA_N+DELTA_N] td_mu_prior;
  // vector[THETA_N+DELTA_N] td_sd_prior;
  
  ////////////\end{joint model machinery}   
}
transformed data{
//   ////////////\begin{joint model machinery}
//   ////////////\end{joint model machinery}
  // int THETA_rpe=1;
  // int THETA_ev=2;
  // //int THETA_oc=1;
  // int TD_rpe=THETA_rpe;
  // //int TD_oc=THETA_oc;
  // 
  // //int THETA_N=THETA_oc;
  // int TD_ev=THETA_ev;
  int TD_N=DELTA_N+THETA_N;
//   
   vector[TD_N] zeros = rep_vector(0,TD_N);
//   
   //real<lower=0> s = 1;
// 
//   matrix[LENGTH,NUM_CHOICES] choice_outcomes;
//   for (i in 1:LENGTH){
//     //if the required choice was chosen then 
//     //assign the value of 1 to it and distribute the value of -1 across all alternatives.
//     //or if the required choice was not chosen, 
//     //assign the value of -1 to it and distribute the value of 1 across all alternatives.
//     for (j in 1:NUM_CHOICES){
//       if(j==response[i]){
//         choice_outcomes[i,j]=((response[i]==required_choice[i])-0.5)*2;
//       }else{
//         choice_outcomes[i,j]=-((response[i]==required_choice[i])-0.5)*2/(NUM_CHOICES-1);
//       }
//     }
//   }
}

parameters {
  // real alpha_pr;
  // real k_pr;//relative threshold
  // real tau_pr;//non-decision time
  
  // ////////////\begin{joint model machinery}
  //vector[TD_N] td_mu;
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
  
  //model variables changed to parameters
  // vector[NUM_CHOICES] v;
  
  vector[TD_N] td_var[LENGTH];
}

transformed parameters {
  // real <lower=0,upper=1> alpha;
  // real<lower=0> k;
  // real<lower=0> tau;
  
  // ////////////\begin{joint model machinery}
  matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix[TD_N, TD_N] Sigma = L_Sigma * L_Sigma';
  // ////////////\end{joint model machinery}
  
  // alpha = inv_logit(alpha_pr);
  // k = exp(k_pr);
  // tau = exp(tau_pr);
}

model {
  // matrix[max(cue), NUM_CHOICES] exp_val = rep_matrix(0,max(cue), NUM_CHOICES);
  // real pred_err;
  // vector[LENGTH] run_pred_err_c2;
  // vector[LENGTH] trial_expected_val;
  // vector[LENGTH] actual_outcome;
  // real outcome;
  
  // matrix[LENGTH,NUM_CHOICES] v_record;
  // ////////////\begin{joint model machinery}
  
  // matrix[LENGTH, TD_N] theta_delta;
  vector[TD_N] td_var_std[LENGTH];
  vector[TD_N] td_mean;
  vector[TD_N] td_sd;
  // ////////////\end{joint model machinery}
  
  //these use VERY weak priors because we are going to use this data to set priors for future analyses
  //so it's important that we don't unduly bias analysis at this level.
  //this priors works out to ROUGHLY a uniform distribution between 0 and 1 when transformed.
  // alpha_pr ~ normal(0,1.5);//weak prior, agnostic about learning rate, but still biased to low learning rates.
  //   //as a compromise between an absolute 0 learning rate (-Inf) and a learning rate flat in the middle of the possible distribution (0).
  // 
  // k_pr ~ normal(log(.5),2); //other thing we could do with these is replace them with half-normals as in the Annis paper.
  // //A ~ normal(.5,1)T[0,];
  // //tau_pr ~ normal(log(.5),1);//normal(.5,.5)T[0,];
  // tau_pr ~ normal(log(.5),2);
  //now we need to loop through the trials, modelin
  //so how do we model choices in this context?
  // for (i in 1:LENGTH){//loop through timesteps.
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    
    // trial_expected_val[i] = exp_val[cue[i],response[i]];
    // 
    // for(j in 1:NUM_CHOICES){
    //   v[j]=logit(exp_val[cue[i],j]/4+0.75);//We could simplify this right down to just passing in the expected value. Should we???
    //   v_record[i,j]=v[j];
    //   //if j was the reinforced choice and it was the response value,
    //   pred_err=choice_outcomes[i,j]-exp_val[cue[i],j]; 
    //   
    //   if(j==response[i]){
    //     //recorded reward prediction error has to be the prediction of *what the subject thought would happen*,
    //     //i.e., the RPE for the particular choice they made. churr.
    //     run_pred_err_c2[i] = pred_err;
    //   }
    //   
    //   exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
    //   
    //   //the value of the outcome obtained for this choice.
    //   actual_outcome[i]=choice_outcomes[i,response[i]];
    // }
    // v ~ normal(0,1);
    //i don't know what to do with non-response time...but let's work that out later.
  //   print("response[i]:",response[i],"; k:",k,"; A:",A,"; v:",v,"; s:",s,"; tau:",tau);
  //   response_time[i] ~ lba(response[i],k,A,v,s,tau);
  // }
  ////////////\begin{joint model machinery}
   
  //transfer the one theta into the theta-delta matrix, transforming it at the same time.
  // theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  //theta_delta[:,THETA_oc]=(actual_outcome-mean(actual_outcome))./sd(actual_outcome);
  // theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  //maybe this could make a difference, could check??
  
  //transfer the deltas into the theta-delta matrix.
  // for (d_i in 1:DELTA_N){
  //   theta_delta[:,THETA_N+d_i]=neural_data[:, d_i];
  // }
  
  // //In order to operate a faster model, we don't estimate these means; we just calculate them.
  // //I've got no interest in actually calculating means here, so I think that's the best approach.
  for (tdi in 1:TD_N){
     td_mean[tdi] = mean(td_var[:,tdi]);
     td_sd[tdi] = sd(td_var[:,tdi]);
  }

  //standardize the variance.
  for (i in 1:LENGTH){
     td_var_std[i,:] = (to_vector(td_var[i,:]) - td_mean) ./ td_sd;
  }
  
  //predict the variance from the remaining information.
  L_Omega ~ lkj_corr_cholesky(prior_cholesky);
  L_sigma ~ cauchy(0,prior_cauchygamma); //these yield standard deviations of each individual value.
  td_var_std ~ multi_normal_cholesky(zeros,L_Sigma);
  ////////////\end{joint model machinery}

    
}

