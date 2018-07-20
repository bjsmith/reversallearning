//taken from rl_single_exp_joint_v6.stan.
//v2: Change in the way the discriminability parameter works.
//v1: I added a "discriminability" parameter
//    I assume that participants will always be distracted by the last few trials
//    They will never confuse the n-1 trial with the current trial because the values alternate
//    However they may confuse n-2, n-3, n-4 trials with one another.
//    I model "discriminability" parameter delta which determines the likelihood
//    participants will behave according to the prediction from the expected value of the actual selection
//    rather than expected value of one of the potential confusion trials.
//    In a hierarchical model, we could write a constraint for the confusability of any two specfic cues
//    and estimate that (maybe?) but in the current model we just have to assume constant confuseability
//    I think this will still be much better than the current task.

data{
   int<lower=1> LENGTH;
   int<lower=2> NUM_CHOICES;
   real<lower=0> A;
   vector[LENGTH] response_time;
   int response[LENGTH];
   int required_choice[LENGTH];//the choice which would be reinforced on each round.
   int cue[LENGTH];
   
   int presentation_order[LENGTH];
   int n_back;
     
     
  ////////////\begin{joint model machinery}
  vector[5] td_mu_prior;
  vector[5] td_sd_prior;
  ////////////\end{joint model machinery}   
  //////neural model
  matrix[LENGTH,4] neural_data;
  int DELTA_accumbens_lh;// = 3;
  int DELTA_accumbens_rh;// = 4;
  int DELTA_ofc_lh;// = 5;
  int DELTA_ofc_rh;// = 6;
}
transformed data{
  int THETA_rpe=1;
  //int THETA_ev=0;
  int TD_rpe=THETA_rpe;
  //int TD_ev=THETA_ev;
  int TD_accumbens_lh = DELTA_accumbens_lh+THETA_rpe;
  int TD_accumbens_rh = DELTA_accumbens_rh+THETA_rpe;
  int TD_ofc_lh = DELTA_ofc_lh+THETA_rpe;
  int TD_ofc_rh = DELTA_ofc_rh+THETA_rpe;
  int TD_N=TD_ofc_rh;
  
  vector[TD_N] zeros = rep_vector(0,TD_N);
  
  real<lower=0> s = 1;

  matrix[LENGTH,NUM_CHOICES] choice_outcomes;
  for (i in 1:LENGTH){
    //if the required choice was chosen then assign the value of 1 to it and distribute the value of -1 across all alternatives.
    //or if the required choice was not chosen, assign the value of -1 to it and distribute the value of 1 across all alternatives.
    for (j in 1:NUM_CHOICES){
      if(j==response[i]){
        choice_outcomes[i,j]=((response[i]==required_choice[i])-0.5)*2;
      }else{
        choice_outcomes[i,j]=-((response[i]==required_choice[i])-0.5)*2/(NUM_CHOICES-1);
      }
    }
  }
  
  //make sure trials are in order.
  for (i in 1:LENGTH){
    if(presentation_order[i]!=i){
      //throw error.
      reject("the trials across a run must be presented in order.")
    }
  }
  
  
  
}

parameters {
  real alpha_pr;
  // real k_pr;
  // real tau_pr;
  real beta_pr;
  // 
  real d_prime;
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_mu;
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
}

transformed parameters {
  real <lower=0,upper=1> alpha;
  real<lower=0> beta;
  //real<lower=0> k;
  //real<lower=0> tau;
  
  // ////////////\begin{joint model machinery}
  matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix [TD_N,TD_N] Sigma = L_Sigma * L_Sigma';
  // 
  // ////////////\end{joint model machinery}
  
  alpha = inv_logit(alpha_pr);
  //k = exp(k_pr);
  //tau = exp(tau_pr);
  beta = exp( beta_pr);
}

model {
  matrix[max(cue),NUM_CHOICES] exp_val = rep_matrix(0,max(cue),NUM_CHOICES);
  real pred_err;
  vector[LENGTH] run_pred_err_c2;
  vector[LENGTH] trial_expected_val;
  real outcome;
  vector[NUM_CHOICES] v;
  ////////\begin{n-back code}
  int trial_n_back;//definition of this is slightly idiosyncratic. 
  //This is a number determining how many prior trials are paid attention. Becasue we assume subjects
  //never struggle to distinguish between trial i and trial i-1, the "n" indicates the number of trials prior to the current one
  // that might confuse subjects. Thus if trial_n_back=3, then n-2, n-3, n-4 trials will be considered.
  vector[NUM_CHOICES] exp_val_g_pr_recognition;
  real pr_recognition;
  real cue_familiarity;
  vector[trial_n_back] distractor_discriminability;
  ////////\end{n-back code}
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_var[LENGTH];
  matrix[LENGTH, TD_N] theta_delta;
  vector[TD_N] td_sd;
  // ////////////\end{joint model machinery}
  
  alpha_pr ~ normal(0,1.5);//a uniform prior in the logit
  beta_pr ~ normal(0,3);
  // k_pr ~ normal(log(.5),5); 
  // tau_pr ~ normal(log(.5),3);//normal(.5,.5)T[0,];
  d_prime ~ normal(0,1); //d' actually works on the normal distribution
  //https://en.wikipedia.org/wiki/Sensitivity_index
  //so we will keep it measured and quantified in this format.
  

  for (i in 1:LENGTH){//loop through timesteps.
    
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    trial_expected_val[i] = exp_val[cue[i],response[i]];
    
    
    //alright....so this is basically assuming that the subject correctly identifies the expected value
    //and then does a toss-up on their likelihood of actually selecting that value.
    //we need to model based on a discriminability parameter d_prime the subject's probability of picking the correct option
    //we could actually consider removing the beta parameter...
    //then the probability of a response is going to depend on the probability of recognizing the cue, 
    //rather than one of the alternatives
    //response[i] ~ categorical_logit( to_vector(exp_val[cue[i],]) * beta);
    //(assumes that these trials ARE in order. I have added an assertion command to ensure that they are)
    
    //how many trials to go back? we're going to fix at a constant of up to 3 for now.
    //will NOT do the last trial, so it will be n-2, n-3, n-4
    //but n-back can't be more than the number of trials available
    trial_n_back=max(min(n_back,i-2),0);
    
    
    
    pr_recognition=inv_logit(d_prime+logit(cue_familiarity/2+0.5));
    //probability of discriminability between the target stimulus and EACH of the distractor stimulus should be proportional to
    //familiarity with the target stimulus as well as familiarity with the given distractor stimulus
    //so we posit:
    //real cue_familiarity;
    vector[1:trial_n_back] distractor_familiarity;
    real distractor_discriminability;//the discriminability of the distractor from the target, INDEPENDENT of the general d_prime
    cue_familiarity=min([max([sd(exp_val[cue[i],])^2/2,0.001]),0.999]);
    //cue_familiarity=min([max([abs(exp_val[cue[i],1]),0.001]),0.999]);
    
    
    
    
    if(trial_n_back==0){
      //no possibility of confusion.
      exp_val_g_pr_recognition=to_vector(exp_val[cue[i],]);
    }else if (trial_n_back==1){//no sum across necessary, we just look at the one item that was n-back
      exp_val_g_pr_recognition=pr_recognition*to_vector(exp_val[cue[i],])+
      (1-pr_recognition)*to_vector(
        exp_val[cue[(i-trial_n_back-1):(i-2)],]);
    }else{
      //some d_prime possibility of confusion.
      for (di in 1:trial_n_back){
        distractor_familiarity[di]=min([max([sd(exp_val[cue[i-di-1],])^2/2,0.001]),0.999]);
      }
      distractor_discriminability[1:trial_n_back] = cue_familiarity/2+distractor_familiarity[1:trial_n_back]/2;
      distractor_pr_recognition[1:trial_n_back] = inv_logit(d_prime+logit(distractor_discriminability[1:trial_n_back]/2+0.5)*(1-pr_recognition)/trial_n_back);
      
      
      exp_val_g_pr_recognition=(pr_recognition)*to_vector(exp_val[cue[i],]) +
        to_vector(
        (rep_matrix((1-pr_recognition)/trial_n_back,trial_n_back,1)'*(exp_val[cue[(i-trial_n_back-1):(i-2)],])
        ));
      //we assume equal probability for selecting each of the inferior alternatives so we can just calculate mean across.
      
      //some error checking.

      print("round ",i,"; cue:",cue[i],"; cue EV:",exp_val[cue[i],],"; logit(cue_familiarity):",logit(cue_familiarity));
      print("distractor cue set:");
      print(cue[(i-trial_n_back-1):(i-2)]);
      print("distractor cue EVs");
      print(exp_val[cue[(i-trial_n_back-1):(i-2)],]);
      print("pr(recognition):",pr_recognition,"; exp_val_g_pr_recognition:", exp_val_g_pr_recognition);
      
    }
    
    
    response[i] ~ categorical_logit( exp_val_g_pr_recognition * beta);
    
    for(j in 1:NUM_CHOICES){
      //v[j]=logit(exp_val[cue[i],j]/4+0.75);
      pred_err=choice_outcomes[i,j]-exp_val[cue[i],j]; 
      
      
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      //for occam's razor, I'm going to avoid any transformation from expected value to drift rate. we'll treat expected value as drift rate exactly!
      
    }
    //we're going to do prediction error just for the last choice
    run_pred_err_c2[i] = pred_err;
    
    //i don't know what to do with non-resposne time...but let's work that out later.
    //response_time[i] ~ lba(response[i],k,A,v,s,tau);
  }
  
  ////////////\begin{joint model machinery}
  // 
  theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  // theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  theta_delta[:,TD_accumbens_lh]=neural_data[:, DELTA_accumbens_lh];
  theta_delta[:,TD_accumbens_rh]=neural_data[:, DELTA_accumbens_rh];
  theta_delta[:,TD_ofc_lh]=neural_data[:, DELTA_ofc_lh];
  theta_delta[:,TD_ofc_rh]=neural_data[:, DELTA_ofc_rh];
  
  // //estimate the means for each of our parameters
  // //These should be zero already but just to make sure.
  for (tdi in 1:TD_N){
    td_mu[tdi] ~ normal(td_mu_prior[tdi],td_sd_prior[tdi]);
    td_sd[tdi] = sd(theta_delta[:,tdi]);
  }
  // 
  //standardize the variance.
  for (i in 1:LENGTH){
    td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mu)./td_sd;
  }
  //predict the variance from the remaining information.
  L_Omega ~ lkj_corr_cholesky(1);
  L_sigma ~ cauchy(0,1); //these yield standard deviations of each individual value.
  td_var ~ multi_normal_cholesky(zeros,L_Sigma);
  ////////////\end{joint model machinery}

    
}
generated quantities{
  matrix[TD_N,TD_N] y_corr;
  matrix[TD_N,TD_N] inv_mat_var;
  
  inv_mat_var = rep_matrix(((1) ./ sqrt(diagonal(Sigma))),TD_N);
  y_corr = inv_mat_var' .* Sigma .* inv_mat_var;
}
