library(rstan)
source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/rl_discrim/lba_rl_discrim_v2_functions_v2.R")
source("stanlba/singlelevelmodel/rl_discrim/rl_discrim_v4_functions.R")

source("lba_rl_evaluate_functions.R")
source("freesurfer_region_naming.R")
source("freesurfer_region_naming.R")

load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/rl_joint_discrim_20180914_1/run_package_106_1_reward_rl_discrim_single_exp_joint_v5.RData")

library(rstan)
srm.data<-select_rawdata_cols_for_run(rawdata,106,1,"punishment")
model.data<-create_standatalist(srm.data = srm.data,theta_count=2)
extractedFit<-rstan::extract(run_package$fit)

alpha<-extractedFit$alpha[[1]]
beta<-extractedFit$beta[[1]]

#recreate teh generated model in R code
#data block{}
list2env(model.data,globalenv())
#generated data block{}
n_back=3;

choice_outcomes=matrix(0,LENGTH,NUM_CHOICES);
for (i in 1:LENGTH){
  #if the required choice was chosen then assign the value of 1 to it and distribute the value of -1 across all alternatives.
  #or if the required choice was not chosen, assign the value of -1 to it and distribute the value of 1 across all alternatives.
  for (j in 1:NUM_CHOICES){
    if(j==response[i]){
      choice_outcomes[i,j]=((response[i]==required_choice[i])-0.5)*2;
    }else{
      choice_outcomes[i,j]=-((response[i]==required_choice[i])-0.5)*2/(NUM_CHOICES-1);
    }
  }
}


exp_val = matrix(0,max(model.data$cue), model.data$NUM_CHOICES);
pred_err = NA;
run_pred_err_c2 = vector(mode="numeric",length=model.data$LENGTH);
trial_expected_val = vector(mode="numeric",length=model.data$LENGTH);
y_hat = vector(mode="numeric",length=model.data$LENGTH);

recent_cue_familiarity = vector(mode="numeric",length=n_back);## [n_back];
recent_cue_discriminability = vector(mode="numeric",length=n_back);
recent_cue_id = vector(mode="integer",length=n_back);## [n_back];
cue_match = vector(mode="numeric",length=n_back);
match_score = vector(mode="numeric",length=n_back);
scaled_match_score = vector(mode="numeric",length=n_back);
trial_n_back = NULL
ev_over_discrim = vector("numeric",model.data$NUM_CHOICES);#meant to be a row vector.


log_lik = 0;

for (i in 1:LENGTH){##loop through timesteps.
  #i<-4
  ##the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
  trial_n_back=max(min(n_back,i-2),0);
  
  if(trial_n_back==0){
    #no possibility of confusion.
  }else if (trial_n_back==1){#no sum across necessary, we just look at the one item that was n-back
    recent_cue_familiarity[1] = cue_presentation_n[i-2] - 1;
    recent_cue_id[1] = cue[i-1];
    cue_match[1] = cue[i]==cue[i-2];
  }else{
    recent_cue_familiarity[1:trial_n_back] = cue_presentation_n[(i-trial_n_back-1):(i-2)] - rep(1,trial_n_back);
    for (mi in 1:trial_n_back){
      recent_cue_id[mi] = cue[i-trial_n_back+mi-2];#NEW CODE.
      cue_match[mi] = cue[i]==cue[i-trial_n_back+mi-2];
    }
    
  }
  if(trial_n_back>0){
    recent_cue_discriminability[1:trial_n_back] = 
      recent_cue_familiarity[1:trial_n_back] +
      rep(cue_presentation_n[i] - 1,trial_n_back);
    #discriminability is simply the sum of the familiarity score of the cue and the trial
    
    #so now we know the discriminability 
    match_score[1:trial_n_back]=boot::inv.logit((cue_match[1:trial_n_back]*2-1)*recent_cue_discriminability[1:trial_n_back]*alpha);
    scaled_match_score[1:trial_n_back] = match_score[1:trial_n_back]/sum(match_score[1:trial_n_back]);
  }
  
  if(trial_n_back==0){
    ev_over_discrim = exp_val[cue[i],];
  }else{
    for (j in 1:NUM_CHOICES){#j<-1
      #ev_over_discrim[j] = sum(exp_val[1:trial_n_back,j]*scaled_match_score[1:trial_n_back]);
      #warning("this line I've changed and the main function may be going wrong here.")
      ev_over_discrim[j] = sum(exp_val[recent_cue_id[1:trial_n_back],j]*scaled_match_score[1:trial_n_back]);
    }
  }
  
  
  trial_expected_val[i] = ev_over_discrim[response[i]];
  
  #response[i] ~ categorical_logit( to_vector(exp_val[cue[i],]) * beta);
  #log_lik= log_lik + categorical_logit_lpmf( response[i] |  to_vector(ev_over_discrim) * beta);
  
  # Posterior prediction
  #y_hat[i] = categorical_rng( softmax(to_vector(exp_val[cue[i],]) * beta));
  
  for(j in 1:NUM_CHOICES){#j<-1
    #if j was the reinforced choice and it was the response value,
    pred_err=choice_outcomes[i,j]-exp_val[cue[i],j];
    exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
  }
  #we're going to do prediction error just for the last choice #why is this???
  run_pred_err_c2[i] = pred_err;
  
  
  
  #i don't know what to do with non-resposne time...but let's work that out later.
  #response_time[i] ~ lba(response[i],k,A,v,s,tau);
}


