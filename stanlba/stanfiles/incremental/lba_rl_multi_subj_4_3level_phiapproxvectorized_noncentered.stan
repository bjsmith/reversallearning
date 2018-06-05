functions{
     
     real lba_pdf(real t, real b, real A, real v, real s){
          //PDF of the LBA model
          
          real b_A_tv_ts;
          real b_tv_ts;
          real term_1;
          real term_2;
          real term_3;
          real term_4;
          real pdf;
          
          b_A_tv_ts = (b - A - t*v)/(t*s);
          b_tv_ts = (b - t*v)/(t*s);
          term_1 = v*Phi_approx(b_A_tv_ts);
          term_2 = s*exp(normal_lpdf(b_A_tv_ts | 0,1)); 
          term_3 = v*Phi_approx(b_tv_ts);
          term_4 = s*exp(normal_lpdf(b_tv_ts | 0,1)); 
          pdf = (1/A)*(-term_1 + term_2 + term_3 - term_4);
          
          return pdf;
     }
     
     real lba_cdf(real t, real b, real A, real v, real s){
          //CDF of the LBA model
          
          real b_A_tv;
          real b_tv;
          real ts;
          real term_1;
          real term_2;
          real term_3;
          real term_4;
          real cdf;	
          
          b_A_tv = b - A - t*v;
          b_tv = b - t*v;
          ts = t*s;
          term_1 = b_A_tv/A * Phi_approx(b_A_tv/ts);	
          term_2 = b_tv/A   * Phi_approx(b_tv/ts);
          term_3 = ts/A     * exp(normal_lpdf(b_A_tv/ts | 0,1)); 
          term_4 = ts/A     * exp(normal_lpdf(b_tv/ts | 0,1)); 
          cdf = 1 + term_1 - term_2 + term_3 - term_4;
          
          return cdf;
          
     }
     
     real lba_log(real response_time, int response, real k, real A, vector v, real s, real tau){
          
          real t;
          real b;
          real cdf;
          real pdf;		
          real prob;
          real out;
          real prob_neg;
          real prob_neg_over_v[num_elements(v)];

          b = A + k;
          // for (i in 1:rows(RT)){	
             t = response_time - tau;
             if(t > 0){			
                  cdf = 1;
                  
                  for(j in 1:num_elements(v)){
                       if(response == j){
                            pdf = lba_pdf(t, b, A, v[j], s);
                       }else{	
                            cdf = (1-lba_cdf(t, b, A, v[j], s)) * cdf;
                       }
                  }
                  // prob_neg = 1;
                  // for(j in 1:num_elements(v)){
                  //      prob_neg = Phi_approx(-v[j]/s) * prob_neg;    
                  // }
                  for(j in 1:num_elements(v)){
                       prob_neg_over_v[j] = Phi_approx(-v[j]/s);    
                    }
                  prob_neg = prod(prob_neg_over_v);
                  
                  prob = pdf*cdf;		
                  prob = prob/(1-prob_neg);	
                  if(prob < 1e-10){
                       prob = 1e-10;				
                  }
                  
             }else{
                  prob = 1e-10;			
             }		

          out = log(prob);
          //print("lba lprob:",out)
          return out;		
     }
     
    vector lba_rng(real k, real A, vector v, real s, real tau){
          
          int get_pos_drift;	
          int no_pos_drift;
          int get_first_pos;
          vector[num_elements(v)] drift;
          int max_iter;
          int iter;
          real start[num_elements(v)];
          real ttf[num_elements(v)];
          int resp[num_elements(v)];
          real rt;
          vector[2] pred;
          real b;
          
          //try to get a positive drift rate
          get_pos_drift = 1;
          no_pos_drift = 0;
          max_iter = 1000;
          iter = 0;
          while(get_pos_drift){
               for(j in 1:num_elements(v)){
                    drift[j] = normal_rng(v[j],s);
                    if(drift[j] > 0){
                         get_pos_drift = 0;
                    }
               }
               iter = iter + 1;
               if(iter > max_iter){
                    get_pos_drift = 0;
                    no_pos_drift = 1;
               }	
          }
          //if both drift rates are <= 0
          //return an infinite response time
          if(no_pos_drift){
               pred[1] = -1;
               pred[2] = -1;
          }else{
               b = A + k;
               for(i in 1:num_elements(v)){
                    //start time of each accumulator	
                    start[i] = uniform_rng(0,A);
                    //finish times
                    ttf[i] = (b-start[i])/drift[i];
               }
               //rt is the fastest accumulator finish time	
               //if one is negative get the positive drift
               resp = sort_indices_asc(ttf);
               ttf = sort_asc(ttf);
               get_first_pos = 1;
               iter = 1;
               while(get_first_pos){
                    if(ttf[iter] > 0){
                         pred[1] = ttf[iter] + tau;
                         pred[2] = resp[iter]; 
                         get_first_pos = 0;
                    }
                    iter = iter + 1;
               }
          }
          return pred;	
     }
}

data{
     int<lower=2> NUM_CHOICES;
     real<lower=0> A;
     
     int<lower=1> NUM_SUBJECTS;
     int<lower=1> NUM_TRIALS;
     int<lower=1> NUM_RUNS;
     //int<lower=1> trial_subjid[NUM_TRIALS];
     int<lower=1> run_subjid[NUM_RUNS];
     int<lower=1> trial_runid[NUM_TRIALS];
     
     //matrix[NUM_TRIALS,2] RT;
     vector[NUM_TRIALS] response_time;
     int response[NUM_TRIALS];
     int required_choice[NUM_TRIALS];//the choice which would be reinforced on each round.
     int cue[NUM_TRIALS];
     
}
transformed data{
  real<lower=0> lba_sd = 1;
  int PARID_alpha = 1;
  int PARID_lba_k = 2;
  int PARID_lba_tau = 3;
  
  int NUM_PARAMS=3;
  //from stan manual:
  //Before generating any samples, data variables are read in, 
  //then the transformed data variables are declared and the associated statements executed to define them. 
  //This means the statements in the transformed data block are only ever evaluated once.
  
  matrix[NUM_TRIALS,NUM_CHOICES] choice_outcomes;
  for (i in 1:NUM_TRIALS){
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
  
  print("matrix of choice outcomes generated from required_choice and response input data:")
  print(choice_outcomes);
  print(NUM_SUBJECTS);
  print(NUM_TRIALS);
  print(NUM_RUNS);
  print(trial_runid);
  
  
}

parameters {
  ////////////////////
  //GROUP LEVEL
  vector[NUM_PARAMS] subj_mu;
  vector<lower=0>[NUM_PARAMS] subj_sigma;
  
  
  ////////////////////
  //SUBJECT LEVEL
  matrix[NUM_SUBJECTS,NUM_PARAMS] run_mu_var;
  real<lower=0> run_sigma[NUM_SUBJECTS,NUM_PARAMS];
  
  ////////////////////
  //RUN LEVEL
  real alpha_pr[NUM_RUNS];
  real k_pr[NUM_RUNS];
  real tau_pr[NUM_RUNS];
}

transformed parameters {
  vector[NUM_PARAMS] run_mu[NUM_SUBJECTS];
  
  real <lower=0,upper=1> alpha[NUM_RUNS];
  real<lower=0> k[NUM_RUNS];
  real<lower=0> tau[NUM_RUNS];
  
  
  //////////////////////
  //SUBJECT LEVEL
  
  for (s in 1:NUM_SUBJECTS){
    run_mu[s,] = subj_mu + run_mu_var[s,] * subj_sigma;
  }
  
  //////////////////////
  //RUN LEVEL
  
  alpha = inv_logit(alpha_pr);
  k = exp(k_pr);
  tau = exp(tau_pr);
}

model {
  real exp_val[NUM_RUNS,max(cue),NUM_CHOICES];
  real pred_err;
  real outcome;
  vector[NUM_CHOICES] v;
  //int curRunId=0;//just for printout diagnostic
  exp_val = rep_array(0,NUM_RUNS,max(cue),NUM_CHOICES);
  
  ////////////////////
  //GROUP LEVEL
  
  //priors for mean of subject params
  subj_mu[PARID_alpha] ~ normal(-3,3);
  subj_mu[PARID_lba_k] ~ normal(log(.5),1);
  subj_mu[PARID_lba_tau] ~ normal(log(.5),0.5);
  
  //priors for deviation of subject params from their mean.
  subj_sigma[PARID_alpha] ~ cauchy(0,5); 
  //these have lower prior SDs because our priors for them originally, from Palmeri et al., were lower.
  subj_sigma[PARID_lba_k] ~ cauchy(0,3); 
  subj_sigma[PARID_lba_tau] ~ cauchy(0,2);
  
  ////////////////////
  //RUN LEVEL
  for (s in 1:NUM_SUBJECTS){
    //run_mu[s,] ~ normal(subj_mu,subj_sigma);//I think we can do this vectorized here.
    run_mu_var[s,] ~ normal(0,1);//non-centered data
  
    //we do NOT assume same run-level variance across subjects but we do assume a constant prior for these.
    //this simplifies the calculation although with sigma unconstrained across subjects it may make the estimation actually harder.
    //these might be too narrow, but I'm wary of having so much variance at every level!
    
    run_sigma[s,PARID_alpha] ~ cauchy(0,4); 
    run_sigma[s,PARID_lba_k] ~ cauchy(0,2); 
    run_sigma[s,PARID_lba_tau] ~ cauchy(0,1);
  }
  
  for (r in 1:NUM_RUNS){
    ////////////////////
    //SUBJECT LEVEL
    alpha_pr[r] ~ normal(run_mu[run_subjid[r],PARID_alpha],run_sigma[run_subjid[r],PARID_alpha]);//weak prior for no learning.  
    k_pr[r] ~ normal(run_mu[run_subjid[r],PARID_lba_k],run_sigma[run_subjid[r],PARID_lba_k]);
    tau_pr[r] ~ normal(run_mu[run_subjid[r],PARID_lba_tau],run_sigma[run_subjid[r],PARID_lba_tau]);
  }
  
  
  
  for (t in 1:NUM_TRIALS){//loop through timesteps.
    
    for(j in 1:NUM_CHOICES){
      v[j]=logit(exp_val[trial_runid[t],cue[t],j]/4+0.75);
      //our LBA model relies on getting values for *each choice* so we do need to model that.
      //if j was the reinforced choice and it was the response value,
      pred_err=choice_outcomes[t,j]-exp_val[trial_runid[t],cue[t],j]; 

      exp_val[trial_runid[t],cue[t],j] = exp_val[trial_runid[t],cue[t],j] + alpha[trial_runid[t]]*pred_err;
      //for occam's razor, I'm going to avoid any transformation from expected value to drift rate. we'll treat expected value as drift rate exactly!
      
    }
    //print("t=",t,"; ",exp_val[NUM_RUNS,cue[t],]);
    response_time[t] ~ lba(response[t],k[trial_runid[t]],A,v,lba_sd,tau[trial_runid[t]]);
    
  }
  
    
}

