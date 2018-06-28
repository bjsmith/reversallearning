functions{
  
  //this distribution just passes back a high likelihood, just to help identify which likelihoods the model is struggling with.
  real cheat_distribution_log(real outval, real param_1, real param_2){
    #return log(1/(1+fabs(outval)+fabs(param_1+param_2)));
    return max([normal_lpdf(outval|param_1,param_2),-4.0]);
  }
  
  real cheat_distribution_arr_log(real[] outval, real param_1, real param_2){
    #return rep_array(log(1/(1+fabs(mean(outval))+fabs(param_1+param_2))),num_elements(outval));
    #return max([normal_lpdf(outval|param_1,param_2),-4.0]);
    return max([normal_lpdf(outval|param_1,param_2),-4.0]);
  }
  
  
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
      
      #return out;		
      return(-2);
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
    
    real array2d_min(real[,] arr){
      
      real vec[dims(arr)[1]];
      
      for (i in 1:dims(arr)[1]){
        vec[i] = min(arr[i,]);
      }
      return(min(vec));
    }
    
    real array2d_max(real[,] arr){
      
      real vec[dims(arr)[1]];
      
      for (i in 1:dims(arr)[1]){
        vec[i] = max(arr[i,]);
      }
      return(max(vec));
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
    
    real priors_alpha;
    real priors_lba_k;
    real priors_lba_tau;
    
    real priors_alpha_spread;
    real priors_lba_k_spread;
    real priors_lba_tau_spread;
    
    real priors_alpha_sd_gamma;
    real priors_lba_k_sd_gamma;
    real priors_lba_tau_sd_gamma;
    
    real priors_alpha_run_sigma_gamma;
    real priors_lba_k_run_sigma_gamma;
    real priors_lba_tau_run_sigma_gamma;
  }
  transformed data{
    real<lower=0> lba_sd = 1;
    int PARID_alpha = 1;
    int PARID_lba_k = 2;
    int PARID_lba_tau = 3;
    int NUM_PARAMS = 3;
    //real priors_lba_k=log(0.5);
    //real priors_lba_tau=log(.2*0.9); #a more realistic mean non-decision time.
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
    // print(choice_outcomes);
    print(NUM_SUBJECTS);
    print(NUM_TRIALS);
    print(NUM_RUNS);
    // print(trial_runid);
    
    
  }
  
  parameters {
    ////////////////////
      //GROUP LEVEL
    vector[NUM_PARAMS] subj_mu;
    vector<lower=0>[NUM_PARAMS] subj_sigma;
    
    real<lower=0> run_sigma_gamma[NUM_PARAMS];
    
    
    ////////////////////
      //SUBJECT LEVEL
    //real run_mu[NUM_SUBJECTS,3];
    real run_mu_var[NUM_SUBJECTS,NUM_PARAMS];
    real<lower=0> run_sigma[NUM_SUBJECTS,NUM_PARAMS];
    
    ////////////////////
      //RUN LEVEL
    // real alpha_pr[NUM_RUNS];
    // real k_pr[NUM_RUNS];
    // real tau_pr[NUM_RUNS];
    real alpha_pr_var[NUM_RUNS];
    real k_pr_var[NUM_RUNS];
    real tau_pr_var[NUM_RUNS];
  }
  
  transformed parameters {
    vector[NUM_PARAMS] run_mu[NUM_SUBJECTS];
    
    real <lower=0,upper=1> alpha[NUM_RUNS];
    real<lower=0> k[NUM_RUNS];
    real<lower=0> tau[NUM_RUNS];
    
    //////////////////////
      //SUBJECT LEVEL
    for (s in 1:NUM_SUBJECTS){
      run_mu[s,] = to_vector(run_mu_var[s,]) .* subj_sigma + subj_mu;
    }//according to the stan manual, an elementwise vector operation would NOT be more efficient.
    
    //////////////////////
      //RUN LEVEL
    // alpha = inv_logit(alpha_pr);
    // k = exp(k_pr);
    // tau = exp(tau_pr);
    for (r in 1:NUM_RUNS){
      alpha[r] = inv_logit(run_mu[run_subjid[r],PARID_alpha] + run_sigma[run_subjid[r],PARID_alpha] * alpha_pr_var[r]);
      k[r] = exp(run_mu[run_subjid[r],PARID_lba_k] + run_sigma[run_subjid[r],PARID_lba_k] * k_pr_var[r]);
      tau[r] = exp(run_mu[run_subjid[r],PARID_lba_tau] + run_sigma[run_subjid[r],PARID_lba_tau] * tau_pr_var[r]);
    }
  }
  
  model {
    real exp_val[NUM_RUNS,max(cue),NUM_CHOICES];
    real pred_err;
    real outcome;
    vector[NUM_CHOICES] v;
    //int curRunId=0;//just for printout diagnostic
    exp_val = rep_array(0,NUM_RUNS,max(cue),NUM_CHOICES);
    
    #we have to print every fucking initial value. with its name.
    // print("subj_mu:");print(min(subj_mu),",",max(subj_mu));
    // print("subj_sigma:");print(subj_sigma);print(subj_sigma);
    // print("run_sigma_gamma:");print(run_sigma_gamma);print(run_sigma_gamma);
    // print("run_mu_var:");print(run_mu_var);print(run_mu_var);
    // print("run_sigma:");print(run_sigma);
    // print("alpha_pr_var:");print(alpha_pr_var);
    // print("k_pr_var:");print(k_pr_var);
    // print("tau_pr_var:");print(tau_pr_var);
    // 
    //   print("subj_mu:");print(min(subj_mu),",",max(subj_mu));
    // print("subj_sigma:");print(min(subj_sigma),",",max(subj_sigma));
    // print("run_sigma_gamma:");print(min(run_sigma_gamma),",",max(run_sigma_gamma));
    // print("run_mu_var:");print(array2d_min(run_mu_var),",",array2d_max(run_mu_var));
    // print("run_sigma:");print(array2d_min(run_sigma),",",array2d_max(run_sigma));
    // print("alpha_pr_var:");print(min(alpha_pr_var),",",max(alpha_pr_var));
    // print("k_pr_var:");print(min(k_pr_var),",",max(k_pr_var));
    // print("tau_pr_var:");print(min(tau_pr_var),",",max(tau_pr_var));
    // 
    ////////////////////
      //GROUP LEVEL
    
    //priors for mean of subject params
    subj_mu[PARID_alpha] ~ cheat_distribution(0,1);// normal(priors_alpha,priors_alpha_spread);
    //also changed this. -3 was a pessimistic, implausible prior. -1 is more realistic
    //and might be a better place to start.
    // At least, this cannot possibly make estimation harder.
    // in a three-parameter model, a pessimistic alpha would force k and tau to make up for it and that's no good.
    subj_mu[PARID_lba_k] ~ cheat_distribution(0,1);// normal(priors_lba_k,priors_lba_k_spread);
    subj_mu[PARID_lba_tau] ~ cheat_distribution(0,1);// normal(priors_lba_tau,priors_lba_tau_spread);
    
    //priors for deviation of subject params from their mean.
    subj_sigma[PARID_alpha] ~ cheat_distribution(0,1);// cauchy(0,priors_alpha_sd_gamma); 
    //these have lower prior SDs because our priors for them originally, from Palmeri et al., were lower.
    subj_sigma[PARID_lba_k] ~ cheat_distribution(0,1);// cauchy(0,priors_lba_k_sd_gamma); 
    subj_sigma[PARID_lba_tau] ~ cheat_distribution(0,1);// cauchy(0,priors_lba_tau_sd_gamma);
    
    
    run_sigma_gamma[PARID_alpha] ~ cheat_distribution(0,1);// cauchy(0,priors_alpha_run_sigma_gamma);
    run_sigma_gamma[PARID_lba_k] ~ cheat_distribution(0,1);// cauchy(0,priors_lba_k_run_sigma_gamma);
    run_sigma_gamma[PARID_lba_tau] ~ cheat_distribution(0,1);// cauchy(0,priors_lba_tau_run_sigma_gamma);
    
    ////////////////////
    //RUN LEVEL
    for (s in 1:NUM_SUBJECTS){
    run_mu_var[s,] ~ cheat_distribution_arr(0,1);// normal(0,1);//non-centered data
    //run_mu[s,] ~ normal(subj_mu,subj_sigma);//I think we can do this vectorized here.
    
    //we do NOT assume same run-level variance across subjects but we do assume a constant prior for these.
    //this simplifies the calculation although with sigma unconstrained across subjects it may make the estimation actually harder.
    //these might be too narrow, but I'm wary of having so much variance at every level!
      
    #THIS IS LIKELY TO BE CAUSING SUBSTANTIAL EFFICIENCY PROBLEMS! APPLYING A CAUCHY WITH A GAMMA-GENERATED CAUCHY!!!
    run_sigma[s,PARID_alpha] ~ cheat_distribution(0,1);// cauchy(0,run_sigma_gamma[PARID_alpha]); 
    run_sigma[s,PARID_lba_k] ~ cheat_distribution(0,1);// cauchy(0,run_sigma_gamma[PARID_lba_k]); 
    run_sigma[s,PARID_lba_tau] ~ cheat_distribution(0,1);// cauchy(0,run_sigma_gamma[PARID_lba_tau]); 
    }
  
  alpha_pr_var ~ cheat_distribution_arr(0,1);// normal(0,1);
  k_pr_var ~ cheat_distribution_arr(0,1);// normal(0,1);
  tau_pr_var ~ cheat_distribution_arr(0,1);// normal(0,1);
  
  
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

