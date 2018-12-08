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
          term_1 = v*Phi(b_A_tv_ts);
          term_2 = s*exp(normal_lpdf(b_A_tv_ts | 0,1)); 
          term_3 = v*Phi(b_tv_ts);
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
          term_1 = b_A_tv/A * Phi(b_A_tv/ts);	
          term_2 = b_tv/A   * Phi(b_tv/ts);
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
                  prob_neg = 1;
                  for(j in 1:num_elements(v)){
                       prob_neg = Phi(-v[j]/s) * prob_neg;    
                  }
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
     int<lower=1> LENGTH;
     int<lower=2> NUM_CHOICES;
     real<lower=0> A;
     //matrix[LENGTH,2] RT;
     vector[LENGTH] response_time;
     int response[LENGTH];
     int required_choice[LENGTH];//the choice which would be reinforced on each round.
     int cue[LENGTH];
     
}
transformed data{
  real<lower=0> s = 1;
  //from stan manual:
  //Before generating any samples, data variables are read in, 
  //then the transformed data variables are declared and the associated statements executed to define them. 
  //This means the statements in the transformed data block are only ever evaluated once.
  
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
  
  print("matrix of choice outcomes generated from required_choice and response input data:")
  print(choice_outcomes)
  
  
}

parameters {
  real alpha_pr;
  real k_pr;
  real tau_pr;
  //vector<lower=0>[NUM_CHOICES] v;
  
}

transformed parameters {
  real <lower=0,upper=1> alpha;
  real<lower=0> k;
  real<lower=0> tau;

  
  alpha = inv_logit(alpha_pr);
  k = exp(k_pr);
  tau = exp(tau_pr);
}

model {
  matrix[max(cue),NUM_CHOICES] exp_val = rep_matrix(0,max(cue),NUM_CHOICES);
  real pred_err;
  real outcome;
  
  alpha_pr ~ normal(-3,3);//weak prior for no learning.  
  k_pr ~ normal(log(.5),1);
  //A ~ normal(.5,1)T[0,];
  tau_pr ~ normal(log(.5),0.5);#normal(.5,.5)T[0,];
  //now we need to loop through the trials, modelin
  //print("alpha_pr:",alpha_pr,"; k_pr:",k_pr,"; tau_pr:",tau_pr)
  //print("alpha:",alpha,"; k:",k,"; tau:",tau)
  //so how do we model choices in this context?
  
  for (i in 1:LENGTH){//loop through timesteps.
    
    for(j in 1:NUM_CHOICES){
      //this might be problematic for more than two choices, but for two, it'll work.
      //assuming the participant infers that for every choice made, the opposite is the *wrong choice*
      //now, if j was the correct choice, then pred_err should be 1-exp_val.
      //  But if j was the incorrect choice, pred_err should be -1-exp_val.
      // so...because we are identifying particular choices, we do need to know which was the rewarded choice.
      //our LBA model relies on getting values for *each choice* so we do need to model that.
      //if j was the reinforced choice and it was the response value,
      pred_err=choice_outcomes[i,j]-exp_val[cue[i],j]; 
      //print("pred_err:",pred_err)
      
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      
      #pred_err_alt=actual_outcome-exp_val[cue[i,j]];
          //v[n] ~ normal(2,1)T[0,];
      //for occam's razor, I'm going to avoid any transformation from expected value to drift rate. we'll treat expected value as drift rate exactly!
      
    }
    //print("i=",i,"; ",exp_val[cue[i],])
    response_time[i] ~ lba(response[i],k,A,to_vector(exp_val[cue[i],]),s,tau);
  }
  
    
}

// generated quantities {
//      vector[2] pred;
//      pred = lba_rng(k,A,v,s,tau);
// }






