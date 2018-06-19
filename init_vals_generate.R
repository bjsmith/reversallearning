qnorm2sd<-function(n){
  sample(qnorm(seq(0.025,0.975,0.95/(n-1))),replace=FALSE)
}

get_bootstrapped_init_vals<- function(bootstrapped_sample,bootseed=1761614456,data_to_pass){#bootstrapped_sample<-smart_init_vals[[1]]
  set.seed(bootseed)
  
  #print(paste0("running get_bootstrapped_init_vals; data_to_pass$NUM_SUBJECTS=",data_to_pass$NUM_SUBJECTS))
  return(
    list(
      ################
      ######GROUP LEVEL
      subj_mu=c(bootstrapped_sample$alpha_pr_mean,bootstrapped_sample$k_pr_mean,bootstrapped_sample$tau_pr_mean),
      subj_sigma=c(bootstrapped_sample$alpha_sd_prior,bootstrapped_sample$k_sd_prior,bootstrapped_sample$tau_sd_prior),
      run_sigma_gamma=c(bootstrapped_sample$alpha_run_sigma_gamma,bootstrapped_sample$k_run_sigma_gamma,bootstrapped_sample$tau_run_sigma_gamma),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      #define with an artificially normal, truncated distribution.
      ################
      ####SUBJECT LEVEL
      run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      #might need to get these empirically rather than just trying to filter down.
      #or convert it into non-centered and then see where we go from there.
      run_sigma=cbind(sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$alpha_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$k_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$tau_run_sigma_gamma,replace=FALSE)),
      #I think these cauchys are probably going to screw us!
      #no way we can start with these starting values.
      
      ################
      ######RUN LEVEL
      alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
    )
  )
}
get_init_vals<-function(data_to_pass){
  return(
    list(
      ################
      ######GROUP LEVEL
      subj_mu=c(rnorm(1,data_to_pass$priors_alpha,data_to_pass$priors_alpha_spread),
                rnorm(1,data_to_pass$priors_lba_k,data_to_pass$priors_lba_k_spread),
                rnorm(1,data_to_pass$priors_lba_tau,data_to_pass$priors_lba_tau_spread)),
      subj_sigma=c(abs(rnorm(1,0,data_to_pass$priors_alpha_sd_gamma)),
                   abs(rnorm(1,0,data_to_pass$priors_lba_k_sd_gamma)),
                   abs(rnorm(1,0,data_to_pass$priors_lba_tau_sd_gamma))),
      run_sigma_gamma=c(abs(rnorm(1,0,data_to_pass$priors_alpha_run_sigma_gamma)),
                        abs(rnorm(1,0,data_to_pass$priors_lba_k_run_sigma_gamma)),
                        abs(rnorm(1,0,data_to_pass$priors_lba_tau_run_sigma_gamma))),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      ################
      ####SUBJECT LEVEL
      run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      # run_sigma=cbind(abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_alpha_run_sigma_gamma)),
      #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_k_run_sigma_gamma)),#this seems to be an error.
      #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_tau_run_sigma_gamma))),
      run_sigma=cbind(sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_alpha_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_lba_k_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_lba_tau_run_sigma_gamma,replace=FALSE)),
      
      #I think these cauchys are probably going to screw us!
      #no way we can start with these starting values.
      
      ################
      ######RUN LEVEL
      alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
    )
  )
  
}