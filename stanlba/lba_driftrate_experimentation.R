source("stanlba/lba_rl_setup.R")

options(mc.cores = 3)

sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
fit_rl_lba_proto4_sub105 <- stan(file='stanlba/stanfiles/drift_rate_mapping/lba_rl_single_exp_drm_unit.stan', 
                                 data = list(
                                   LENGTH=dim(sub105data)[1],
                                   NUM_CHOICES=2,
                                   A=0.01,
                                   response_time=sub105data$reaction_time,
                                   response=sub105data$choice,
                                   required_choice=sub105data$cor_res_Counterbalanced,
                                   cue=sub105data$cue
                                 ),
                                 warmup = 500, 
                                 iter = 1000,
                                 chains = 3,
                                 control = list(max_treedepth = 15))

# Elapsed Time: 2.48604 seconds (Warm-up)
# 1.68655 seconds (Sampling)
# 4.17259 seconds (Total)
# Elapsed Time: 2.78616 seconds (Warm-up)
# 1.69218 seconds (Sampling)
# 4.47834 seconds (Total)

# Elapsed Time: 2.85908 seconds (Warm-up)
# 1.60829 seconds (Sampling)
# 4.46737 seconds (Total)
print(fit_rl_lba_proto4_sub105)
fit_rl_lba_proto4_sub105_drm_logit <- stan(file='stanlba/stanfiles/drift_rate_mapping/lba_rl_single_exp_drm_logit.stan', 
                                 data = list(
                                   LENGTH=dim(sub105data)[1],
                                   NUM_CHOICES=2,
                                   A=0.01,
                                   response_time=sub105data$reaction_time,
                                   response=sub105data$choice,
                                   required_choice=sub105data$cor_res_Counterbalanced,
                                   cue=sub105data$cue
                                 ),
                                 warmup = 500, 
                                 iter = 1000,
                                 chains = 3,
                                 control = list(max_treedepth = 15))

print(fit_rl_lba_proto4_sub105_drm_logit)
#much much better fit!

# Elapsed Time: 3.199 seconds (Warm-up)
# 3.05913 seconds (Sampling)
# 6.25813 seconds (Total)
# 
# 
# 
# Elapsed Time: 2.68986 seconds (Warm-up)
# 2.81785 seconds (Sampling)
# 5.5077 seconds (Total)
# 
# 
# Elapsed Time: 3.6368 seconds (Warm-up)
# 1.58291 seconds (Sampling)
# 5.2197 seconds (Total)

#but not faster; in fact, it's slower, which makes sense, because there's an extra calculation involved.

fit_rl_lba_proto4_sub105_drm_logit <- stan(file='stanlba/stanfiles/drift_rate_mapping/lba_rl_single_exp_drm_logit.stan', 
                                           data = list(
                                             LENGTH=dim(sub105data)[1],
                                             NUM_CHOICES=2,
                                             A=0.01,
                                             response_time=sub105data$reaction_time,
                                             response=sub105data$choice,
                                             required_choice=sub105data$cor_res_Counterbalanced,
                                             cue=sub105data$cue
                                           ),
                                           warmup = 500, 
                                           iter = 1000,
                                           chains = 3,
                                           control = list(max_treedepth = 15))
print(fit_rl_lba_proto4_sub105_drm_logit)

fit_rl_lba_proto4_sub105_drm_invlogit <- stan(file='stanlba/stanfiles/drift_rate_mapping/lba_rl_single_exp_drm_invlogit.stan', 
                                           data = list(
                                             LENGTH=dim(sub105data)[1],
                                             NUM_CHOICES=2,
                                             A=0.01,
                                             response_time=sub105data$reaction_time,
                                             response=sub105data$choice,
                                             required_choice=sub105data$cor_res_Counterbalanced,
                                             cue=sub105data$cue
                                           ),
                                           warmup = 500, 
                                           iter = 1000,
                                           chains = 3,
                                           control = list(max_treedepth = 15))
print(fit_rl_lba_proto4_sub105_drm_invlogit)
#how does the inverse logit do? That's what we've been using all this time :/