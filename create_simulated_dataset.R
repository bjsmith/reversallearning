  if(length(grep("nate_files",getwd()))>0){
  source("../../util/apply_local_settings.R")
  modelcode_rl<-""
  apply_local_settings("../")
}else if(length(grep("notebooks",getwd()))>0){
  source("../../util/apply_local_settings.R")
  modelcode_rl<-"../nate_files/"
  apply_local_settings("../")
}else{
  source("../util/apply_local_settings.R")
  modelcode_rl<-"nate_files/"
  apply_local_settings()
}

source("../util/graphics.R")

# create_simulated_dataset<-function(){
# }

#we gotta load the actual data.

rawdata <- data.table(read.table(paste0(localsettings$data.dir,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T))


#generate group mean and sigma alpha and beta values.
#start with values similar to what we actually estimated
#look up the values. I think we need to apply that phi approximation; should be fine to do the actual function rather than just he approximation

# A fast approximation to the cumulative unit normal distribution function Φ is im- plemented in Stan as the function Phi_approx. The approximate probit regression model may be coded with the following.
# y[n] ~ bernoulli(Phi_approx(alpha + beta * x[n]));
pnorm(1.5)
#library(boot)
#Phi_approx<-function(x){inv.logit(0.07056*x^3+1.5976*x)}
#so yes, pnorm is what we need to transform our normals into 

#what values do we model from?
#let's try these summary stats I pulled from the du_model_rev5a:
get_param_col_means<-function(x){
  if(length(dim(x))==1){
    return(mean(x))
  }else if(length(dim(x))==2){
    return(colMeans(x))
  }else{
    
  }
}
# > cbind(lapply(rstan::extract(model.stanfits[[1]]),mean),lapply(rstan::extract(model.stanfits[[2]]),mean),lapply(rstan::extract(model.stanfits[[3]]),mean))
#                       [,1]        [,2]       [,3]        
# group_pr_mu           -1.11294    -1.02921   -1.207097   
# group_pr_sigma        0.5042885   0.577777   0.4992102   
# group_pr_rpdiff_mu    -0.05776922 0.02649249 -0.01221717 
# group_pr_rpdiff_sigma 0.2571479   0.1956869  0.306303    
# alpha_s_pr_mu         -0.4971525  -0.4356513 -0.7548656  
# beta_s_pr_mu          -1.736311   -1.626549  -1.672656   
# alpha_s_pr_rpdiff_mu  -0.1587205  0.02275932 -0.01621103 
# beta_s_pr_rpdiff_mu   0.04292631  0.03070179 -0.007735852
# alpha_s_pr_sigma      0.3948113   0.2856412  0.3453466   
# beta_s_pr_sigma       0.1700316   0.1318389  0.1428143   
# alpha_pr              -0.4975857  -0.4356989 -0.754887   
# beta_pr               -1.736607   -1.626652  -1.672801   
# alpha                 0.3655653   0.3890362  0.2891645   
# beta                  0.6460625   0.794882   0.7260629   
# group_mu_alpha        0.3130653   0.333738   0.231109    
# group_mu_beta         0.5822459   0.7308061  0.6672794   
# group_sigma_alpha     0.7940012   0.829988   0.7916435   
# group_sigma_beta      8.01848     8.091897   8.004766    
# group_rew_mu_alpha    0.286203    0.3378969  0.2294421   
# group_rew_mu_beta     0.6098083   0.7542341  0.6631916   
# group_pun_mu_alpha    0.3416397   0.3298749  0.2340018   
# group_pun_mu_beta     0.5566619   0.7084657  0.6732218   
# lp__                  -23179.24   -27737.82  -17305.79  

# MODEL 1 (GROUP 1)
# $group_pr_mu	[1] -0.4909911 -1.7348893
# $group_pr_sigma	[1] 0.8252644 0.1833125
# $group_pr_rpdiff_mu	[1] -0.15812869  0.04259026
# $group_pr_rpdiff_sigma	[1] 0.42305243 0.09124341
# $alpha_s_pr_sigma	[1] 0.3948113
# $beta_s_pr_sigma	[1] 0.1700316
# $group_mu_alpha	[1] 0.3130653
# $group_mu_beta	[1] 0.5822459
# $group_sigma_alpha	[1] 0.7940012
# $group_sigma_beta	[1] 8.01848
# $group_rew_mu_alpha	[1] 0.286203
# $group_rew_mu_beta	[1] 0.6098083
# $group_pun_mu_alpha	[1] 0.3416397
# $group_pun_mu_beta	[1] 0.5566619
# $lp__	[1] -23179.24
# MODEL 2 (GROUP 2)
# 
# $group_pr_mu	[1] -0.4325632 -1.6258576
# $group_pr_sigma	[1] 0.9589362 0.1966178
# $group_pr_rpdiff_mu	[1] 0.02228016 0.03070482
# $group_pr_rpdiff_sigma	[1] 0.32770447 0.06366926
# $alpha_s_pr_sigma	[1] 0.2856412
# $beta_s_pr_sigma	[1] 0.1318389
# $group_mu_alpha	[1] 0.333738
# $group_mu_beta	[1] 0.7308061
# $group_sigma_alpha	[1] 0.829988
# $group_sigma_beta	[1] 8.091897
# $group_rew_mu_alpha	[1] 0.3378969
# $group_rew_mu_beta	[1] 0.7542341
# $group_pun_mu_alpha	[1] 0.3298749
# $group_pun_mu_beta	[1] 0.7084657
# $lp__	[1] -27737.82
# 
# MODEL 3 (GROUP 3)
# 
# $group_pr_mu	[1] -0.7431853 -1.6710092
# $group_pr_sigma	[1] 0.8175503 0.1808701
# $group_pr_rpdiff_mu	[1] -0.016748893 -0.007685442
# $group_pr_rpdiff_sigma	[1] 0.4867189 0.1258871
# $alpha_s_pr_sigma	[1] 0.3453466
# $beta_s_pr_sigma	[1] 0.1428143
# 
# $group_mu_alpha	[1] 0.231109
# $group_mu_beta	[1] 0.6672794
# $group_sigma_alpha	[1] 0.7916435
# $group_sigma_beta	[1] 8.004766
# $group_rew_mu_alpha	[1] 0.2294421
# $group_rew_mu_beta	[1] 0.6631916
# $group_pun_mu_alpha	[1] 0.2340018
# $group_pun_mu_beta	[1] 0.6732218
# $lp__	[1] -17305.79

#let's generate everything based on the normally distributed values, since it was generated this way originally...
#group-level mu and sigma, rew-pun diff values
#alpha
sim_group_pr_mu_1<- -0.4325632
sim_group_pr_sigma_1 <- 0.9589362
sim_group_pr_rpdiff_mu_1 <- 0.02228016
sim_group_pr_rpdiff_sigma_1 <- 0.32770447

#beta
sim_group_pr_mu_2<- -1.6258576
sim_group_pr_sigma_2 <- 0.1966178
sim_group_pr_rpdiff_mu_2 <- 0.03070482
sim_group_pr_rpdiff_sigma_2 <- 0.06366926

s_n<-20

sim_alpha_s_pr_sigma<- 0.2856412 #representation of within-subject variance of alpha run averages
sim_beta_s_pr_sigma<-0.1318389 #representation of within-subject variance of beta run averages

#generate subject-level distributions for runs
sim_alpha_s_pr_mu<-rnorm(s_n,sim_group_pr_mu_1,sim_group_pr_sigma_1)
sim_beta_s_pr_mu<-rnorm(s_n,sim_group_pr_mu_2,sim_group_pr_sigma_2)
#generate subject-level rew-pun diffs
sim_alpha_s_pr_rpdiff_mu<-rnorm(s_n,sim_group_pr_rpdiff_mu_1,sim_group_pr_rpdiff_sigma_1)
sim_beta_s_pr_rpdiff_mu<-rnorm(s_n,sim_group_pr_rpdiff_mu_2,sim_group_pr_rpdiff_sigma_2)



#still need some sigmas here, but I'll come back to that...
#we have to get in the real data for this, can't be working on fakes.
#table(trials[,.(Motivation,runid,fullrunid)])
#this matches the trial data we have - first two trials are punishment; second two are reward.
rew_pun_vector<-c(0.5,0.5,-0.5,-0.5)
runs_per_subj<-length(rew_pun_vector)
sim_alpha_pr <-matrix(NA,s_n,runs_per_subj)
sim_beta_pr<-  matrix(NA,s_n,runs_per_subj)
#generate run mean and sigma
for (s in 1:s_n){
  #s=2
  #for each subject, generate the run averages....
  sim_alpha_pr[s,] <- rnorm(runs_per_subj,sim_alpha_s_pr_mu[s]+rew_pun_vector*sim_alpha_s_pr_rpdiff_mu[s],sim_alpha_s_pr_sigma)
  sim_beta_pr[s,] <- rnorm(runs_per_subj,sim_beta_s_pr_mu[s]+rew_pun_vector*sim_beta_s_pr_rpdiff_mu[s],sim_beta_s_pr_sigma)
}

#TRANSFORMS
#Group level transforms.
sim_group_mu_1<- pnorm(sim_group_pr_mu_1)
sim_group_sigma_1<- pnorm(sim_group_pr_sigma_1)*14
sim_group_rpdiff_mu_1<- pnorm(sim_group_pr_rpdiff_mu_1)
#these do come out to what we expect based on the mean posterior values....


#beta
sim_group_mu_2<-pnorm(sim_group_pr_mu_2)
sim_group_sigma_2<-pnorm(sim_group_pr_sigma_2)*14
sim_group_rpdiff_mu_2<-pnorm(sim_group_pr_rpdiff_mu_2)

#reward and punishment-specific group-level transforms.
#alpha
sim_group_mu_1_rew<-pnorm(sim_group_pr_mu_1+0.5*sim_group_pr_rpdiff_mu_1)
sim_group_mu_1_pun<-pnorm(sim_group_pr_mu_1-0.5*sim_group_pr_rpdiff_mu_1)
#and these also look like what the Bayesian model averages were.

#beta
sim_group_mu_2_rew<-pnorm(sim_group_pr_mu_2+0.5*sim_group_pr_rpdiff_mu_2)*14
sim_group_mu_2_pun<-pnorm(sim_group_pr_mu_2-0.5*sim_group_pr_rpdiff_mu_2)*14
#these check out.

#run level transforms
sim_alpha<-pnorm(sim_alpha_pr)
sim_beta<-pnorm(sim_beta_pr)*14

#alright, so, based on the data passed via cor_res, how do we expect this subject to be doing throughout the task?
#all for all subjects are the same, so we can just pick one subject who's done all four complete runs and look at their data.
#table(rawdata$subid)
#sub108 is the first subject who did all the data.
trials<-rawdata[subid==108,.(image,cor_res,runid, Motivation, blockid, first_reversal,presentation_n_after_reversal,trial_contains_reversal,presentation_n_in_segment, presentation_n_over_segments)]
trials$fullrunid<-(trials$Motivation=="punishment")*2 + trials$runid
tNum<-dim(rawdata[subid==108,])[1]

#from the model:
ev=matrix(0,100,2) ;#expected values
choice<-matrix(0,s_n,tNum)
PEnc=0; #// fictitious prediction error (PE-non-chosen)
PE=0;    #     // prediction error

#iterate through each subject
cat(paste0("iterating through ", s_n, " subjects."))
for (s in 1:s_n){#s<-1
  cat(".")
  #for each trial
  for (t in 1:tNum){ t<-1
    
    #so we're at a given trial
    #which cue is it?
    cue_id<-trials[t,image]
    # if(cue_id %in% c(27)){
    #   print("cueid in the range.")
    # }
    #which run contains this trial?
    fullrunid<-trials[t,fullrunid]
    #in this model we can't predict 0 choices.
    #choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta[s, run] );
    #using a probability between 0 and 1 representing a likelihood between 1 and 2, and returns values of 1,2.
    #rbinom(1,1,pnorm(x)) should be the right thing to do the job for this :-D
    choice[s,t] <- rbinom(1,1,pnorm(ev[cue_id,2]*sim_beta[s,fullrunid]))+1
    #print(paste0("correct response for this image in this trial is ",trials[t,cor_res],"; choice was ", choice[s,t]))
    #prediction error
    #outcome is the "actual" outcome of the choice that was made; prediction error
    #THIS COULD BE A PROBLEM. NEED TO DOUBLE CHECK THAT PE IS CALCULATED CORRECTLY AN THAT OUTCOME IS WHAT THIS IS INTERPRETING IT AS.
    #prediction error *should* be the difference between the prediction, as predicted by our EV....
    outcome=(trials[t,cor_res]==choice[s,t])*2-1 # model of outcome delivered to the subject, based on their modeled choice and the actual value
    #outcome is 1 if trial matches choice, and -1 if it does not.
    PE   =  outcome - ev[cue_id,choice[s,t]]; #difference between the modeled outcome and what the subject is modeled to expect.
    PEnc = -outcome - ev[cue_id,3-choice[s,t]];#difference between the modeled hypothetical not-outcome and what the subject expects.
    
    #Prediction error is observed minus expected;
    #so the reward outcome was 1 and we got an EV of 0 (no particular expectancy)
    #then EV for the choice is increased by 1
    #if the reward outcome was -1, and EV is 0,
    #then EV for the choice is (-1 - 0)= decreased by 1.
    #so then next time if the reward outcome was 1 and we got an EV of 1
    #then it was expected, PE is 0 and Ev does not change.
    #and if reward outcome was 1 and we got an EV of -1 (it does happen, depending on beta)
    #then prediction error is 2 and we update EV, adding 2.
    #and if reward outcome was -1 and EV was 1
    #then prediction error is (-1 - 1)=-2, and update EV, subtracting 2.
    #all this makes sense so I don't know why choices are then being pushed towards wrong choices
    
    #now update the values
    ev[cue_id,choice[s,t]] <-   ev[cue_id,choice[s,t]]    + PE    * sim_alpha[s,fullrunid]
    ev[cue_id,3-choice[s,t]] <- ev[cue_id,3-choice[s,t]]  + PEnc  * sim_alpha[s,fullrunid]
  }
}
print("")
#great. So I think that's our generative model how subjects would respond.
#Anything that could be wrong?
#(1) I'm pretty confident that it reflects the theoretical double-update reinforcement learning model we're trying to depict;
#(2) Need to double-check that "outcome"; though I'm confident that here, it does what it should, I actually don't know if the corresponding outome variable in teh stan file does this correctly.
# (3) I'm pretty confident everything else in the stan model is correct, but I could have missed something.
# (4) there might be something wrong with the stats here but more likely I've done something bad with the stats in the stan model
# (5) interpretation of "cor_res" remains a perennial issue.

#now, can we graph the choices subjects made as recorded in the choice matrix?
#need to map from trial to cue_id*presentation_n_over_segments
#because trials was simply a (presumed) sequential list of all presentations of all images
#we can just cbind choices onto trials
dim(choice)
choice.dt<-data.table(t(choice))
colnames(choice.dt)<-paste0("MS",1:s_n)

trials_modeled <- trials %>% cbind(choice.dt) %>% tidyr::gather("modeled_subid","modeled_choice",rlang::UQ(paste0("MS",1:s_n))) %>% data.table
trials_modeled[,modeled_correct:=modeled_choice==(cor_res)]
#inexplicably, modeled subjects seem to be learning the opposite of corret.
trials_modeled[,.(modeled_choice,cor_res)]
#OK. Now we can calculate % correct by 

accuracy.by.pres_seg.image<-
  trials_modeled[,.(prop.correct=sum(modeled_correct)/.N,count=.N),.(presentation_n_over_segments,Motivation,image)]
accuracy.by.pres_seg.image.finalpc<-
  accuracy.by.pres_seg.image[presentation_n_over_segments==13,.(final.prop.correct=prop.correct),.(image,Motivation)]

accuracy.by.pres_seg.image<-merge(accuracy.by.pres_seg.image,accuracy.by.pres_seg.image.finalpc,by=c("image","Motivation"))


accuracy.by.pres_seg.subid<-trials_modeled[,.(prop.correct=sum(modeled_correct)/.N,count=.N),.(presentation_n_over_segments,modeled_subid,Motivation)]

accuracy.by.pres_seg.subid.finalpc<-
  accuracy.by.pres_seg.subid[presentation_n_over_segments==13,.(final.prop.correct=prop.correct),.(modeled_subid,Motivation)]
accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by=c("modeled_subid","Motivation"))


accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
  presentation_n_over_segments<=13,
  .(prop.correct.m=mean(prop.correct),
    prop.correct.sd=sd(prop.correct)),
  .(presentation_n_over_segments,Motivation)]

main.prop.cor.ggplot<-
  ggplot(accuracy.by.pres_seg.image[!is.na(presentation_n_over_segments)],
         aes(x=presentation_n_over_segments,y=prop.correct,group=image))+
  geom_line(aes(colour=final.prop.correct),size=1.5,alpha=0.3)+ scale_colour_gradientn(colours=c("red","green","blue","violet"))+
  #scale_x_continuous(breaks=-8:4,labels=break.labels)+
  labs(#x="Presentation",
    y="Proportion correct across all users by image",
    title=paste0("proportion correct across all users by image (Modeled)\n from start to finish of reversal learning"))+
  geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
  facet_grid(Motivation ~ .)+
  #theme(strip.text.y=element_text(colour="orange"))+
  reversal_learning_timeline_ggplot_commands+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5],
             aes(yintercept = prop.correct.m),
             linetype=2)+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13],
             aes(yintercept = prop.correct.m),
             linetype=2)

#1) why are correct and incorrect swapped? this is probably a trivial error.
#2) Why does the proportion correct distribution of subjects at each point look so strange? Doesn't look like the real data somehow

main.prop.cor.ggplot
