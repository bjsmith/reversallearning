trial_n_back=3
recent_cue_familiarity = c(2,5,7)-1
current_cue_familiarity=3-1
recent_cue_discriminability=recent_cue_familiarity+current_cue_familiarity
cue_match= c(FALSE,TRUE,FALSE)

discrim_learning_rate=0.2
match_exp_probability=inv.logit((cue_match*2-1)*recent_cue_discriminability*discrim_learning_rate)
match_exp_probability

discrim_function=function(trial_n_back,recent_cue_n,current_cue_n,cue_match,discrim_learning_rate){
  recent_cue_familiarity = recent_cue_n-1
  current_cue_familiarity=current_cue_n-1
  recent_cue_discriminability=recent_cue_familiarity+current_cue_familiarity

  #match_exp_probability=inv.logit((cue_match*2-1)*recent_cue_discriminability*discrim_learning_rate)
  match_exp_probability=inv.logit((cue_match*2-1)*recent_cue_discriminability*discrim_learning_rate)
  match_exp_probability/sum(match_exp_probability)
}

discrim_function(3,c(2,5,7),3,c(FALSE,TRUE,FALSE),0.2)

discrim_function(3,c(1,2,1),3,c(FALSE,TRUE,FALSE),0.2)
discrim_function(3,c(2,2,2),3,c(FALSE,TRUE,FALSE),0.2)
discrim_function(3,c(2,5,2),3,c(FALSE,TRUE,FALSE),0.2)
discrim_function(3,c(2,5,5),3,c(FALSE,TRUE,FALSE),0.2)
discrim_function(3,c(2,5,5),3,c(FALSE,TRUE,FALSE),0.3)
discrim_function(3,c(2,5,5),3,c(FALSE,FALSE,FALSE),0.3)
