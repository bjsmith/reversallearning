library(data.table)
dim(rl.all.subjects.list)
colnames(rl.all.subjects.list)
#alpha represents learning rate
#Q represents decision value
#delta_q represents change in decision value
#for stimulus a
#R_t is reward at time t

#this might be a big simultaneous equation - it's over-determined...well...we have different samples for each of the 
length(table(rl.all.subjects.list[,subid]))
class(rl.all.subjects.list)
View(rl.all.subjects.list[subid==101 & trial==4])
View(rl.all.subjects.list[,.(Count=.N),by=.(subid,presentation_n)][,.(Mean=mean(Count),Max=max(Count)),subid])

table(rl.all.subjects.list[subid==101,trial]

delta_for_Q_a_t<-function(R_t, Q_at_a_t){
  #alpha=learning rate
  #Q_at_a_t=decision value for stimulus a at time t
  return(R_t - Q_at_a_t)
}
update_Q_a_t_plus_1<-function(Q_at_a_t,alpha,R_t){
  stopifnot(alpha>=0 & alpha <=1)
  return(Q_at_a_t+alpha*delta_for_Q_a_t(R_t,Q_at_a_t))
}

#OK, now the iDU anti-correlation
delta_for_Q_ua_t<-function(R_t,Q_at_a_t){
  return(-R_t - Q_at_a_t)
}
update_Q_ua_t_plus_1<-function(Q_at_ua_t,kappa,alpha,R_t){
  stopifnot(kappa<=0 & kappa >=0 & alpha>=0 & alpha <=1)
  return(Q_at_au_t+kappa*alpha*delta_for_Qu_a_t(R_t,Q_at_ua_t))
}

