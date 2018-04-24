#this is the main function run by the the de_joint iterator.
#it wraps the main "crossover" function
#extracts "sub" from idx[x,1], chain from idx[x,2]
#where idx is a subj*chain list where there is 1 row for every subject*chain combination
#selects the theta values and data relevant to the subject
#and mu, sigma, phi values across all.
#for the three-level model, we should make idx a subj*run*chain list.
wrap.crossover=function(x,idx,pars,use.theta,use.like,use.mu,use.Sigma,use.phi_s,use.phi_g,use.data){
  sub=idx[x,1]
  run=idx[x,2]
  chain=idx[x,3]
  #to modify this for our three-level model we're to somehow run this for subject and run
  #I believe it still makes sense to run this separately for each subject and run.
  crossover(chain,pars,
            use.theta=array(use.theta[,,sub,run],c(n.chains,n.pars)),
            use.like=use.like[,sub,run],
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            #we're concatenating the phi vars at different levels. I *think* this is OK...
            use.phi=array(cbind(use.phi_g[,,use.data[[idx[x,1]]]$group],use.phi_s[,,use.data[[idx[x,1]]]$SubID]),
                          c(n.chains,param.l3.N+param.l2.N)),
            use.data=use.data[[sub,run]]
            )
}


#transformation functions to transform variables from our estimation space into the space we need them for calculations.
f_alpha_s_tr<-function(alpha){invlogit(alpha)}
f_thresh_s_tr<-function(thresh){exp(thresh)}
f_tau_s_tr<-function(tau){exp(tau)}


