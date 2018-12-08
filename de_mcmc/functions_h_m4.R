#this is the main function run by the the de_joint iterator.
#it wraps the main "crossover" function
#extracts "sub" from idx[x,1], chain from idx[x,2]
#where idx is a subj*chain list where there is 1 row for every subject*chain combination
#selects the theta values and data relevant to the subject
#and mu, sigma, phi values across all.
wrap.crossover=function(x,idx,pars,use.theta,use.like,use.mu,use.Sigma,use.phi,use.data){
  sub=idx[x,1]
  chain=idx[x,2]
  
  crossover(chain,pars,
            use.theta=array(use.theta[,,sub],c(n.chains,n.pars)),
            use.like=use.like[,sub],
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            use.phi=array(use.phi[,,use.data[[idx[x,1]]]$group],c(n.chains,n.hpars)),
            use.data=use.data[[sub]]
            )
}

#transformation functions to transform variables from our estimation space into the space we need them for calculations.
f_alpha_s_tr<-function(alpha){invlogit(alpha)}
f_thresh_s_tr<-function(thresh){exp(thresh)}
f_tau_s_tr<-function(tau){exp(tau)}