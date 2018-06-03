
#main function to be called, via its wrapper function 
crossover=function(i,pars,use.theta,use.like,use.data,use.level2vars,#use.mu,use.Sigma,
                   use.phi){
  #this is a package for multi-state and hidden markov models in continuous time.
  require(msm)
  
  #get the weight to use calculated as the sum of the current likelihood
  #and the calculated density likelihood of parameters given current parameter values and hyperparameters
  use.weight=use.like[i] + log.dens.prior(x=use.theta[i,],
                                          use.level2vars=use.level2vars
                                          #use.mu=use.mu[i,],use.Sigma=use.Sigma[i,]
                                          ,use.phi=use.phi[i,])
  #from Turner et al. (2014, on de-MCMC), this is the tuning parameter for DE-MCMC, controlling the magnitude of the jumping distribution
  gamma = 2.38/sqrt(2*length(pars))
  
  #get two randomly sampled without replacement chains
  index=sample(c(1:n.chains)[-i],2,replace=F)
  
  #recalculate theta values for each parameter
  theta=use.theta[i,]    				
  #equation 3 in Turner et al. (2014). Ingenious - by using difference between two randomly chosen values
  #we can kind of bootstrap a variance in our estimation of theta
  #the tiny random noise at the end seems hacky, but what do I know?
  theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-b,b)
  theta=matrix(theta,1,length(theta))
  
  #Turner et al. (2014) notes "the stationary distribution of this chain will be the target distribution"
  #which is great - we can sample across chains to get the target distribution at the end.
  
  #define the log density likelihood based on the theta values and data, and the 
  like=log.dens.like(x=theta,use.data=use.data)
  #add the log density likelihood calculated for the model with the log density likelihood calculated for the link parameters and the hyperparameters
  #so this is quite an imporant pivot point because it combines the likelihood of the parameters based on DATA (log.dens.like) with likelihood of parameters based on HYPERS (log.dens.prior)
  weight=like + log.dens.prior(x=theta,use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
  
  
  if(is.na(weight))weight=-Inf
  #so here, we're comparing the weight of the current likelihoods ('use.like') and the density from the priors
  #with the weight from the priors and the newly calculated log density likelihood
  #and if the new weight exceeds the existing weight by more than a specified amount, we use the new one!
  if(runif(1) < exp(weight-use.weight)) {							
    use.theta[i,]=theta
    use.like[i]=like
  }
  c(use.like[i],use.theta[i,])
}


#this is the main function run by the the de_joint iterator.
#it wraps the main "crossover" function
#extracts "sub" from idx[x,1], chain from idx[x,2]
#where idx is a subj*chain list where there is 1 row for every subject*chain combination
#selects the theta values and data relevant to the subject
#and mu, sigma, phi values across all.
wrap.crossover=function(x,idx,pars,use.theta,use.like,
                        use.level2vars=use.level2vars,use.mu,use.Sigma
                        ,use.phi,use.data){
  sub=idx[x,1]
  chain=idx[x,2]
  crossover(chain,pars,
            use.theta=array(use.theta[,,sub],c(n.chains,n.pars)),
            use.like=use.like[,sub],
            use.level2vars=array(use.level2vars,c(n.chains,n.pars)),
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            use.phi=array(use.phi,c(n.chains,n.hpars)),
            use.data=use.data[[sub]])
}