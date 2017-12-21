source("visualization/geom_hdi.R")
source("diagnostics.R")

#this file follows on from du_model_revised_2aa_4runs_mcmc_firstniterations.R
for (i in 1:length(model.summaries)){
  if(model.summaries[[i]]$m=="double_update_nov_rev2-a-a" & model.summaries[[i]]$g==1
  ){
    print("got it")
    m.g1.mcmc<-model.summaries[[i]]
    m.sf.g1.mcmc<-model.stanfits[[i]]
    m.sf.g2.mcmc<-model.stanfits[[i+1]]
  }
}



for (i in 1:length(model.summaries)){
  if(model.summaries[[i]]$m=="double_update_nov_rev2-a-a"# & model.summaries[[i]]$g==1
  ){
    representativeness_stats(model.summaries[[i]],model.stanfits[[i]],c("mu_p[1]","mu_p[2]"))
  }
}



show.dist.by.chain(m.sf.g1.mcmc,"mu_p[1]")
show.dist.by.chain(m.sf.g2.mcmc,"mu_p[1]")
show.dist.by.chain(m.sf.g1.mcmc,"mu_p[2]")

show.dist.by.chain(m.sf.g2.mcmc,"mu_p[2]")
#somehow the model simply didn't converge for Group 1. several chains got stuck in very small value ranges.
#we're moving on to another configuration anyway....