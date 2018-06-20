
source("de_mcmc/raw_data_all_runs.R")
safe_meth_subjs<-unlist(lapply(data,function(d){d$group=="SafeMeth"}))
subjs.without.group<-unlist(lapply(data,function(d){is.na(d$group)}))
data<-data[!safe_meth_subjs & !subjs.without.group]

#dim(rawdata.dt[rawdata.dt$SubjectGroup %in% c(1,2,3),])

#exclude subjects who seem to be just repeatedly pressing buttons.
changeDetect<-function(vec){sum(vec[2:length(vec)]!=vec[1:(length(vec)-1)])}
buttonChanges<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){changeDetect(r$choice)})))}))
#a more sophisticated model might have a policy detection which probabilitistically detects which policy a subject
#uses to press buttons but I'm not using that for now.
#also accuracy data will be useful.
overallperformance<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){sum(r$outcome==1)/length(r$outcome)})))}))
cbind(buttonChanges,overallperformance)
#plot(buttonChanges,overallperformance)
data<-data[buttonChanges>90 & overallperformance>0.4] 
table(unlist(lapply(data,function(d){d$group})))
#performance worse than 0.4 may suggest the subject has misunderstood the task.

#new in m5hl: consider responses less than 110 ms to be non-responses.

# #
# # # # #get a set of subjects who are in each of the three groups.
g1_subs<-which(unlist(lapply(data,function(d){d$group=="SafeNoMeth"})))
g2_subs<-which(unlist(lapply(data,function(d){d$group=="RiskyNoMeth"})))
g3_subs<-which(unlist(lapply(data,function(d){d$group=="RiskyMeth"})))
subs_to_inc<-c(g1_subs[1:5],g2_subs[1:5],g3_subs[1:5])
subs_to_inc#now what do we do with these subjects?
data<-data[subs_to_inc]
# #
rm(rawdata,rawdata.dt) #save memory.

n.chains=24

########################################## load priors from empirical data.
source("load_lba_rl_allsingles_resultsdata.R")
source("generate_lbarl_group_summary_stats.R")
improperly.estimated.runs<-unique(results.summary.dt[which(results.summary.dt$Rhat>1.05),.(sid,rid,motivation,FullRunId)])

lba_group_sstats<-generate_lbarl_group_summary_stats(results.summary.dt[!(FullRunId %in% improperly.estimated.runs$FullRunId)])
source("bootstrap_smart_init_vals.R")
smart_init_vals<-bootstrap_smart_init_vals(n_samples = n.chains,
                                           subid_set = sort(unlist(lapply(data,function(di)as.integer(gsub("SUB","",di$SubID))))),
                                           bootstrap_seed = c(1973449269))


param.l1.names=c("alpha",#"beta",
                 "thresh","tau")
param.l1.ids<-as.list(1:length(param.l1.names))
param.l1.N<-length(param.l1.names)
names(param.l1.ids)=param.l1.names
par.names=c(param.l1.names)

param.l2.names<-c(paste0(param.l1.names, "_s_mu"),paste0(param.l1.names, "_s_sigma"))
param.l2.ids<-as.list(1:length(param.l2.names))
param.l2.N<-length(param.l2.names)
names(param.l2.ids)<-param.l2.names
param.l2.distributions.N<-param.l2.N/2


#level three parameters.
#param.l3.names<-c(paste0(param.l2.names[1:(length(param.l2.names))], "_g_mu"),paste0(param.l2.names[1:(length(param.l2.names))], "_g_sigma"))
param.l3.names<-c(paste0(param.l2.names[1:3], "_g_mu"),paste0(param.l2.names[1:3], "_g_sigma"))
#Let's NOT sample subject variance from a random distribution, for now. Instead,
#we'll just assume constant run-level variance across subjects. That will halve the number of third-level parameters we have to estimate.
param.l3.ids<-as.list(1:length(param.l3.names))
param.l3.N<-length(param.l3.names)
names(param.l3.ids)<-param.l3.names
param.l3.distributions.N<-param.l3.N/2
#we'll need these separately for each group...
#not sure how to handle that just yet.

phi.ids<-as.list(1:(param.l3.N+param.l2.N))
names(phi.ids)<-c(param.l3.names,param.l2.names)

#hpar.names=par.names.l2



param.N=length(par.names) #count of raw parameters, not considering the number of subjects or the number of levels we process it on.

#n.hpars=length(hpar.names)#not sure what we do with this. Maybe best to avoid replacing it until we go through the code and see what we need to do with it.
#n.phi.mu=n.hpars/2

group_by_subject<-unlist(lapply(data,function(d){d$group})) #lists the group membership of each subject.
groups.l3.list<-unique(group_by_subject)
groups.l3.N<-length(groups.l3.list)#formerly n.l2.groups
groups.l3.ids<-as.list(1:length(groups.l3.list)) #formerly ids.l2.groups
names(groups.l3.ids)<-groups.l3.list


sname<-unlist(lapply(data,function(d){d$SubID})) #lists the group membership of each subject.
groups.l2.list<-sname
groups.l2.N<-length(sname)
groups.l2.ids<-as.list(1:length(groups.l2.list)) #formerly ids.l2.groups
names(groups.l2.ids)<-groups.l2.list

#hyper-parameters are the parameters describing the distributions from which the main model parameters are drawn
#so it seems that we aren't calculating sigmas across all subjects; if we were, they'd be hyper-parameters

#link parameters are parameters for which we're creating sigma correlations at the end.
link.pars=c() #at this stage
#link.pars=c(1:n.components, n.components+1, n.components+2, n.components+3)
unlink.pars=c(1:param.l3.distributions.N)

n.pars=param.l1.N

n.link.pars=length(link.pars)
n.unlink.pars=length(unlink.pars)

n.mu=n.link.pars
n.Sigma=n.link.pars^2

# n.delta.pars=length(delta.pars)
# n.theta.pars=length(theta.pars)
nmc=20000
#burnin=4000
burnin=16000
thin=1
keep.samples=seq(burnin,nmc,thin)
print("keep.samples overridden in order to track how the new migrate function works.")
keep.samples<-seq(10,nmc,thin)
length(keep.samples)*n.chains

migrate.prob=0.2
#migrate.duration=round(burnin*.5) + 1
migrate.duration=round(nmc*.8*.5) + 1
b=.001
gamma_numerator=0.3

S=length(data)
R_max=max(unlist(lapply(data,function(s){length(s$runs)})))#maximum number of runs for all subjects.
s_runs.N<-unlist(lapply(data,function(s){length(s$runs)}))
s_runs.MotivationType<-lapply(data,function(x){lapply(x[["runs"]],function(y){y[["motivation"]]})})

param.l1.init=array(NA,c(S,R_max, param.l1.N))#renamed x.init to param.l1.init
#initial values for alpha and thresh
for(j in 1:S){#j<-1
  for (r in 1:s_runs.N[j]){#r<-1
    param.l1.init[j,r,param.l1.ids$alpha] = lba_group_sstats$alpha_pr_mean
    param.l1.init[j,r,param.l1.ids$thresh] = lba_group_sstats$k_pr_mean
    #we're giving every single run a default tau based on the 20th percentile RT in that run.
    #had been doing min() but 5th percentile will ignore influence of outliers.
    param.l1.init[j,r,param.l1.ids$tau] = lba_group_sstats$tau_pr_mean#log(0.45*(quantile(data[[j]]$runs[[r]]$rt,c(0.05),na.rm=TRUE)[[1]]))
  }
}
