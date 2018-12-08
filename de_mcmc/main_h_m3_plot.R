library(ggplot2)
source("de_mcmc/functions_h_m3.R")
source("visualization/geom_hdi.R")
load(paste(save.dir,"output_h_m320180411T175926",".RData",sep=""))
#load.image(paste(save.name,"run.ts",".RData",sep=""))
setwd(mainDir)

start=2
start.weights=2

pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)
setwd(mainDir)
source("de_mcmc/fig_base5.R")
hpar.names<-paste0(dimnames(phi)[[2]],"_",rep(dimnames(phi)[[3]],times=1,each=6))
phi<-matrix(phi,nrow = dim(phi)[1],ncol=prod(dim(phi)[2:3]))
n.hpars<-18
#fig_base(env = environment(),plot.phi=TRUE,plot.lower = TRUE,ask=FALSE)
fig_base( env = environment(),plot.phi=TRUE,plot.lower = FALSE,ask=FALSE,
          par.functions=rep(c(f_alpha_s_tr,f_thresh_s_tr,f_tau_s_tr),6))#list one function for each param. the second set are for the sigmas.
dev.off()

#now let's do some more plots.
additional_output<-paste(save.dir,save.name,"_extended",sep="")
dir.create(additional_output)


#cool

#now let's look at the relationships between each of the phi values.

source("plot_library.R")
res<-hyper_param(
  param.names = par.names.l2,
  group.names = l2.groups.list,
  tnmc = tnmc,
  n.chains = n.chains)

dim(res)
par(mfrow=c(3,3),ask=FALSE)
sr_diff<-res[,,,"Risky No Meth"]-res[,,,"Safe No Meth"]
hist(sr_diff[,,"alpha_mu"],breaks=100,main="Risky-Safe No Meth alpha_mu")
hist(sr_diff[,,"thresh_mu"],breaks=100,main="Risky-Safe No Meth thresh_mu")
hist(sr_diff[,,"tau_mu"],breaks=100,main="Risky-Safe No Meth tau_mu")
meth_diff<-res[,,,"Risky Meth"]-res[,,,"Risky No Meth"]
table(meth_diff[,,"alpha_mu"]<0)/prod(dim(meth_diff[,,"alpha_mu"]))


hist(meth_diff[,,"alpha_mu"],breaks=100,main="Meth-NoMeth (sexually risky) alpha_mu")
hist(meth_diff[,,"thresh_mu"],breaks=100,main="Meth-NoMeth (sexually risky) thresh_mu")
hist(meth_diff[,,"tau_mu"],breaks=100,main="Meth-NoMeth (sexually risky) tau_mu")

sr_meth_diff<-res[,,,"Risky Meth"]-res[,,,"Safe No Meth"]
hist(sr_meth_diff[,,"alpha_mu"],breaks=100,main="Risky Meth - Safe NoMeth alpha_mu")
hist(sr_meth_diff[,,"thresh_mu"],breaks=100,main="Risky Meth - Safe NoMeth thresh_mu")
hist(sr_meth_diff[,,"tau_mu"],breaks=100,main="Risky Meth - Safe NoMeth tau_mu")
dim(res[,,,"Risky No Meth"])
res3<-array(res,dim=c(prod(dim(res)[1:2]),dim(res)[3],dim(res)[4]),
            dimnames=dimnames(res)[2:4])

res3[2,,"Risky Meth"]
res[2,1,,"Risky Meth"]
res3[2,"alpha_mu",]
res[2,1,"alpha_mu",]
alpha_mu<-data.table(res3[,"alpha_mu",])
alpha_mu_graph<-gather(alpha_mu,key = "Group","alpha_mu")
ggplot(alpha_mu_graph,
       aes(x=alpha_mu,color=Group))+geom_density(size=2)+geom_hdi(alpha=0.8,size=2)+
  labs(title="standardized alpha_mu posterior estimates by group")
