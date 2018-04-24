hierarchical_summarize<-function(){
  
  start=2
  start.weights=2
  
  #now let's look at the relationships between each of the phi values.
  
  source("plot_library.R")
  res<-hyper_param(
    param.names = par.names.l2,
    group.names = l2.groups.list,
    tnmc = 1001,
    n.chains = n.chains)
  
  dim(res)
  par(mfrow=c(3,3),ask=FALSE)
  sr_diff<-res[,,,"Risky No Meth"]-res[,,,"Safe No Meth"]
  hist(sr_diff[,,"alpha_mu"],breaks=100,main="Risky-Safe No Meth alpha_mu")
  hist(sr_diff[,,"thresh_mu"],breaks=100,main="Risky-Safe No Meth thresh_mu")
  hist(sr_diff[,,"tau_mu"],breaks=100,main="Risky-Safe No Meth tau_mu")
  HDIofMCMC(sr_diff[,,"tau_mu"])
  
  meth_diff<-res[,,,"Risky Meth"]-res[,,,"Risky No Meth"]
  table(meth_diff[,,"alpha_mu"]<0)/prod(dim(meth_diff[,,"alpha_mu"]))
  hist(meth_diff[,,"alpha_mu"],breaks=100,main="Meth-NoMeth (sexually risky) alpha_mu")
  HDIofMCMC(meth_diff[,,"alpha_mu"])
  hist(meth_diff[,,"thresh_mu"],breaks=100,main="Meth-NoMeth (sexually risky) thresh_mu")
  HDIofMCMC(meth_diff[,,"thresh_mu"])
  hist(meth_diff[,,"tau_mu"],breaks=100,main="Meth-NoMeth (sexually risky) tau_mu")
  HDIofMCMC(meth_diff[,,"tau_mu"])
  
  sr_meth_diff<-res[,,,"Risky Meth"]-res[,,,"Safe No Meth"]
  hist(sr_meth_diff[,,"alpha_mu"],breaks=100,main="Risky Meth - Safe NoMeth alpha_mu")
  HDIofMCMC(sr_meth_diff[,,"alpha_mu"])
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
  
  group_plot=ggplot(alpha_mu_graph,
         aes(x=alpha_mu,color=Group))+geom_density(size=2)+geom_hdi(alpha=0.8,size=2)+
    labs(title="standardized alpha_mu posterior estimates by group")
  #print a table of the 95% HDIs
  #columns are parameters; rows are group comparisons
  hdi.table<-NULL
  group.comparisons<-list("sr_diff"=sr_diff,"meth_diff"=meth_diff,"sr_meth_diff"=sr_meth_diff)
  
  HDI_format<-function(hdi_pair,dp=NA){
    if(is.na(dp)){
      return(paste0("[",hdi_pair[1],", ",hdi_pair[2],"]"))
    }else{
      return(paste0("[",format(hdi_pair[1],digits=dp,nsmall=dp,scientific=-2),", ",format(hdi_pair[2],digits=dp,nsmall=dp,scientific=-2),"]"))
    }
    
  }
  for (gc_name in names(group.comparisons)){
    gc=group.comparisons[[gc_name]]
    hdi.table.r<-data.frame("alpha_mu" = HDI_format(HDIofMCMC(gc[,,"alpha_mu"]),3),
                            "thresh_mu" = HDI_format(HDIofMCMC(gc[,,"thresh_mu"]),3),
                            "tau_mu" = HDI_format(HDIofMCMC(gc[,,"tau_mu"]),3)
                            )
    rownames(hdi.table.r)[1]<-gc_name
    if (is.null(hdi.table)){
      hdi.table <- hdi.table.r
    }else{
      hdi.table<-rbind(hdi.table,hdi.table.r)
    }
    
  }
  return(list("HDI_table"=hdi.table,"group_plot"=group_plot))
  
  
}