require(dplyr)
require(hBayesDM)
require(tidyr)
source(paste0(mainDir,"/visualization/geom_hdi.R"))
source(paste0(mainDir,"/plot_library.R"))
hierarchical_3l_summarize<-function(tnmc=1001){
  
  start=2
  start.weights=2
  
  #now let's look at the relationships between each of the phi values.
  
  
  res.l2l3<-hyper_param_3l(
    param.l2.names = param.l2.names,
    groups.l2.list=paste0("S",1:groups.l2.N),
    param.l3.names=param.l3.names,
    groups.l3.list=groups.l3.list,
    tnmc = tnmc,
    n.chains = n.chains)
  
  #just look at level 3 for now.
  res<-res.l2l3[["phi.g"]]
  
  dim(res)
  res.tr<-res[,,1:3,]
  res.tr[,,1,]<-f_alpha_s_tr(res.tr[,,1,])
  res.tr[,,2,]<-f_thresh_s_tr(res.tr[,,2,])
  res.tr[,,3,]<-f_tau_s_tr(res.tr[,,3,])
  par(mfrow=c(3,3),ask=FALSE)
  sr_diff<-res[,,,"RiskyNoMeth"]-res[,,,"SafeNoMeth"]
  hist(sr_diff[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth alpha_mu")
  hist(sr_diff[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth thresh_mu")
  hist(sr_diff[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth tau_mu")
  HDIofMCMC(sr_diff[,,param.l3.ids$tau_s_mu_g_mu])
  
  meth_diff<-res[,,,"RiskyMeth"]-res[,,,"RiskyNoMeth"]
  table(meth_diff[,,param.l3.ids$alpha_s_mu_g_mu]<0)/prod(dim(meth_diff[,,param.l3.ids$alpha_s_mu_g_mu]))
  hist(meth_diff[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) alpha_mu")
  HDIofMCMC(meth_diff[,,param.l3.ids$alpha_s_mu_g_mu])
  hist(meth_diff[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) thresh_mu")
  HDIofMCMC(meth_diff[,,param.l3.ids$thresh_s_mu_g_mu])
  hist(meth_diff[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) tau_mu")
  HDIofMCMC(meth_diff[,,param.l3.ids$tau_s_mu_g_mu])
  
  sr_meth_diff<-res[,,,"RiskyMeth"]-res[,,,"SafeNoMeth"]
  hist(sr_meth_diff[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth alpha_mu")
  HDIofMCMC(sr_meth_diff[,,param.l3.ids$alpha_s_mu_g_mu])
  hist(sr_meth_diff[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth thresh_mu")
  hist(sr_meth_diff[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth tau_mu")
  dim(res[,,,"RiskyNoMeth"])
  
  
  #and the transformed values
  sr_diff_tr<-res.tr[,,,"RiskyNoMeth"]-res.tr[,,,"SafeNoMeth"]
  hist(sr_diff_tr[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth alpha_mu")
  hist(sr_diff_tr[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth thresh_mu")
  hist(sr_diff_tr[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Risky-Safe No Meth tau_mu")

  meth_diff_tr<-res.tr[,,,"RiskyMeth"]-res.tr[,,,"RiskyNoMeth"]
  hist(meth_diff_tr[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) alpha_mu")
  hist(meth_diff_tr[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) thresh_mu")
  hist(meth_diff_tr[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Meth-NoMeth (sexually risky) tau_mu")

  sr_meth_diff_tr<-res.tr[,,,"RiskyMeth"]-res.tr[,,,"SafeNoMeth"]
  hist(sr_meth_diff_tr[,,param.l3.ids$alpha_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth alpha_mu")
  hist(sr_meth_diff_tr[,,param.l3.ids$thresh_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth thresh_mu")
  hist(sr_meth_diff_tr[,,param.l3.ids$tau_s_mu_g_mu],breaks=100,main="Risky Meth - Safe NoMeth tau_mu")
  
  res3<-array(res,dim=c(prod(dim(res)[1:2]),dim(res)[3],dim(res)[4]),
              dimnames=dimnames(res)[2:4])
  
  res3[2,,"RiskyMeth"]
  res[2,1,,"RiskyMeth"]
  res3[2,param.l3.ids$alpha_s_mu_g_mu,]
  res[2,1,param.l3.ids$alpha_s_mu_g_mu,]
  alpha_mu<-data.table(res3[,param.l3.ids$alpha_s_mu_g_mu,])
  alpha_mu_graph<-gather(alpha_mu,key = "Group",value="Value",SafeNoMeth,RiskyNoMeth,RiskyMeth)
  
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
    hdi.table.r<-data.frame("alpha" = HDI_format(HDIofMCMC(gc[,,param.l3.ids$alpha_s_mu_g_mu]),3),
                            "thresh" = HDI_format(HDIofMCMC(gc[,,param.l3.ids$thresh_s_mu_g_mu]),3),
                            "tau" = HDI_format(HDIofMCMC(gc[,,param.l3.ids$tau_s_mu_g_mu]),3)
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