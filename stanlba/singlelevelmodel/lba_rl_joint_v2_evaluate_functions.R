library(hBayesDM)
source("visualization/geom_hdi.R")
DeltaThetaLabels=c("RewardPredictionError","AccubmensL","AccubmensR","IFGOrbitalL","IFGOrbitalR")
get_sigma_array<- function(fit,DeltaThetaLabels){#fit<-run_package$fit
  Sigma_dims<-fit@par_dims$Sigma
  
  stopifnot(Sigma_dims[1]==Sigma_dims[2])
  Sigma_vals<-summary(fit)$summary[paste0("Sigma[",rep(1:Sigma_dims[1],times=Sigma_dims[2]),",",rep(1:Sigma_dims[2],each=Sigma_dims[1]),"]"),]
  Sigma_mean_vec<-Sigma_vals[,"mean"]
  Sigma_mean<-matrix(Sigma_mean_vec,nrow = Sigma_dims[1])
  #now label them!
  rownames(Sigma_mean)<-DeltaThetaLabels
  colnames(Sigma_mean)<-DeltaThetaLabels
  return(Sigma_mean)
}

Sigma_dims=5
rpe_correlations<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
#that is going to be DeltaTheta[1], 
show_fit<-function(fit){
  summary(fit)$summary[rpe_correlations,]
  #and let's graph this.
  fit_array<-as.matrix(fit)
  #dim(fit_array)
  rpe_samples<-fit_array[,rpe_correlations]
  #right
  colnames(rpe_samples)<-DeltaThetaLabels
  #and now shape into a format we can graph in ggplot
  rpe_samples_dt<-tidyr::gather(data.table(rpe_samples),"Parameter","Value",1:5)
  ggplot(rpe_samples_dt,aes(Value))+geom_histogram()+geom_hdi(color="#aaaaff",size=2,lineend="round")+facet_wrap(~Parameter,nrow = 2)+labs(title="Posterior distribution of RewardPredictionError covariance")

}

do_param_distribution<-function(grand_posterior_estimate_dt,
                              title="Posterior distribution of alpha,k, and tau values across subjects",
                              parameters_to_show){
  ggplot(grand_posterior_estimate_dt[,parameters_to_show,with=FALSE]%>% tidyr::gather("Parameter","Value",1:length(parameters_to_show)),aes(Value))+geom_histogram(bins = 200)+geom_hdi(color="#aaaaff",size=2,lineend="round")+facet_wrap(~Parameter,nrow = 2,scales = "free")+labs(title=title)
}

do_akt_distribution<-function(grand_posterior_estimate_dt,
                              title="Posterior distribution of alpha,k, and tau values across subjects"){
  ggplot(grand_posterior_estimate_dt[,.(alpha,k,tau)]%>% tidyr::gather("Parameter","Value",1:3),aes(Value))+geom_histogram(bins = 200)+geom_hdi(color="#aaaaff",size=2,lineend="round")+facet_wrap(~Parameter,nrow = 2,scales = "free")+labs(title=title)
}

do_alpha_k_tau_graph<-function(grand_posterior_estimate_dt,title="Posterior distribution of alpha,k, and tau values across subjects"){
  warning("do_alpha_k_tau_graph is deprecated in favor of the more-accurately-named function do_akt_violin")
  do_akt_violin(grand_posterior_estimate_dt,title)
}

do_violin_sid<-function(grand_posterior_estimate_dt,
                    title= "Violin plot of Posterior parameter distribution for each subject across runs",
                    parameters_to_show){#grand_posterior_estimate_dt<-data.table(parallel_rl_model_v4[["complete_posterior_list"]]);parameters_to_show<-c("alpha","beta")
  #detect the scale that each parameter is on: 0 to 1 or outside that range, then show on separate graphs.
  #what if we just show the 95% HDIs?
  #might be a better way to do this but easiest just to trim the data itself.
  vals_trimmed<-data.table(grand_posterior_estimate_dt[,c(parameters_to_show,"SubjectId"),with=FALSE]%>% 
                             tidyr::gather("Parameter","Value",1:length(parameters_to_show)))
  #vals_trimmed[,SubjectParameterCombo:=paste0(SubjectId,Parameter)]
  vals_trimmed_HDIs<-vals_trimmed[,.(Lower=HDIofMCMC(Value)[1],
                                     Upper=HDIofMCMC(Value)[2]),by=.(SubjectId,Parameter)]
  
  vals_trimmed<-merge(vals_trimmed,vals_trimmed_HDIs,
                      by=c("SubjectId","Parameter"))
  #then if the value is outside the HDI range, exclude the row
  vals_trimmed<-vals_trimmed[Value>=Lower & Value<=Upper,]
  #then plot it. 
  vals_trimmed$ParamCategory<-as.character(NA)
  #detect the range of each parameter.
  for (p in parameters_to_show){#p<-"alpha"
    if(min(grand_posterior_estimate_dt[,p,with=FALSE])>=0 & max(grand_posterior_estimate_dt[,p,with=FALSE])<=1){
      vals_trimmed[Parameter==p, ParamCategory:="Unit"]
    }else if(min(grand_posterior_estimate_dt[,p,with=FALSE])>=0){
      vals_trimmed[Parameter==p, ParamCategory:="Positive"]
    }else{
      vals_trimmed[Parameter==p, ParamCategory:="Infinite"]
    }
  }
  table(vals_trimmed$ParamCategory)
  
  #and do a violin plot, combining runs but showing across subjects:
  ggplot(vals_trimmed,
         aes(factor(SubjectId),Value,color=Parameter))+geom_violin()+ coord_flip()+
    labs(title=title,subtitle="95% HDIs")+facet_grid(~ParamCategory,scales = "free")+
    theme(strip.text.x = element_blank(), strip.background = element_blank(),legend.position="bottom")
}


do_violin_runs<-function(grand_posterior_estimate_dt,
                    title= "Violin plot of Posterior parameter distribution for each subject across runs",
                    parameters_to_show){#grand_posterior_estimate_dt<-data.table(parallel_rl_model_v4[["complete_posterior_list"]]);parameters_to_show<-c("alpha","beta")
  #detect the scale that each parameter is on: 0 to 1 or outside that range, then show on separate graphs.
  #what if we just show the 95% HDIs?
  #might be a better way to do this but easiest just to trim the data itself.
  vals_trimmed<-data.table(grand_posterior_estimate_dt[,c(parameters_to_show,"UniqueRunCode","SubjectId"),with=FALSE]%>% 
                             tidyr::gather("Parameter","Value",1:length(parameters_to_show)))
  #vals_trimmed[,SubjectParameterCombo:=paste0(SubjectId,Parameter)]
  vals_trimmed_HDIs<-vals_trimmed[,.(Lower=HDIofMCMC(Value)[1],
                                     Upper=HDIofMCMC(Value)[2]),by=.(UniqueRunCode,Parameter,SubjectId)]
  
  vals_trimmed<-merge(vals_trimmed,vals_trimmed_HDIs,
                      by=c("UniqueRunCode","Parameter","SubjectId"))
  #then if the value is outside the HDI range, exclude the row
  vals_trimmed<-vals_trimmed[Value>=Lower & Value<=Upper,]
  #then plot it. 
  vals_trimmed$ParamCategory<-as.character(NA)
  #detect the range of each parameter.
  for (p in parameters_to_show){#p<-"alpha"
    if(min(grand_posterior_estimate_dt[,p,with=FALSE])>=0 & max(grand_posterior_estimate_dt[,p,with=FALSE])<=1){
      vals_trimmed[Parameter==p, ParamCategory:="Unit"]
    }else if(min(grand_posterior_estimate_dt[,p,with=FALSE])>=0){
      vals_trimmed[Parameter==p, ParamCategory:="Positive"]
    }else{
      vals_trimmed[Parameter==p, ParamCategory:="Infinite"]
    }
  }
  table(vals_trimmed$ParamCategory)
  
  #and do a violin plot, combining runs but showing across subjects:
  ggplot(vals_trimmed,
         aes(factor(UniqueRunCode),Value,color=Parameter))+geom_violin()+ coord_flip()+
    labs(title=title,subtitle="95% HDIs")+facet_grid(SubjectId~ParamCategory,scales = "free")+
    theme(strip.text.x = element_blank())
}

# merge(alphaktau_vals_trimmed[1:5,],alphaktau_vals_trimmed_HDIs,all.y=FALSE)
do_akt_violin<-function(grand_posterior_estimate_dt,
                               title="Posterior distribution of alpha,k, and tau values across subjects"){
  do_violin(grand_posterior_estimate_dt,title,c("alpha","k","tau"))
}






heatmap<-function(covar_matrix,label="Covariance",show_labels=TRUE,extra_ggitems=NULL,order=TRUE,labelsize=2,scale_midpoint=0){#covar_matrix<-Sigma_mean
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  #reorder
  dd <- as.dist((1-covar_matrix)/2)
  hc <- hclust(dd)
  if(order){
    covar_matrix_ordered <-covar_matrix[hc$order, hc$order]
  }else{
    covar_matrix_ordered <-covar_matrix
  }
  
  #reshape
  upper_tri <- get_upper_tri(covar_matrix_ordered)
  # Melt the correlation matrix
  covar_matrix_ordered_melted <- melt(upper_tri, na.rm = TRUE)
  
  
  #http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  ggheatmap <- ggplot(covar_matrix_ordered_melted, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "#bbbbbb", 
                         midpoint = scale_midpoint, space = "Lab", 
                         name=label) +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  if(show_labels){
    ggheatmap = ggheatmap + geom_text(aes(Var2, Var1, label = round(covar_matrix_ordered_melted$value,digits=2)), color = "black", size = labelsize,fontface="bold")
  }
  
  ggheatmap = ggheatmap + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  if(!is.null(extra_ggitems)){
    ggheatmap = ggheatmap+extra_ggitems
  }
  ggheatmap
}


show_distribution_of_all_sigmas<-function(
  gpe,
  Sigma_dims,
  ggplot_args=NULL){
  #gpe<-data.table(model_lba_rl_joint_v7[["complete_posterior_list"]])
  #if(is.null(correlations_post)){
  if(Sigma_dims[1]>1 & Sigma_dims[2]==1){
    correlations_post<-paste0("Sigma.",1,".",rep(1:Sigma_dims[1]),".")
  }else if (Sigma_dims[1]>1 & Sigma_dims[2]>1){
    correlations_post<-paste0("Sigma.",rep(1:Sigma_dims[1],times=Sigma_dims[2]),".",rep(1:Sigma_dims[2],each=Sigma_dims[1]),".")
    if(Sigma_dims[1]!=Sigma_dims[2]){
      stop("this configuration hasn't been tested")
    }
  }
  
  #}
  
  posteriors_dt<-gpe[,correlations_post,with=FALSE]
  colnames(posteriors_dt)<-paste0(rep(DeltaThetaLabels[1:Sigma_dims[1]],times=Sigma_dims[2])," x ",
                                  rep(DeltaThetaLabels[1:Sigma_dims[2]],each=Sigma_dims[1]))
  group_posterior_rpe.long<- posteriors_dt %>% tidyr::gather("Parameter","Value",1:Sigma_dims[1]*Sigma_dims[2])
  ggplot(group_posterior_rpe.long,aes(Value))+
    geom_histogram(bins = 200)+
    geom_hdi(color="#aaaaff",size=1)+
    facet_wrap(~Parameter,nrow = 5,scales = "free")+
    labs(title="Posterior distribution of covariance matrix across subjects")+ggplot_args
  
}

show_distribution_of_all_sigmas_new<-function(
  gpe,
  Sigma_dims,
  ggplot_args=NULL){
  #gpe<-data.table(model_lba_rl_joint_v7[["complete_posterior_list"]])
  #if(is.null(correlations_post)){
  if(Sigma_dims[1]>1 & Sigma_dims[2]==1){
    correlations_post<-paste0("Sigma[",1,",",rep(1:Sigma_dims[1]),"]")
  }else if (Sigma_dims[1]>1 & Sigma_dims[2]>1){
    correlations_post<-paste0("Sigma[",rep(1:Sigma_dims[1],times=Sigma_dims[2]),",",rep(1:Sigma_dims[2],each=Sigma_dims[1]),"]")
    if(Sigma_dims[1]!=Sigma_dims[2]){
      stop("this configuration hasn't been tested")
    }
  }
  
  #}
  
  posteriors_dt<-gpe[,correlations_post,with=FALSE]
  colnames(posteriors_dt)<-paste0(rep(DeltaThetaLabels[1:Sigma_dims[1]],times=Sigma_dims[2])," x ",
                                  rep(DeltaThetaLabels[1:Sigma_dims[2]],each=Sigma_dims[1]))
  group_posterior_rpe.long<- posteriors_dt %>% tidyr::gather("Parameter","Value",1:Sigma_dims[1]*Sigma_dims[2])
  ggplot(group_posterior_rpe.long,aes(Value))+
    geom_histogram(bins = 200)+
    geom_hdi(color="#aaaaff",size=1)+
    facet_wrap(~Parameter,nrow = 5,scales = "free")+
    labs(title="Posterior distribution of covariance matrix across subjects")+ggplot_args
  
}

