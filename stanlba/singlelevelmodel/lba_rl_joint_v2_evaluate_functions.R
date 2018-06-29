library(hBayesDM)
source("visualization/geom_hdi.R")
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
do_violin<-function(grand_posterior_estimate_dt,
                    title= "Violin plot of Posterior parameter distribution for each subject across runs",
                    parameters_to_show){
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
  
  #and do a violin plot, combining runs but showing across subjects:
  ggplot(vals_trimmed,
         aes(factor(SubjectId),Value,color=Parameter))+geom_violin()+ coord_flip()+
    labs(title=title,subtitle="95% HDIs")
}
# merge(alphaktau_vals_trimmed[1:5,],alphaktau_vals_trimmed_HDIs,all.y=FALSE)
do_akt_violin<-function(grand_posterior_estimate_dt,
                               title="Posterior distribution of alpha,k, and tau values across subjects"){
  do_violin(grand_posterior_estimate_dt,title,c("alpha","k","tau"))
}






heatmap<-function(covar_matrix,label="Covariance"){#covar_matrix<-Sigma_mean
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
  covar_matrix_ordered <-covar_matrix[hc$order, hc$order]
  #reshape
  upper_tri <- get_upper_tri(covar_matrix_ordered)
  # Melt the correlation matrix
  covar_matrix_ordered_melted <- melt(upper_tri, na.rm = TRUE)
  
  
  #http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  ggheatmap <- ggplot(covar_matrix_ordered_melted, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "#bbbbbb", 
                         midpoint = 0, space = "Lab", 
                         name=label) +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = format(covar_matrix_ordered_melted$value,digits=1)), color = "black", size = 4) +
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
}
