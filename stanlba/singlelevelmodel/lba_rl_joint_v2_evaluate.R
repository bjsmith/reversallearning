source("stanlba/singlelevelmodel/lba_rl_joint_v2_summarize.R")
source("visualization/geom_hdi.R")

#first, let's get us our estimate across all subjects in the sample.
#rather than using this summary, we want to get a huge matrix of all the posteriors.
#OK. Let's just grab the data we're interested in....
grand_posterior_estimate_neural_dt<-data.table(grand_posterior_estimate)

#OK, what are our variables of interest here?
#I guess we want to produce several graphics for a report, including the posteriors for parameters, and a representation of what this subject actually did???

#first, let's get these estimates into a format I can read more easily.
#we're interested in:
#alpha, k, tau, Sigma
#and particularly specific parts of sigma.

#this is currently just coded to display a single subject. I think that's all we want for this one.

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

DeltaThetaLabels=c("RewardPredictionError","AccubmensL","AccubmensR","IFGOrbitalL","IFGOrbitalR")

#summary(run_package)


Sigma_dims=5
# heatmap(Sigma_mean,label="Covariance")
# heatmap(cov2cor(Sigma_mean),label="Correlation")

rpe_correlations<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
#OK  but we are interested in specifically the relationship between reward prediction error and each of the other values. Can we pull that out?
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

library(dplyr)
rpe_correlations_post<-paste0("Sigma.",1:Sigma_dims,".",1,".")
rpe_posteriors_dt<-grand_posterior_estimate_neural_dt[,rpe_correlations_post,with=FALSE]
colnames(rpe_posteriors_dt)<-DeltaThetaLabels
group_posterior_rpe.long<- rpe_posteriors_dt %>% tidyr::gather("Parameter","Value",1:5)
ggplot(group_posterior_rpe.long,aes(Value))+
  geom_histogram(bins = 200)+
  geom_hdi(color="#aaaaff",size=2,lineend="round")+
  facet_wrap(~Parameter,nrow = 2,scales = "free")+
  labs(title="Posterior distribution of RewardPredictionError covariance across subjects")


#plot the alpha, k, tau values.

do_akt_distribution<-function(grand_posterior_estimate_dt,
                              title="Posterior distribution of alpha,k, and tau values across subjects"){
  return(ggplot(grand_posterior_estimate_dt[,.(alpha,k,tau)]%>% tidyr::gather("Parameter","Value",1:3),aes(Value))+geom_histogram(bins = 200)+geom_hdi(color="#aaaaff",size=2,lineend="round")+facet_wrap(~Parameter,nrow = 2,scales = "free")+labs(title=title))
}
# merge(alphaktau_vals_trimmed[1:5,],alphaktau_vals_trimmed_HDIs,all.y=FALSE)
do_alpha_k_tau_graph<-function(grand_posterior_estimate_dt,
                               title="Posterior distribution of alpha,k, and tau values across subjects"){
  #what if we just show the 95% HDIs?
  #might be a better way to do this but easiest just to trim the data itself.
  alphaktau_vals_trimmed<-data.table(grand_posterior_estimate_dt[,.(alpha,k,tau,SubjectId)]%>% 
    tidyr::gather("Parameter","Value",1:3))
  #alphaktau_vals_trimmed[,SubjectParameterCombo:=paste0(SubjectId,Parameter)]
  alphaktau_vals_trimmed_HDIs<-alphaktau_vals_trimmed[,.(Lower=HDIofMCMC(Value)[1],
                                                         Upper=HDIofMCMC(Value)[2]),by=.(SubjectId,Parameter)]
  
  alphaktau_vals_trimmed<-merge(alphaktau_vals_trimmed,alphaktau_vals_trimmed_HDIs,
                                by=c("SubjectId","Parameter"))
  #then if the value is outside the HDI range, exclude the row
  alphaktau_vals_trimmed<-alphaktau_vals_trimmed[Value>=Lower & Value<=Upper,]
  #then plot it. 
  
  #and do a violin plot, combining runs but showing across subjects:
  print(ggplot(alphaktau_vals_trimmed,
         aes(factor(SubjectId),Value,color=Parameter))+geom_violin()+ coord_flip()+
    labs(title=title,subtitle="95% HDIs"))
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

#several summary statistics we could use here:

# - take point estimate of RPE covariance within subject, and then plot the average across subjects.
# - add posterior across all runs and subjects and present that. I *think* this is valid; it should 'control itself'.
do_alpha_k_tau_graph(grand_posterior_estimate_neural_dt,title="Posterior distribution of alpha,k, and tau values across subjects (Neural-behavioral model)")
do_akt_distribution(grand_posterior_estimate_neural_dt,title="Posterior distribution of alpha,k, and tau values across subjects (Neural-behavioral model)")

#Because the alpha learning rate is so low, I wonder if something is wrong here. We weren't getting learning rates this low before.
#What do they look like in the behavioral model?
load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/20180624_1/posteriors_summary_v2_20180624_1.RData")
grand_posterior_estimate_dt_behavioral<-data.table(grand_posterior_estimate)
do_akt_distribution(grand_posterior_estimate_dt_behavioral,title="Posterior distribution of alpha,k, and tau values across subjects (behavioral-only model)")
do_alpha_k_tau_graph(grand_posterior_estimate_dt_behavioral,title="Posterior distribution of alpha,k, and tau values across subjects (behavioral-only model)")