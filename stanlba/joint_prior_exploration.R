source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
#so let's compare how the idfferent cholesky and sigma priors create different priors in the sigma matrix;

cholesky_priors<-c(0.25,0.5,1.0,2.0,4.0)
sigma_priors<-c(0.25,0.5,1.0,2.0,4.0)

lba_rl_version<-"joint_20180713_1"
model.subversion<-"n"
single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")

mean_sigma_matrix_list<-vector("list",length(cholesky_priors)*length(sigma_priors))
iter<-0
for (cholesky_prior in cholesky_priors){
  for(sigma_prior in sigma_priors){
    iter<-iter+1
    #cholesky_prior<-cholesky_priors[1];sigma_prior<-sigma_priors[1]
    package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_",
                             model.name,model.subversion,
                             "cp",cholesky_prior,
                             "sp",sigma_prior,".RData")
    load(package_filepath)
    
    #now extract the Sigma matrix.
    Sigma_matrix<-matrix(NA,
                         nrow=run_package$fit@par_dims$Sigma[1],
                         ncol=run_package$fit@par_dims$Sigma[2])
    for (row in 1:run_package$fit@par_dims$Sigma[1]){
      for (col in 1:run_package$fit@par_dims$Sigma[2]){
        #row<-1;col<-1
        Sigma_matrix[row,col]<-summary(run_package$fit)$summary[paste0("Sigma[",row,",",col,"]"),"mean"]
      }
    }
    mean_sigma_matrix_list[[iter]]=list("cholesky_prior"=cholesky_prior,
                                        "sigma_prior"=sigma_prior,
                                        "Sigma_matrix"=Sigma_matrix)
  }
}

heatmap_list<-vector("list",length(mean_sigma_matrix_list))
for (i in 1:length(mean_sigma_matrix_list)){
  
  heatmap_list[[i]]<-heatmap(mean_sigma_matrix_list[[i]][["Sigma_matrix"]],extra_ggitems = 
            labs(title="Prior-sampled covariance matrix",
                 subtitle=paste0("Choleksy prior:",mean_sigma_matrix_list[[i]][["cholesky_prior"]],
                                 "; sigma prior:",mean_sigma_matrix_list[[i]][["sigma_prior"]]
                                 )
                 ))
}
library(gridExtra)
library(cowplot)
plot_grid(plotlist=heatmap_list, nrow = sqrt(length(mean_sigma_matrix_list)))
