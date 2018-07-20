source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

#JOINT AND PARALLEL MODELS WITH BIASED ALPHA AND BROKEN! BETA PRIORS

joint_rl_model<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180628_1",
  model_filename="rl_single_exp_joint",
  model_subversion="_joint_v1")

parallel_rl_model<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180628_1",
  model_filename="rl_single_exp_parallel",
  model_subversion="_joint_v1")

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA AND BROKEN! BETA PRIORS AND CHOLESKY(4)
#Shouldn't make a difference to Alpha and Beta because this is a 'parallel' model.

parallel_rl_model2<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_parallel_v2",
  model_subversion="")



#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA AND BROKEN! BETA PRIORS AND CHOLESKY(1)
#ALPHA=N(0,1.8);BETA=N(0,1.8);cholesky(1);
joint_rl_model2<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v2",
  model_subversion="")

parallel_rl_model_v3<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_parallel_v3",
  model_subversion="")

#JOINT MODELS WITH MODERATE ALPHA AND BROKEN! BETA PRIORS AND CHOLESKY(1) 

joint_rl_model3<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v3",
  model_subversion="")

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA AND BETTER BETA PRIORS AND CHOLESKY(1)
parallel_rl_model_v4<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_parallel_v4",
  model_subversion="")

#OK now, let's check out the charts of their parameter distributions.
joint_rl_model4<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v4",
  model_subversion="")

joint_rl_model5<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v5",
  model_subversion="")

#JOINT MODELS WITH STANDARDIZED COVARIANCE MATRIX.
# 
joint_rl_model6<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v6",
  model_subversion="")

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA, K, TAU PRIORS AND CHOLESKY(1)
joint_lba_rl_model3<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="lba_rl_single_exp_joint_v3",
  model_subversion="")
#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA, K, TAU PRIORS AND CHOLESKY(1)
joint_lba_rl_model5<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180701_1",
  model_filename="lba_rl_single_exp_joint_v5",
  model_subversion="")



############# OUTLYING-BIASED PRIORS: JOINT AND PARALLEL COMPARISON.

#Is there a difference between joint and parallel models with these poor priors?
do_violin(data.table(joint_rl_model[["complete_posterior_list"]]),parameters_to_show=c("alpha","beta"),title="Parameter distribution for each subject across runs\n Joint model, outlying priors")
  #: completely uninformative beta. alphas often indistinguishable from zero.  
do_violin(data.table(parallel_rl_model[["complete_posterior_list"]]),parameters_to_show=c("alpha","beta"),title="Parameter distribution for each subject across runs\n PARALLEL model, outlying priors")
  #: alphas obviously biased too high. betas that are also high but biased.
#The parallel model DOES consider the covariance of the neural parameters with each other.
#literally the only difference is adding the RPE into the covariance matrix.
#so any difference between these two really reflects the addition of the reward prediction error to the deltatheta covariance matrix.
#we need to consider several priors that might be constraining the model to have different alpha and beta values.
# - covariance matrix priors; covariance of RPE with neural values
# - covariance matrix priors; covariance of RPE with itself.
# - mean value for RPE Theta

#so this should be considered a warning, 
#but really I'm not too interested because 
#the alpha and beta priors are so fucked there's no point in trying to make sense of these.
#but we are going to have to take a look at another joint vs. parallel comparison 
#to see what happens with more reason with more reasonable priors.
#may also want to carefully consider that cholesky prior.

########### MODERATE PRIORS (ALPHA, BETA)~N(0,1.8), CHOLESKY(4)
#: completely uninformative beta. alphas often indistinguishable from zero.
#Now, this should tell us what difference the change in alpha/beta parameters make, independent of changing the cholesky distribution.

do_violin(data.table(parallel_rl_model2[["complete_posterior_list"]]),
          parameters_to_show=c("alpha","beta"),
          title="Parameter distribution for each subject across runs\n PARALLEL model, Alpha,beta~N(0,1.8), CHOL(4)")
#this is a slight improvement. 

########### MODERATE PRIORS (ALPHA, BETA)~N(0,1.8), CHOLESKY(1)

do_violin(data.table(parallel_rl_model_v3[["complete_posterior_list"]]),
          parameters_to_show=c("alpha","beta"),
          title="Parameter distribution for each subject across runs\n PARALLEL model, Alpha,beta~N(0,1.8), CHOL(1)")
#this is virtually identical as the previous model. This is no surprise because neither model connect to the joint distribution.


do_violin(data.table(joint_rl_model2[["complete_posterior_list"]]),
          parameters_to_show=c("alpha","beta"),
          title="Parameter distribution for each subject across runs\n JOINT model, Alpha,beta~N(0,0.8), CHOL(1)")
# it's really dramatic how the learning rate crashes in this version of the model.

########### MODERATE PRIORS (ALPHA)~N(0,1.8) BETA~EXP(N(0,6)) CHOLESKY(1)
do_violin_sid(data.table(parallel_rl_model_v4[["complete_posterior_list"]]),
          parameters_to_show=c("alpha","beta"),
          title="Parameter distribution for each subject across runs\n PARALLEL model, Alpha,beta~N(0,1.8), CHOL(1)")

do_violin_sid(data.table(joint_rl_model4[["complete_posterior_list"]]),
               parameters_to_show=c("alpha","beta"),
               title="Parameter distribution for each subject across runs\n JOINT model, Alpha,beta~N(0,1.8), CHOL(1)")

########### MODERATE PRIORS (ALPHA)~N(0,1.5) BETA~EXP(N(0,3)) CHOLESKY(1)

do_violin_sid(data.table(joint_rl_model5[["complete_posterior_list"]]),
              parameters_to_show=c("alpha","beta"),
              title="Parameter distribution for each subject across runs\n JOINT model, Alpha,beta~N(0,1.8), CHOL(1)")


show_distribution_of_rpe_sigmas<-function(gpe){
  rpe_correlations_post<-paste0("Sigma.",1:Sigma_dims,".",1,".")
  rpe_posteriors_dt<-gpe[,rpe_correlations_post,with=FALSE]
  colnames(rpe_posteriors_dt)<-DeltaThetaLabels
  group_posterior_rpe.long<- rpe_posteriors_dt %>% tidyr::gather("Parameter","Value",1:5)
  ggplot(group_posterior_rpe.long,aes(Value))+
    geom_histogram(bins = 200)+
    geom_hdi(color="#aaaaff",size=2,lineend="round")+
    facet_wrap(~Parameter,nrow = 2,scales = "free")+
    labs(title="Posterior distribution of RewardPredictionError covariance across subjects")
  
}



show_distribution_of_all_sigmas(data.table(joint_rl_model5[["complete_posterior_list"]]))


get_sigma_array_from_post_dist(joint_rl_model5$complete_posterior_list,
                               Sigma_dims = c(5,5),
                               DeltaThetaLabels=DeltaThetaLabels
                              )


########### STANDARDIZED ALPHA COVARIANCE

do_violin_runs(data.table(joint_rl_model6[["complete_posterior_list"]]),
              parameters_to_show=c("alpha","beta"),
              title="Parameter distribution for each subject across runs\n JOINT model, Alpha~logit(N(0,1.5)),beta~exp(N(0,3)), CHOL(1)")


show_distribution_of_all_sigmas(data.table(joint_rl_model6$complete_posterior_list),
                                Sigma_dims = c(5,5))
#what if we examined the point estimates of covariance averaged for each subject?
#this should be a conservative method.
rpe_covariances_post<-paste0("Sigma.",1:Sigma_dims,".",1,".")
rpe_covariances_post<-paste0("Sigma[",1:Sigma_dims,",",1,"]")
rpe_correlations_post<-paste0("L_Omega[",1:Sigma_dims,",",1,"]")

res.by.sub<-data.table(joint_rl_model6$results_summary)[param_name %in% rpe_covariances_post,] %>%
  #subject means
  .[,.(MeanEstimateMeanAcrossRuns=mean(mean)),by=.(sid,param_name)]

res.by.sub<-data.table(joint_rl_model6$results_summary)[param_name %in% rpe_correlations_post,] %>%
  #subject means
  .[,.(MeanEstimateMeanAcrossRuns=mean(mean)),by=.(sid,param_name)]


#this should be RPE*RPE
t.test(res.by.sub[param_name=="Sigma[1,1]",MeanEstimateMeanAcrossRuns])
#L+R Accumbens
t.test(res.by.sub[param_name=="Sigma[2,1]",MeanEstimateMeanAcrossRuns])
t.test(res.by.sub[param_name=="Sigma[3,1]",MeanEstimateMeanAcrossRuns])
#L+R OFC
t.test(res.by.sub[param_name=="Sigma[4,1]",MeanEstimateMeanAcrossRuns])
t.test(res.by.sub[param_name=="Sigma[5,1]",MeanEstimateMeanAcrossRuns])

load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/rl_joint_20180629_1/run_package_106_1_punishment_rl_single_exp_joint_v6.RData")
library(rstan)
cat(run_package$fit@stanmodel@model_code)
lapply(split(res.by.sub$MeanEstimateMeanAcrossRuns,as.character(res.by.run$param_name)),t.test)
data.table(joint_rl_model6$results_summary)[param_name =="Sigma[1,2]"]

#let's take a look at alpha k tau model.

do_violin_sid(data.table(joint_rl_model6[["complete_posterior_list"]]),
              parameters_to_show=c("alpha","beta"),
              title="Parameter distribution for each subject across runs\n JOINT model, Alpha~N(0,1.5),beta, CHOL(1)")

do_violin_sid(data.table(joint_lba_rl_model5[["complete_posterior_list"]]),
          parameters_to_show=c("alpha","k","tau"),
          title="Parameter distribution for each subject across runs\n JOINT model, Alpha~N(0,1.5),k,tau, CHOL(1)")

#so: 
# - going from a alpha-k-tau version to a alpha-beta version cuts down learning rate considerably.
#   - we need to ask: does the alpha-k-tau version allow enough room for error during the ballistic accumulation stage?
# - Going from the alpha-beta joint model to the alpha-beta parallel version ALSO cuts down learning rates considerably. Seems like constraining reward prediction error to possibly covary with RPE makes a difference?
#
