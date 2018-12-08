---
title: "MCMC Analysis Final Report"
output:
  html_document:
    keep_md: yes
    self_contained: no
  html_notebook: default
  pdf_document:
    keep_tex: yes
---


## Introduction

In this chapter, I describe a Differential Evolution MCMC model.

Unfortunately, I was unable to build a three-level Differential Evolution MCMC model. The attempt to build this will be discussed in another section. However, I did implement a two-level Differential Evolution MCMC model. In the dataset, all subjects took part in four runs. Thus, I was able to implement a two-level model by analyzing each run separately in Differential Evolution MCMC.

Differential Evolution MCMC is an implementation of Monte Carlo Markov Estimation pioneered in psychology by Turner (reference). The iterative algorithm consists of two key steps. In the first step, two chains are randomly selected, and the difference between the chains is taken and added to another chain. In this way, there is a sort of bootstrap sampling of the space currently occupied across all of the chains. In the second step, occuring only during the warmup phase of the MCMC, a few values are probabilistically selected for swaps between chains. This acts as an accelerator and a form of evolutionary propagation of values between the chains, speeding up their journey toward the highest-likelihood probability space.

## Method


### Implementation

Following the pattern laid out in Brandon Turner's DE-MCMC implementation, the model built here contains several key functions.

The prior log density is calculated by calculating the log density summed across all first-level parameters $\alpha$, $thresh$, and $\tau$, based on their respective second-level hyperpriors. All are modeld using a normal distribution before being transformed for exploration in the model itself. 


```r
# 
# dens=sum(dnorm(x_s[par.ids.l1[["alpha"]]],
#                use.phi[par.ids.l2[["alpha_mu"]]],
#                use.phi[par.ids.l2[["alpha_sigma"]]],log=TRUE)) + 
#     sum(dnorm(x_s[par.ids.l1[["thresh"]]],
#               use.phi[par.ids.l2[["thresh_mu"]]],
#               use.phi[par.ids.l2[["thresh_sigma"]]],log=TRUE)) +
#     sum(dnorm(x_s[par.ids.l1[["tau"]]],
#               use.phi[par.ids.l2[["tau_mu"]]],
#               use.phi[par.ids.l2[["tau_sigma"]]],log=TRUE)) 
#   
```
<!--Revision 8 follows Rev5a with the following features:

 - includes one group of subjects
 - Has a level for multiple runs as random effects
 - Runs can be either reward, punishment, or unspecified. Each subject has an individual parameter specifying difference between reward and punishment runs, and these are drawn from a group-level distribution of runs.
 -->
 
The log density likelihood is calculated using  the following calculations


```r
# dens=0
# 
# ev=matrix(0,100,2)
# v_t=matrix(0,nt,2)
# 
# alpha_tr<-f_alpha_s_tr(as.numeric(x_s[which(par.names=="alpha")]))
# for(tr in 1:use.data_s$choice){
#   if (use.data_s$choice[tr]!=0) {
#     #this must come first - this represents the choice being made.
#     # there is some transformation based on ev and beta needed
#     # before a drift rate can be obtained
#     v_t[tr,]=invlogit(ev[use.data_s$cue[tr],])
#     
#     # prediction error
#     PE   =  use.data_s$outcome[tr] - ev[use.data_s$cue[tr],use.data_s$choice[tr]]
#     PEnc = -use.data_s$outcome[tr] - ev[use.data_s$cue[tr],3-use.data_s$choice[tr]]
#     
#     # value updating (learning)
#     ev[use.data_s$cue[tr],3-use.data_s$choice[tr]] = 
#       ev[use.data_s$cue[tr],3-use.data_s$choice[tr]] + alpha_tr * PEnc;
#     ev[use.data_s$cue[tr],use.data_s$choice[tr]] = 
#       ev[use.data_s$cue[tr],use.data_s$choice[tr]] + alpha_tr * PE;
#     
#   }
# }
# 
# thresh_s_tr<-f_thresh_s_tr(x_s[which(par.names=="thresh")])
# tau_s_tr<-f_tau_s_tr(x_s[which(par.names=="tau")] )
# 
# dens.2choice=
#   log(wald.pdf.raw(t[idx1],alpha[1],v[idx1,1],theta[1])*
#         (1-wald.cdf.raw(t[idx1],alpha[2],v[idx1,2],theta[2]))) + 
#   log(wald.pdf.raw(t[idx2],alpha[2],v[idx2,2],theta[2])*
#         (1-wald.cdf.raw(t[idx2],alpha[1],v[idx2,1],theta[1])))
# 
# dens=dens+dens.2choice
```
 
Using these two functions, the likelihood of the data at the individual run level was assessed. Then, likelihood of the group $\alpha$, $thresh$, and $\tau$ parameters can be measured based on the model priors.

## Results






```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## rstan (Version 2.17.3, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## This is loo version 2.0.0.
## **NOTE: As of version 2.0.0 loo defaults to 1 core but we recommend using as many as possible. Use the 'cores' argument or set options(mc.cores = NUM_CORES) for an entire session. Visit mc-stan.org/loo/news for details on other changes.
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Type 'citation("pROC")' for a citation.
```

```
## 
## Attaching package: 'pROC'
```

```
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```





### Diagnostics

Traditional diagnostic measures like $\hat{R}$ can be difficult in DE-MCMC, so in order to diagnose the performance of chains here, I simply present visual evidence of chain convergence. For group $\alpha$, $k$, and $\tau$ values, chains seem to have converged nicely at the iteration where I began sampling.


![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_1-1.png)<!-- -->

![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_2-1.png)<!-- -->


![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_3-1.png)<!-- -->

We can also look at the distribution of posterior samples for each parameter across each group. The graph below shows $\mu$ parameter distributions fro $\alpha$, $\tau$, and $thresh$ values.

![](MCMC_final_report_november_revision_files/figure-html/h_m3_reward_run2_graphing_distributions-1.png)<!-- -->



![](MCMC_final_report_november_revision_files/figure-html/h_m3_reward_run2_graphing_distributions_sigma-1.png)<!-- -->


Overall, we can see that group parameters appear to have converged. Chains are distributed equally across the space, and distributions for all chains are similar. In order to verify the method has converged nicely, we should also take a look at individual-level parameters.

I also examined 10 subjects selected sequentially along the list of subjects.

![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_subs-1.png)<!-- -->


![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_subs_graph-1.png)<!-- -->

![](MCMC_final_report_november_revision_files/figure-html/graph_diagnostic_data_subs_graph_sigma-1.png)<!-- -->
Subject-level distributions were not as clean as group-level distributions but cconsidering the group level distributions are what we're most interested in, these seem to converge to an acceptable degree.

### Comparing groups

There does appear to be some discrepancy between the estimates for alpha $\mu$ values for both the reward and punishment groups. In particular, the starkest difference appears between Meth and No Meth.

![](MCMC_final_report_november_revision_files/figure-html/ggplot_res_graph_by_motivation_stat-1.png)<!-- -->

We can examine the difference between the Meth and NoMeth groups using the following graph.


![](MCMC_final_report_november_revision_files/figure-html/MethMinusNoMethByMotivation-1.png)<!-- -->

The graph in the top column shows that the 95% HDI for the alpha parameter for the reward-motivated runs appears to be somewhat less for the Risky Meth compared to the Risky No Meth group. There are no other group differences apparent.


![](MCMC_final_report_november_revision_files/figure-html/MethMinusNoMethAcrossMotivations-1.png)<!-- -->


Table: 95% Highest Density Interval for Punishment Condition

Comparison                 alpha           tau             thresh        
-------------------------  --------------  --------------  --------------
RiskyMeth - RiskyNoMeth    [-2.53, 0.30]   [-0.38, 0.26]   [-0.23, 0.35] 
RiskyMeth - SafeNoMeth     [-2.07, 0.76]   [-0.31, 0.38]   [-0.31, 0.32] 
RiskyNoMeth - SafeNoMeth   [-1.29, 2.11]   [-0.17, 0.35]   [-0.28, 0.19] 



Table: 95% Highest Density Interval for Reward Condition

Comparison                 alpha              tau             thresh        
-------------------------  -----------------  --------------  --------------
RiskyMeth - RiskyNoMeth    [-2.81, -0.0074]   [-0.36, 0.24]   [-0.24, 0.32] 
RiskyMeth - SafeNoMeth     [-2.34, 0.79]      [-0.30, 0.36]   [-0.28, 0.31] 
RiskyNoMeth - SafeNoMeth   [-0.64, 1.73]      [-0.16, 0.35]   [-0.27, 0.20] 



Table: 95% Highest Density Interval for all condition

Comparison                 alpha           tau             thresh        
-------------------------  --------------  --------------  --------------
RiskyMeth - RiskyNoMeth    [-2.68, 0.19]   [-0.37, 0.25]   [-0.24, 0.34] 
RiskyMeth - SafeNoMeth     [-2.22, 0.79]   [-0.31, 0.36]   [-0.29, 0.32] 
RiskyNoMeth - SafeNoMeth   [-1.05, 1.96]   [-0.16, 0.35]   [-0.28, 0.19] 


Looking at the group differences across motivations, we can see that the 95% highest density interval encompasses zero, meaning that in a null hypothesis significance test we would be unable to reject a null hypothesis of no difference between groups.

However, because we started with a prior of no difference, a distribution of this character is credible evidence that the Risky Meth group learned the task more slowly than the Risky No Meth group.

![](MCMC_final_report_november_revision_files/figure-html/h_m3_reward_run2 graphing_chains-1.png)<!-- -->

## Discussion

Overall, there is weak evidence presented here of a greater learning rate among the sexually risky, non-meth-using group compared to the sexually risky, meth-using group.

## Attempt at a three level model

It's uncertain how much we would gain from a three-level model. In the two-level model, we do apply appropriate partial pooling of variance across subjects. Effectively, by gathering 4 runs instead of 1, we increase the sample size by a factor of four. With approximately 30 subjects in each group already, this might not gain much extra power. However, a three-level model would certainly be more elegant and parsimonious. Considering the considerable variation I observed in the previous section across the four runs, particularly in the Risky Sex Meth group, a three level model that contains an overall parameter might be instructive.


### Method

The three level model places run-level parameters at the first level, subject-level parameters at the second level, and group-level parameters at the third level. Every parameter is simply some measure of learning rate ($\alpha$), threshold ($thresh$ or $k$), or non-decision-time ($\tau$). 

In a two-level model, for each of those three parameters, we estimate each parameter once per run. At the second level, we estimate a $\mu$ and a $\sigma$ hyper-parameter for each first-level parameter, yielding 6 second-level parameters overall: $\alpha_{\mu}$, $\alpha_{\sigma}$, $k_{\mu}$, $k_{\sigma}$, $\tau_{\mu}$, and $\tau_{\sigma}$.

Priors for the second-level hyper-parameters need to be specified. We could estimate all of these as hyper-parameters themselves, or we could make some simplifying assumptions to reduce the number of hyperparameters at the third level. For instance, $\alpha_{\sigma}$ denotes the variance of the $\alpha$ parameter across runs for a particular subject. That variance could itself vary across subjects, or we might make a simplifying assumption that all subjects have the same run-level variance, or we could make a more moderate simplification by assuming that all subjects within each group have the same run-level variance. With a three-level model, we _also_ have the choice of assuming whether the variance of subjects within groups is fixed across groups or whether it varies.

I modeled group-level $\mu$ and $\sigma$ values to express the mean and variance of the three parameters across subjects. However, I did not model any group-level run variance. All across-run, within-subject variances were modeled separately for each subject from weakly informative priors.

For both level 2 and level 3, $\mu$ priors were set to $\alpha=-3, thresh=log(2), \tau=log(0.6\times min(rt))$, where the prior for non-decision time $\tau$ was calculated based on the minimum reaction time across all subjects.

WHAT ARE THE DISTRIBUTIONS USED FOR THESE, THOUGH? NORMAL, OR SOMETHING ELSE?
<!--
level1 params are alpha, thresh, tau
level2 params have an _s_mu and an _s_sigma for each level 1 parameter.
level3 params have a _g_mu and a g_sigma for everything.
level2 sigma params are simply estimated from the prior - I have NOT attempted to estimate an overall level 2 sigma parmeter
                This could actually be a problem and the reason this hasn't been working!

-->



### Results

Unfortunately, diagnostics for the three level model indicated a lack of convergence--see the three level model section. 

I began by attempting to run an analysis with just 15 subjects. Run-level data often did not properly converge, and didn't produce useful distributions. This might matter less, except that subject-level data estimating parameters across a subject's runs also frequently failed to converge on a solution across chains. This was true even when I extended the analysis to a large number of iterations, up to 10,000 iterations.

When analyzing the data with 15 selected subjects, group-level chains did converge nicely, but didn't differ much from the prior distribution. However this is to be expected with such a small number of subjects.

I also ran the analysis with a full group of subjects and 16000 iterations. At time of publication, the full output from our analysis is available via our server [here](http://msm.fmri.cn/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5j20180528T121720.pdf). At the elevel 1 run-level, chains sometimes fail to converge, but generally show convergence on a set of values. Often, one or two chains of the total set of 24 do not properly initialize. At the level 2 subject level, again, chains often failed to converge. At the level 3 group level, we _do_ generally get proper convergence and adequate chain distribution at least for the $\mu$ values, although $\sigma$ values someitmes showed poor convergence.





```
## [1]   24    3  146    4 2001
```

![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-1.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-2.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-3.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-4.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-5.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-6.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-7.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-8.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-9.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-10.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-11.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-12.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_firstset-13.png)<!-- -->


![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-1.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-2.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-3.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-4.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-5.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-6.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-7.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-8.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-9.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-10.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-11.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-12.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-13.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-14.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-15.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-16.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-17.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-18.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-19.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-20.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-21.png)<!-- -->![](MCMC_final_report_november_revision_files/figure-html/main_h_m5j_3l_model_graph_secondset-22.png)<!-- -->




### Discussion

Modeling subject-level sigma values without attempting to enforce any constraint over these values at the higher level may be the reason why our data was not able to properly converge.




