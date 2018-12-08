---
title: "Exploring Revision 8 MCMC"
output:
  html_document:
    keep_md: yes
    self_contained: no
  pdf_document:
    fig_caption: yes
    keep_tex: yes
---
 

I ran Revision 8 using `rstan` MCMC, running separately on each of the three risk groups. 







```
## [1] "initializing..."
```





##Results

### Diagnostics: Reliablity, representativeness, and accuracy

Previously I had run the MCMC algorithm multiple times, getting the same result each time. This confirms that the posterior is representative of the underlying data. I didn't repeat the reliability test for the final analysis because there was very, very little difference between repetitions in earlier tests.

To assess Representativeness for an MCMC algorithm, we need to see whether chains have converged such that initial randomly-chosen priors are not related to final values. We can examine the Gelman-Rubin statistic (Gelman & Rubin, 1992), also known as the "potential scale reduction factor" or "shrink factor". In $rstan$, this is available as the statistic $\widehat{r}$.



Table: group= 1 double_update_rev8 1 MCMC vars= 931

                      mean   se_mean     sd     n_eff   Rhat
-------------------  -----  --------  -----  --------  -----
group_mu_alpha        0.33         0   0.05   2055.30   1.01
group_mu_beta         0.55         0   0.05    754.39   1.01
group_rew_mu_alpha    0.38         0   0.08   2537.63   1.00
group_pun_mu_alpha    0.29         0   0.07   2437.59   1.01



Table: group= 2 double_update_rev8 1 NA vars= 1199

                      mean   se_mean     sd     n_eff   Rhat
-------------------  -----  --------  -----  --------  -----
group_mu_alpha        0.34         0   0.05   6833.29      1
group_mu_beta         0.73         0   0.05   4741.86      1
group_rew_mu_alpha    0.30         0   0.06   6511.53      1
group_pun_mu_alpha    0.38         0   0.07   5993.03      1



Table: group= 3 double_update_rev8 1 NA vars= 687

                      mean   se_mean     sd     n_eff   Rhat
-------------------  -----  --------  -----  --------  -----
group_mu_alpha        0.26      0.00   0.05   1919.88   1.01
group_mu_beta         0.65      0.01   0.08    188.82   1.05
group_rew_mu_alpha    0.31      0.00   0.08    659.24   1.02
group_pun_mu_alpha    0.21      0.00   0.06   2377.73   1.01

Accuracy can be measured by the effective sample size, as described in the sectio nabove. 

For Group 3, specifically for the group beta parameter, the effective sample size is just 189, indicating poor sampling for that value. However, for all other values, the Rhat was in the appropriate range. For all other groups, the Rhat was within the appropriate range for all variables of interest.

## Group differences

The basic graphs show that there is substantial overlap in estimated alpha and beta parameters within each model.

![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

![](final_report_three_level_analysis_rev8_files/figure-html/sigma-1.png)<!-- -->

We can compare Motivational conditions by subtracting the posteriors derived for one condition with the posteriors derived in the other condition.

Comapring reward and punishment estimates, there is not a clear estimate of a difference of overall performance between the two motivational conditions.

![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-3-1.png)<!-- -->![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

We can also compare the groups, looking at group differences overall, and also specifically at group differences within each reward and punishment conditions. We use the same method as before: subtracting the posteriors derived in one group from the posteriors derived in another group.

In the reward condition, there's no difference apparent in learning rate between the meth-using group and the other groups. However, in the punishment condition, there may be some evidence of a difference, with the Highest Density Interval of the differnce just crossing zero.

![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Comparing group 2 with group 3, there may be a difference between alpha learning rates in the punishment condition.

![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Statistic          Mean          SD  HDI          
----------  -----------  ----------  -------------
mu           -0.0848639   0.0699501  [-0.22,0.06] 
sigma         0.0220197   0.0813884  [-0.13,0.18] 
rew_mu        0.0126976   0.1022954  [-0.18,0.21] 
pun_mu       -0.1761860   0.0943302  [-0.37,0.00] 

The highest density interval for the difference between Group3 minus Group 2 still encompassed zero. We can also look at a multivariate 95% highest density interval. This uses the R function |MASS::cov.mve| to estimate the ellipsoid with the minimum volume encompassing 95% of the points.


![](final_report_three_level_analysis_rev8_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

We can't confirm using a bivariate confidence interval that there is a difference between the means of Group 2 and Group 3 in the punishment condition.

# Discussion

This analysis demonstrates that it is in principle possible to run a three-level behavioral model in MCMC on our reversal learning dataset. A joint model built on this initial three-level behavioral model is very time-intensive to run, and so I have not included it in this thesis. Still, by examining behavioral data we are able to glean insight into behavioral patterns. The behavioral model also serves as a useful baseline for testing the efficacy of joint modeling. It may be that some insights are available using a joint model that cannot be gleaned using the method presented here.

