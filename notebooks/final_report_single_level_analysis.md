---
title: 'Final Report: Single level analysis'
output:
  html_document: 
    keep_md: yes
    self_contained: no
  pdf_document:
    keep_tex: yes
---




I ran several single-level joint analyses. These ran quickly without the limitations I experienced running multi-level analyses for joint models.

##Limitations of joint models.

Both the Differential Evolution joint models and the stan NUTS models could not proceed. The Differential Evolution model was able to produce estimates using a two-level model. However, it was unstable in producing estimates at three levels.

% Remember? this was when I ran some massive, 10,000 iteration models; they seemd to run but they were messy and Brandon said the chains were unacceptably all over the place, which they kind of were.


### Experimenting with initialization

The three level NUTS model would initialize, run, and produce an estimate for small groups of as many as 15 subjects but could not properly intialize for larger groups. Because initializing seemed to be the main problem, I focused a lot on attempting to resolve initialization problems. The rstan MCMC estimator works by starting chains off at a particular point within the prior distribution, and calculates Jacobian transforms away from that point repeatedly, picking more likely proposals with a randomly determined probability. THus, it is important that the estimator starts at a point in the probability space that it can iterate from. If the point is too far at one extreme of the distribution, then the model may fail to properly intialize.

I thus experimented with several initialization methods, including randomization and bootstrap methods. For more information on these, see hierarchical\_noncentered\_compare\_starting\_values.

# Single-level models

With single-level models, it was easier to quickly run models and also to implement joint models. In single-level reversal learning models, I experimented with:

 - Different behavioral models, including a discrimination model, an OPAL-inspired model, a simple reinforcement learning model, and a LBA RL model.
 - centered and non-centered parameterizations
 - Variation of priors
 - Variation of covariance matrix estimation method
 
NEED TO EXPLAIN EACH OF THESE! 


## Single-level behavioral LBA RL model

Overall, I found that the behavioral LBA RL model was the best choice. I used a non-centered parameterization, and kept to priors I had been previously using, with a few exceptions. 

### Model

The model implemented Annis, Palmeri, Miller, as discussed elsewhere.

### Prior distributions

In order to facilitate ease of future development of the model into a hierarchical model, at all times I attempted to generate priors using conjugate distributions. Thus, parameter priors were all initially estimated on a normal distribution before being transformed into a more appropriate distribution. The $\alpha$ learning parameter is on a scale from 0 to 1, where zero represents no learning, and 1 represents a perfectly learned stimulus-response pair on each trial. An inverse logit transform converts $\alpha$ from the normal distribution into a logit distribution. At first, I had erroneously used large sigma values on the normal distribution with an aim of producing a non-informative prior. However, sigma values of greater than about 1.7 actually manipulate the shape of the alpha distribution to produce higher probabilities near the 0 and 1 point and lower probabilities at the midpoint. Thus, I settled on a prior for $\alpha$ of 1.5.

$k$ and $\tau$ were both estimated by starting with normal priors and were transformed to an exponential distribution with the inverse natural log function $e^x$. 

(we have the runs for this; consider adding them in!)

I followed recommended stan best practice ( need to add reference to this; I have done some writing on this already!) to use the following process for creating a covariance matrix:

 - Generate a lower-half Omega matrix using an LKJ cholesky correlation matrix function with an $\eta$ parameter of 1.
 - Generate a lower-half sigma matrix from a cauchy distribution with a $\mu=0, \sigma=1$, equal to the number of linked parameters.
 - Get the product of the diagonal matrix formed from the lower-half sigma value and the lower-half Omega matrix.
 - Generate a Sigma matrix using the product of the lower-half sigma matrix and its transpose.
 - Normalize each linked parameter by subtracting its mean and dividing the parameter by its standard deviation.
 - Estimate the log probability of the normalized parameter matrix given the Sigma matrix.
 
The normalized sigma matrix is effectively a correlation matrix.
 
I sampled from the prior distribution of the matrix derived from this method and confirmed that although not completely uninformative, with some bias toward zero covariance, priors were loose and enabled wide ranges of proposal covariance values to be considered.
 
### Regions
 
I tried analyzing freesurfer-derived regions as well as FSL-derived regions. There were adequately large correlations between freesurfer subcortical regions and FSL-derived regions to suggest they were capturing roughly the same regions. Correspondence between cortical regions could not be determined because each method--Freesurfer and FSL--subdivide the cortex using very different regional boundaries.

## Results

For the first model, I ran the model examining potential links with 25 selected freesurfer regions identified as part of the decision-making network.



49 runs did not complete. Runs fail to complete for several reasons, including timing out after 2 hours of failing to converge, or because $\hat{R}$ values are insufficiently close to 1, indicating poor convergence. There were 8 subjects missing from the analysis in the analysis altogether.

![](final_report_single_level_analysis_files/figure-html/Resultsv11f_table_byRPE-1.png)<!-- -->

Putamen, Accumbens, and anterior cingulate gyrus correlated most strongly with the Reward Prediction Error.


![](final_report_single_level_analysis_files/figure-html/Resultsv11e_table_byEV-1.png)<!-- -->

<!--Expected value was most strongly linked positively to the frontal superior sulcus and right dorsal cingulate gryus. -->
Of the regions studied, expected value was most strongly linked positively to the insula superior circular sulcus, the front middle sulcus, and the front superior sulcus. 

Suborbital prefrontal regions did not show the expected effects. The suborbital sulcus was linked _negatively_ to expected value, and several other relevant areas - in particular, the rectus gyrus, did not significantly correlate with the modeled expected value signal.

ADD: count of subjects included in this analysis.

## Targeted analyses

```r
load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180709_1/lba_rl_single_exp_joint_v11t_rsdt.RData")
runs_missing_data <- runs_missing(rawdata, rsdt, FALSE)
```


I re-ran the single-level model to test whether we could get a stronger covariance result by focusing on a few regions. Using the LJK estimator, the estimates do covary between cells across samples and so it might be important to only include the few values we really think will show a correlation. In this analysis, 53 runs were missing and 1 subjects had no runs at all. The missing runs are a concern, and I believe that these missing runs could be driving the problems I have been having getting three-level models running.

## Correlation size and number of parameters

These tiny correlation estimates don't seem to be related to the number of parameters. When I run a model with just a Delta parameters, the sizes are no larger.

![](final_report_single_level_analysis_files/figure-html/JustDeltaParams1-1.png)<!-- -->

The low correlations apparent in this data do not appear to arise from the large number of predictors. Here, it can be seen that even with just four regions tested, although results are significant, they in fact, for the expected value, sit on the opposite side of the significance line. This does call into question the reliability of these data. Significance levels are high, and it would be quite unlikely, with the estimated significance levels here, for the model to swing so strongly from one 

ADD: count of subjects included in this analysis. Do they explain why these correlation strengths are different?


# Expected Value and Reward Prediction Error are strongly bivariate in this model

(reference 20180726 report)
