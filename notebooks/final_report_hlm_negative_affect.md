---
title: "Pain signal correlates with activity"
output:
  html_document:
    keep_md: yes
    self_contained: no
  pdf_document:
    keep_tex: yes
---

# Pain signature correlates with behavioral performance in the punishment task

In the literature, 'negative reinforcer' typically refers to an aversive stimulus that is removed when a correct response is performed; 'positive punishment' typically refers to an aversive stimulus that is applied when an incorrect response is performed, while 'negative punishment' refers to an reinforcer that is removed when an incorrect response is performed. 'Negative punishment' might include taking away money a subject has been given following an incorrect response, while 'negative reinforcment might include' continually applying an unpleasant sensation or loud noise until In our dataset, subjects received positive reinforcement when they made the correct response to a trial in the positive condition, and received an unconditioned positive punisher in the form of an electric shock to the wrist following an incorrect response to a trial in the punishment condition.

As far as I am aware there is no published work examining the effect of a unconditioned primary positive pain punisher on shaping human behavior in fMRI\footnote{Loud noises have certainly been used in fMRI studies as an aversive stimulus, but I am unaware of whether they have been used in a behavioral task to shape behavior through positive punishment. Their status as an unconditioned aversive stimulus is also unclear, whereas an electric shock is indisputably an unconditioned aversive stimulus}. This is interesting because understanding this can help us to understand the differing neural circuitries involved in positive pain punishment compared to positive reinforcement. An fMRI punishment approach is particularly helpful because it helps us to understand the differences in a controlled experimental setting.

In our joint modeling approach, a natural way to examine the differences is to examine differences in parameters for positive compared to negative reinforcement runs. We may examine model parameters such as the learning rate, as well as other parameters such as the covariance of expected value and reward prediction error with punishment cues.

### Neural pain signature

The Neural Pain Signature (NPS) is a brain image from Tor Wager's 2013 paper on brain activity correlated with pain intensity. Rather than attempting to identify clusters, Wager (2013) identified a whole-brain measure, examining the extent to which each individual voxel correlates with pain intensity. In doing so, he was able to get a very fine-grained brain image that expresses in standard space the extent to which each voxel correlates with pain intensity. 



### NPS in this task

If the NPS signature is generalizable to our subjects, then we should see that when subjects were in the punishment condition, their NPS activity should be elevated when they receive positive punishment. We can predict pain using a hierarchical linear model, in which responding correctly to a trial and punishment condition are included as independent variables. A main effect of punishment condition would suggest an extended pain feeling associated with the condition. This might be variously interpreted as residual physical pain felt after the electric shock and carrying over into subsequent trials, or as anticipatory pain or anxiety. A main effect of incorrect response would be interesting and suggest that the pain intensity signal identified by Wager et al (2013) can also capture what might be interpreted as 'non-physical pain' associated with failing to respond correctly. An interaction effect would suggest that our subjects really felt pain when experiencing the aversive stimulus, and that this was not associated with disappointment from failing to respond correctly. Any of the above would suggest that the NPS is generalizable to our subjects in some form. A failure to detect an effect could variously suggest that the NPS isn't generalizable to our subjects, or that our electric shock was not substantially painful, or could suggest a simple analysis error.

### Predictions

If the NPS similarity reliably measures pain, then we should see that the NPS score is higher in trials where subjects receive an electric shock compared to trials where they do not. Thus, sign that the NPS is measuring pain is to ensure that:

 1. In the punishment runs, NPS scores should be higher in those trials where subjects receive an electric shock than those where they did not, i.e., where they made an incorrect response or made no response compared to when they made a correct response.
 2. The difference between incorrect or non-responses and correct responses should be substantially stronger in the punishments than in the reward runs, where subjects were being rewarded for correct responses, but not punished for incorrect responses. If this is indeed the case, then we have evidence the NPS is measuring the brain response to physical pain. If there is no difference, then the NPS might be detecting non-physical pain, or a negative signal which is not physical pain.

## Method

After the NPS scores were obtained (see Section DATA PROCESSING CHAPTER), I tested these two postulates using a series of linear models.






607 runs across 161 subjects in the reversal learning dataset were added into a hierarchical linear model. For reasons discussed in Section~REFERENCE THE OTHER LINEAR MODEL, I used rstanarm to do the analysis.

I first built a base level model, for comparison, that attempts to predict the NPS signal on a given trial without reference to whether or not that trial was correct.

We represent subject as $S$, run as $S$, and image as $\iota$; then 

$V=\mathit{PRESENTATION}_{i} + S_{s(i)} + R_{r(s,i)} + \iota_{j(i)} + \epsilon$\label{eq:PainBaseNoMotivation}

We can also run the model with Motivation, but not response:

$V=M_k + S_{s(i)} + R_{r(s,i)} + \iota_{j(i)} + \epsilon$\label{eq:PainBaseNoResponse}


Then we can repeat the model, this time with the correct response included:

$V=\mathit{RESPONSE}_k*M_k+\mathit{PRESENTATION}_{i} + S_{s(i, k )} + R_{r(s,i , k )} + \iota_{j(i, k)} + \epsilon$ \footnote{I have to go back and see if this notation is really the notation I want to use! It's pretty wack, but I think it is fully expressing the different parts of the model properly. But this section should be consistent with the other section}

If the third model is significantly better than each of the first two, then we have clear evidence that the interaction effect between response and motivation condition (i.e., when subjects actually get an electric shock) exists.

Neural pain signature values were mean centered for each subject across all runs.

## Results

<!-- ## Comparison with Reward runs -->

<!-- However, this by itself does not demonstrate that the NPS reponse is in fact directly related to physical pain from the electric shock. Although there are a large number of possible alternative explanations, one plausible explanation is that the NPS signal change is simply that getting an item wrong in the game is affectively unpleasant, even without a direct physical pain. We might posit an ego-driven response to performing well in the task; in effect, an incorrect response is its own punishment. -->

<!-- In order to rule out this possibility, we need to compare the response we get during the punishment runs with a similar response in the reward runs. If we measure the NPS during the Reward task, and compare correct and incorrect responses, do we see the same or a different response? A significantly larger difference in NPS between incorrect and correct responses during hte Reward task seems very unlikely and would likely suggest some methodological error. A main effect difference in the magnitude of the incorrect and correct responses across the Reward and Punishment conditions would suggest the NPS is tracking some kind of punishment signal other than pain. We hope to see a significantly larger difference in NPS between incorrect and correct responses during the Punishment task, and would demonstrate that the signal we find is related to the physical pain of an electric shock. Note that these last two effects could be observed simultaneously, i.e., a main effect and an interaction effect. -->

<!-- We first build a base model that does NOT include information about motives but only whether the response in each trial is correct. Note that the runid used here will index Reward and Punishment runs uniquely, i.e., if the subject has 2 reward and 2 punishment runs, as most do, then runs are numbered from 1 to 4. Here we do include both reward and punishment data. -->

<!-- $V=\mathit{PRESENTATION}_{i} + S_{s(i)} + R_{r(s,i)} + \iota_{j(i)} + \epsilon$ -->

The first base pain model included information about response, but not about motivation (Equation~\ref{eq:PainBaseNoMotivation}). It can be written in R like

```{}
ValueScaled~
  (ResponseCorrect==FALSE) + 
  presentation_n_in_segment + 
  (1+presentation_n_in_segment | subid/runmotiveid) + 
  (1 | image)

```

The fixed effects of ReponseCorrect and presentation in segment are shown below.


```
## Warning: namespace 'rstanarm' is not available and has been replaced
## by .GlobalEnv when processing object 'm.rp.1.nomotivation.stan'
```

Quitting from lines 124-128 (final_report_hlm_negative_affect.Rmd) 
Error in if (df.r > 0) { : missing value where TRUE/FALSE needed
Calls: <Anonymous> ... eval -> <Anonymous> -> summary -> summary -> summary.glm

Both Response Incorrect and Presentation N postively predict pain.

I then build a second comparison model. This one includes information about motives, but not whether the response in each trial is correct.





                                      mean        mcse          sd         2.5%          50%       97.5%   n_eff        Rhat
-----------------------------  -----------  ----------  ----------  -----------  -----------  ----------  ------  ----------
(Intercept)                     -0.0564802   0.0011174   0.0311705   -0.1171299   -0.0570284   0.0031065     778   1.0035026
ResponseCorrect == FALSETRUE     0.0547575   0.0000926   0.0058535    0.0435186    0.0546743   0.0662753    4000   0.9998315
presentation_n_in_segment        0.0117493   0.0000329   0.0020791    0.0077404    0.0117348   0.0158829    4000   0.9991234







Finally, we can examine the model that includes both Motivation and Trial Response:






```
## 
## 
##                                                                      mean        mcse          sd         2.5%          50%        97.5%   n_eff        Rhat
## ------------------------------------------------------------  -----------  ----------  ----------  -----------  -----------  -----------  ------  ----------
## (Intercept)                                                    -0.0174258   0.0013238   0.0344792   -0.0843168   -0.0176780    0.0491088     678   1.0022331
## ResponseCorrect == FALSETRUE                                   -0.0020475   0.0001358   0.0085867   -0.0186858   -0.0021165    0.0146291    4000   0.9992762
## Motivation == "punishment"TRUE                                 -0.0747125   0.0006274   0.0238127   -0.1233088   -0.0742514   -0.0287189    1440   1.0023112
## presentation_n_in_segment                                       0.0114784   0.0000321   0.0020278    0.0074069    0.0115253    0.0154053    4000   0.9998858
## ResponseCorrect == FALSETRUE:Motivation == "punishment"TRUE     0.1018704   0.0001804   0.0114082    0.0790453    0.1019392    0.1244361    4000   0.9994433
```


In this model, there was a main effect of Punishment, indicating that even when controlling for the specific experience of pain during the incorrect trials in the punishment condition, there was evidence overall of slightly more pain experience in the Punishment condition than in the Reward condition.

## Model comparison

Because these models are Bayesian models, standard AIC and BIC tests are less useful. However we can apply Pareto smoothed importance-sampling leave-one-out cross-validation \footnote{(Vehtari, A., Gelman, A., and Gabry, J. (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. Statistics and Computing. 27(5), 1413–1432. :10.1007/s11222-016-9696-4. Links: published | arXiv preprint.

Vehtari, A., Gelman, A., and Gabry, J. (2017). Pareto smoothed importance sampling. arXiv preprint arXiv:1507.04544.)}




```
## This is loo version 2.0.0.
## **NOTE: As of version 2.0.0 loo defaults to 1 core but we recommend using as many as possible. Use the 'cores' argument or set options(mc.cores = NUM_CORES) for an entire session. Visit mc-stan.org/loo/news for details on other changes.
```

BIC indicates that the model including punishment is strongly predictive.

However the clearest indication of the result here, since we are interested in the direction of an effect, will come from examining the significance and magnitude of the effect sizes of the regressors themselves:

<!-- ```{r } -->

<!-- summary(m.rp.1.stan)[] -->
<!-- ``` -->

<!-- -0.1233088 </td> -->
<!--    <td style="text-align:right;"> -0.0742514 </td> -->
<!--    <td style="text-align:right;"> -0.0287189  -->
The result:

1. With an estimate of $b=0.102$ (CI=$[0.079,0.124]$), the interaction of Incorrect Response and Motivation is strong and indicates that there was a brain response to an incorrect outcome in the electric shock punishment condition but not in the reward condition. The interaction shows up particularly clear in the subject aggregates in table XX which show a large difference in NPS activity between incorrect and correct trials in the Punishment condition but not in the Reward condition.
2. The main effect of motivation actually turns out to be in the reverse of the predicted direction: the magnitude of the NPS signal is _lower_ for the Pain condition than the Reward condition ($b=-0.075$, Ci=$[-0.074, -0.029]$). It should be remembered that when we combine this with the interaction term, $b_{Motiv=punish} + b_{Motiv=Punish*Response=Incorrect}$, we see that the predicted deviation from baseline is $0.102\times 0 - 0.075=-0.075$ during the Punishment condition when there is no Punishment signal (i.e., response is correct), but $0.102\times 1 - 0.075=0.027$ during the Punishment condition in trials that do contain a Punishment signal. This effect may arise from normalization of the NPS signal of the Reward and Punishment conditions separately.



The graph below shows subjects' mean pain responses in the reward and punishment conditions, when they got a response correct or a response incorrect. Subjects who did not have at least one run in each of the motivation conditions were excluded. Values 

![](final_report_hlm_negative_affect_files/figure-html/finalgraphs2-1.png)<!-- -->

![](final_report_hlm_negative_affect_files/figure-html/finalgraphs3-1.png)<!-- -->

![](final_report_hlm_negative_affect_files/figure-html/finalgraphs4-1.png)<!-- -->


# Pain signal section discussion

<!-- 

See negative-affect/stan_task.R 

-->

This may be the first time that a generalized neurologic pain signal has been connected to an electric shock stimulus.

There are problems with the model used as it doesn't fully account for all the levels of variance. To do that we need to use a Bayesian model.

I ran a Bayesian model using rstanarm. In this design, I included random effects of correct response and presentation order, as well as intercept and presentation order within segment for each subject and each run within each subject. This was more variance than I could account for in a straight linear model.

The 95% HDI for the beta value for ResponseCorrect$\times$Punishment interaction was [-0.11, -0.04], reflecting that high probability that an incorrect response would lead to a pain signal in the Punishment condition only.

The graphical data is puzzling, and suggests that although there was a difference in NPS visible within punishment condition between correct and incorrect responses, the level of pain observed during the punishment condition was comparable to the level of pain experienced in the reward condition. This may be an artefact of the analysis process: becasue they were separate runs, each were overall mean-centered separately. 

<!-- $ValueScaled_{i j}= \beta_{0 j} ResponseCorrect + \beta_{1 j}\text{presentation}_{i j} + e_{i j} + (1+presentation_n_in_segment | subid/runmotiveid) +  -->
<!--                           (1 | image) -->

Overall, the result confirms the initial hypothesis. There was a substantial difference between wrong and correct trials in the punishment condition. There was no evidence for residual pain in the punishment condition: correct responses in the punishment condition had, if anything, lower pain responses than correct responses in the reward condition (Figure X). There was also no evidence for a difference in experienced pain between correct and incorrect responses in the reward condition (Figure Y), suggesting that only physical pain through a positive punisher triggered the NPS response while negative reinforcement (losing the opportunity for a reward after an incorrect response in the reward condition) did not yield the same result.
