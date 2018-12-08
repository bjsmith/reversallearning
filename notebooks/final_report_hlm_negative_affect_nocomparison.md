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

After the NPS scores were obtained (see Section DATA PROCESSING CHAPTER), I tested these two postulates using a hierarhical linear model.






607 runs across 161 subjects in the reversal learning dataset were added into a hierarchical linear model. For reasons discussed in Section~REFERENCE THE OTHER LINEAR MODEL, I used rstanarm to do the analysis.

We represent subject as $S$, run as $S$, and image as $\iota$. We also include Response and Motivation:

$V=\mathit{RESPONSE}_k*M_k+\mathit{PRESENTATION}_{i} + S_{s(i, k )} + R_{r(s,i , k )} + \iota_{j(i, k)} + \epsilon$ \footnote{I have to go back and see if this notation is really the notation I want to use! It's pretty wack, but I think it is fully expressing the different parts of the model properly. But this section should be consistent with the other section}

Neural pain signature values were mean centered for each subject across all runs.

## Results

The fixed effects of ReponseCorrect and presentation in segment are shown below.


```{}
ValueScaled~
  (ResponseCorrect==FALSE) + Motivation + 
  presentation_n_in_segment + 
  (1+presentation_n_in_segment | subid/runmotiveid) + 
  (1 | image)

```






                                                                 mean    mcse      sd     2.5%      50%    97.5%   n_eff    Rhat
------------------------------------------------------------  -------  ------  ------  -------  -------  -------  ------  ------
(Intercept)                                                    -0.017   0.001   0.034   -0.084   -0.018    0.049     678   1.002
ResponseCorrect == FALSETRUE                                   -0.002   0.000   0.009   -0.019   -0.002    0.015    4000   0.999
Motivation == "punishment"TRUE                                 -0.075   0.001   0.024   -0.123   -0.074   -0.029    1440   1.002
presentation_n_in_segment                                       0.011   0.000   0.002    0.007    0.012    0.015    4000   1.000
ResponseCorrect == FALSETRUE:Motivation == "punishment"TRUE     0.102   0.000   0.011    0.079    0.102    0.124    4000   0.999


In this model, there was a main effect of Punishment, indicating that even when controlling for the specific experience of pain during the incorrect trials in the punishment condition, there was evidence overall of slightly more pain experience in the Punishment condition than in the Reward condition.

With an estimate of $b=0.102$ (CI=$[0.079,0.124]$), the interaction of Incorrect Response and Motivation is strong and indicates that there was a brain response to an incorrect outcome in the electric shock punishment condition but not in the reward condition. The interaction shows up particularly clear in the subject aggregates in table XX which show a large difference in NPS activity between incorrect and correct trials in the Punishment condition but not in the Reward condition.


The main effect of motivation actually turns out to be in the reverse of the predicted direction: the magnitude of the NPS signal is _lower_ for the Pain condition than the Reward condition ($b=-0.075$, Ci=$[-0.074, -0.029]$). It should be remembered that when we combine this with the interaction term, $b_{Motiv=punish} + b_{Motiv=Punish*Response=Incorrect}$, we see that the predicted deviation from baseline is $0.102\times 0 - 0.075=-0.075$ during the Punishment condition when there is no Punishment signal (i.e., response is correct), but $0.102\times 1 - 0.075=0.027$ during the Punishment condition in trials that do contain a Punishment signal. This effect may arise from normalization of the NPS signal of the Reward and Punishment conditions separately.



The graph below shows subjects' mean pain responses in the reward and punishment conditions, when they got a response correct or a response incorrect. Subjects who did not have at least one run in each of the motivation conditions were excluded. Values 

![](final_report_hlm_negative_affect_nocomparison_files/figure-html/finalgraphs2-1.png)<!-- -->

![](final_report_hlm_negative_affect_nocomparison_files/figure-html/finalgraphs3-1.png)<!-- -->

![](final_report_hlm_negative_affect_nocomparison_files/figure-html/finalgraphs4-1.png)<!-- -->


## Discussion

This may be the first time that a generalized neurologic pain signal has been connected to an electric shock stimulus.

There are problems with the model used as it doesn't fully account for all the levels of variance. To do that we need to use a Bayesian model.

I ran a Bayesian model using rstanarm. In this design, I included random effects of correct response and presentation order, as well as intercept and presentation order within segment for each subject and each run within each subject. This was more variance than I could account for in a straight linear model.

The 95% HDI for the beta value for ResponseCorrect$\times$Punishment interaction was [-0.11, -0.04], reflecting that high probability that an incorrect response would lead to a pain signal in the Punishment condition only.

The graphical data is puzzling, and suggests that although there was a difference in NPS visible within punishment condition between correct and incorrect responses, the level of pain observed during the punishment condition was comparable to the level of pain experienced in the reward condition. This may be an artefact of the analysis process: becasue they were separate runs, each were overall mean-centered separately. 


Overall, the result confirms the initial hypothesis. There was a substantial difference between wrong and correct trials in the punishment condition. There was no evidence for residual pain in the punishment condition: correct responses in the punishment condition had, if anything, lower pain responses than correct responses in the reward condition (Figure X). There was also no evidence for a difference in experienced pain between correct and incorrect responses in the reward condition (Figure Y), suggesting that only physical pain through a positive punisher triggered the NPS response while negative reinforcement (losing the opportunity for a reward after an incorrect response in the reward condition) did not yield the same result.

