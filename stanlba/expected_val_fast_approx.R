#we want to identify ALL of the possible expected values the model can take (how many can there be?)
#then calculate the logit for each combination of expected value ahead of time.
#this might speed up the algorithm a little bit.
#so to catalogue this, we need to run the reinforcement learning algorithm through for ALL subjects and create an exhaustive list of all the learned values.
#this is going to be dependent on the learning rate, but not the other parameters.