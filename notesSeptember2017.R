rl.all.subjects.list[,.(SubCount=.N),by=.(subid,RiskLabel)][,.N,RiskLabel]
#so we should have around about 110 subjects in a full Risky NoMeth vs. Meth analysis.
#Can we find them?

data.table(rawdata)[,.N,by=.(subid,SubjectGroup)][,.N,by=SubjectGroup]
#66 risky non-meth users.
#so only 36 meth-users. figures.
#So the samples of 66 and 36 are meth and non-meth users, as labelled :-)
#I wonder how similar the different stan models are?
#specifically, i wonder if it is easy or difficult to swap out nate's single group models with my amended multi-group models.
#comparing them now, my model was a double-update rule.
#yeah, looks the same as the double-update model; just with slightly different tracking variables 
#in the posterior predictive check.

#so...I don't know, we could probably try out the the group model.