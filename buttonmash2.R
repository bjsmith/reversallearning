  changeDetect<-function(vec,onset_time=1:length(vec)){
    #order vec
    vec<-vec[order(onset_time)]
    sum(vec[2:length(vec)]!=vec[1:(length(vec)-1)])
  }
  
  
#but how do we get the group-level estimate to ignore button-mashers?
#We would need a subject-level parameter telling us that the data is from a button-masher and not to use it

#if we had a person who was button-mashing, they're doing equivalent to chance, and so we want to distinguish a button-masher from a poor
#performer. 
#poor performers wouldn't have a higher correlation answers to values but they would just have more presses overall.
#buttonmashers will just have a high proportion of CONSECUTIVE answers overall; without taking into account a competitive likelihood estimaiton
#we'll get a probability distribution of presses assuming a true distribution of 50% likelihood of making a switch, with N
#trials and a 50% likelihood of switching each time, what's the likelihood of producing this number of switches from the distribution?
#say N=100
N=100
p=0.5#probability of switching each time
p_switches<-pbinom(c(30,40,50,60,70),size=N,p) #given a probability of switching each time and number of rounds, the probability of making this many switches or less.
#expected probability of number of runs with this many switches
r=1
p_switches*r
r=646
p_switches*r
#this is a probability problem too. so if a probability of the event once is 0.1, 
#then the expected number of runs with that event is
#with two runs, if the probability of this number of switches is 0.9 then the expected number is
#just multiply by 0.9, right? yes. That's the *expected* number.
#So let's get empirical Ns and the probability distribution...

#there were 218 trials in each run.
N_trials_per_run=218
N_runs<-dim(buttonChangesDtByRun)[1]
changes<-60:150
p_switches<-pbinom(changes,size=N_trials_per_run,p)
View(rl.all.subjects.list[subid==105&runid==1&Motivation=="reward"])
#Expected number of runs with at least this many changes.
cbind(changes,round(p_switches*N_runs,4))
#what's the actual distribution of runs with this many changes?
sort(buttonChangesDtByRun$RunButtonChanges)
#so let's find the counts of changes at each level
RunsWithChanges=data.frame(ChangeNum=min(buttonChangesDtByRun$RunButtonChanges):max(buttonChangesDtByRun$RunButtonChanges),RunCount=sapply(min(buttonChangesDtByRun$RunButtonChanges):max(buttonChangesDtByRun$RunButtonChanges),function(x)sum(buttonChangesDtByRun$RunButtonChanges<=x)))
RunsWithChanges$ExpectedRunCount<-round(pbinom(RunsWithChanges$ChangeNum,size=N_trials_per_run,p)*N_runs,3)
View(RunsWithChanges)
