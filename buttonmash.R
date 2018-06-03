log.dens.like.buttonmash<-function(dr){
  #x_s<-param.l1.init[35,1,]
  #dr<-data[[35]]$runs[[1]]
  #assuming a p simply equal to the proportion of changes in the series
  p=sum(changeDetect(data[[35]]$runs[[1]]$choice))/(length(data[[35]]$runs[[1]]$choice)-1)
  
  
  #likelihood that this subject is simply randomly bashing buttons given a probability p of switching buttons each time.
  #are we trying to find likelihood of value given data, or data given value?
  #I think it's data given value, right?
  #probability of swithcing keys is 1/3.
  #pretend that there are no zeros in the series as a simplifying assumption.
  choice_series<-dr$choice[dr$choice!=0]
  changeDetect<-function(vec){vec[2:length(vec)]!=vec[1:(length(vec)-1)]}
  #switch_key<-rbinom(length(choice_series),1,p)
  #switch_key
  #so we have model where the subject randomly switches button mashing at each time point with probability p
  #what's the probability of exactly this sequence?
  #with binomial switches, we can reduce the information to simply a list describing the number of consecutive choices,
  #plus one bit describing which the first choice is.
  #but another way perhaps is....
  
  #probability is:
  changes<-changeDetect(choice_series)
  #now the probability of getting this given the parameter the multiplication across the entire series of p where there's a change of 1-p where there's no change.
  log(((p)^sum(changes))*((1-p)^sum(!changes)))
  
}

#optimize(log.dens.like.buttonmash,c(0.0,1),dr=data[[35]]$runs[[1]])
#but how do we get the group-level estimate to ignore button-mashers?
#We would need a subject-level parameter telling us that the data is from a button-masher and not to use it
