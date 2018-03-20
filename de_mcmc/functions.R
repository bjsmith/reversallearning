library(compiler)

logit=function(x){
  log(x/(1-x))
}

invlogit=function(x){
  1/(1+exp(-x))
}

#this is using a wald probability density function to calculate results
#and multiplies the result by alpha
#we take the classical formulation of a wald PDF.
#The entire thing is multiplied by alpha;
#(x-1) is replaced by alpha-v*(t[idx]-theta)
#x is replaced by (t[idx]-theta)

#PDF is marginally slower.
# [1] "twaldcdf:7.60203909873962"
# [1] "twaldpdf:10.4707374572754"

#sqrt2pixtothe3<-approxfun(-10^7:10^7/10^7,sqrt(2*pi*(-10^7:10^7/10^7)^3))
#pnorm.approx<-approxfun(-10^7:10^7/10^6,pnorm(-10^7:10^7/10^6))
time.wald.pdf<-0
wald.pdf.raw.warned.about.short.trials.excluded<<-FALSE
wald.pdf.raw=function(t,alpha,v,theta){#,x_w,idx){
  # alpha is threshold
  # v is drift
  # theta is nondecision time
  # t is the response time
  #tstart<-Sys.time()
  #we're going to exclude trials where the response time was implausibly short
  idx=t>theta
  if(any(!idx) & !wald.pdf.raw.warned.about.short.trials.excluded){
    #we're only firing off this warning once
    warning("In wald.pdf.raw, 1 or more trials were excluded because the trial was too short. This message will not be repeated.")
    wald.pdf.raw.warned.about.short.trials.excluded<<-TRUE
  }
  tmp=numeric(length(idx))
  #tmp=numeric(length(t))

  x_w=t[idx]-theta
  #stopifnot(x_w>=0 & x_w<=1)
  #stopifnot(all(x_w>10))
  #tmp[idx]=alpha/sqrt2pixtothe3(x_w) * exp(-(alpha-v*(x_w))^2/(2*(x_w)))
  #tmp[idx]=alpha/sqrt(2*pi*(x_w)^3) * exp(-(alpha-v*(x_w))^2/(2*(x_w)))
  tmp[idx]=alpha/sqrt(2*pi*(t[idx]-theta)^3) * exp(-(alpha-v[idx]*(t[idx]-theta))^2/(2*(t[idx]-theta)))
  #tmp[!idx] <- 0#don't need this; they're zeros to start with.
  #tend<-Sys.time()
  #time.wald.pdf<<-time.wald.pdf+(tend-tstart)
  #print(paste0("twaldpdf:",time.wald.pdf))
  tmp
}
wald.pdf.c<-cmpfun(wald.pdf.raw)

time.wald.cdf<-0
wald.cdf.raw.warned.about.short.trials.excluded<<-FALSE
wald.cdf.raw=function(t,alpha,v,theta){#,x_w,idx){
  #print(idx)
  # alpha is threshold
  # v is drift
  # theta is nondecision time
  # t is the response time
  #tstart<-Sys.time()
  
  lambda=alpha^2
  
  idx=t>theta
  tmp=numeric(length(idx))
  if(any(!idx) & !wald.cdf.raw.warned.about.short.trials.excluded){
    #we're only firing off this warning once
    warning("In wald.cdf.raw, 1 or more trials were excluded because the trial was too short. This message will not be repeated.")
    wald.cdf.raw.warned.about.short.trials.excluded<<-TRUE
  }
  #tmp=numeric(length(t))
  x_w=t[idx]-theta
  mu=alpha/v[idx]

  
  #x_w=t[idx]-theta
  #sltt<-sqrt(lambda/x_w)#moved this portion of the equation here to try stop doubling it up.
  #ttom<-(t[idx]-theta)/mu
  #tmp[idx]=pnorm(sltt*(ttom-1)) + exp(2*lambda/mu) * pnorm(-1*sltt*(ttom+1))
  #tmp[idx]=pnorm.approx(sltt*(ttom-1)) + exp(2*lambda/mu) * pnorm.approx(-1*sltt*(ttom+1))
  #tmp[idx]=pnorm(sltt*((t[idx]-theta)/mu-1)) + exp(2*lambda/mu) * pnorm(-1*sltt*((t[idx]-theta)/mu+1))
  #these shortened versions do not seem to offer any speed advantage
  tmp[idx]=pnorm(sqrt(lambda/x_w)*(x_w/mu-1)) + exp(2*lambda/mu) * pnorm(-1*sqrt(lambda/x_w)*(x_w/mu+1))
  #tmp[!idx] <- 0#don't need this; they're zeros to start with.
  #tend<-Sys.time()
  
  #time.wald.cdf<<-time.wald.cdf+(tend-tstart)
  #print(paste0("twaldcdf:",time.wald.cdf))
  tmp
}
print("compiling..")
wald.cdf.c<-cmpfun(wald.cdf.raw)

rddm=function(i,alpha,v,theta){
  tol=.01
  xs=seq(0,10,tol)
  lxs=length(xs)
  pdf1=get.dens.2choice.simple(t=xs,choice=rep(1,lxs),alpha=alpha,v=v[i,],theta=theta)
  pdf2=get.dens.2choice.simple(t=xs,choice=rep(2,lxs),alpha=alpha,v=v[i,],theta=theta)
  cdf1=sapply(1:length(xs),function(x,pdf)sum(pdf[1:x]),pdf=pdf1)*tol
  cdf2=sapply(1:length(xs),function(x,pdf)sum(pdf[1:x]),pdf=pdf2)*tol
  upp=cdf1[length(xs)]
  sel=runif(1)
  if(sel<upp){
    resp=1
    temp=runif(1,0,max(cdf1))
    out=sort(abs(cdf1-temp),index.return=T,decreasing=F)
    rt=xs[out$ix[1]]
  } else {
    resp=2
    temp=runif(1,0,max(cdf2))
    out=sort(abs(cdf2-temp),index.return=T,decreasing=F)
    rt=xs[out$ix[1]]
  }
  c(resp,rt)
}
wald.cdf.c<-cmpfun(wald.cdf.raw)

get.dens.2choice_count=0
time.wald<-0
get.dens.2choice=function(t,choice,alpha,v,theta){
  # t is a vector of times
  # choice is a vector of responses, same length as t
  # alpha and v are vectors of length 2, one for each accumulator
  idx1=(choice==1)
  tmp=numeric(length(idx1))
  #tstart<-Sys.time()
  
  #x_w=t[idx]-theta
  #tmp[idx1]=wald.pdf.raw(t[idx1],alpha[1],v[idx1,1],theta[1])*(1-wald.cdf.raw(t[idx1],alpha[2],v[idx1,2],theta[2]))
  tmp[idx1]=wald.pdf.raw(t[idx1],alpha[1],v[idx1,1],theta[1])*(1-wald.cdf.raw(t[idx1],alpha[2],v[idx1,2],theta[2]))
  if (any(is.nan(tmp[idx1]))){
    print("tmp[idx1] is NaN")
    print(alpha)
  }
  
  
  idx2=(choice==2)
  
  #tmp[idx2]=wald.pdf.raw(t[idx2],alpha[2],v[idx2,2],theta[2])*(1-wald.cdf.raw(t[idx2],alpha[1],v[idx2,1],theta[1]))
  tmp[idx2]=wald.pdf.raw(t[idx2],alpha[2],v[idx2,2],theta[2])*(1-wald.cdf.raw(t[idx2],alpha[1],v[idx2,1],theta[1]))
  if (any(is.nan(tmp[idx2]))){
    print("tmp[idx2] is NaN")
    print(alpha)
    
  }
  #print("running get.dens.2choice")
  get.dens.2choice_count<<-get.dens.2choice_count+1
  #tend<-Sys.time()
  #time.wald<<-time.wald+(tend-tstart)
  #if((get.dens.2choice_count%%1000)==0) print(paste0(get.dens.2choice_count, "twalds:",time.wald))
  
  # if (is.nan(sum(log(tmp)))){
  #   
  #   print("sum log tmp is NaN")
  #   #it's because densities are coming out negative, so the log densities are NA.
  #   print(paste0("alpha:",alpha))
  #   print(tmp)
  #   print(log(tmp))
  #   print(sum(log(tmp)))
  #   print(t[idx1])
  #   print(alpha[1])
  #   print(v[idx1,1])
  #   print(theta[1])
  # }
  tmp
}

get.dens.2choice.simple=function(t,choice,alpha,v,theta){
  # t is a vector of times
  # choice is a vector of responses, same length as t
  # alpha and v are vectors of length 2, one for each accumulator
  idx1=(choice==1)
  tmp=numeric(length(idx1))
  tmp[idx1]=wald.pdf(t[idx1],alpha[1],v[1],theta[1])*(1-wald.cdf(t[idx1],alpha[2],v[2],theta[2]))
  tmp[!idx1]=wald.pdf(t[!idx1],alpha[2],v[2],theta[2])*(1-wald.cdf(t[!idx1],alpha[1],v[1],theta[1]))
  tmp
}
############################################################

#write files for a two level hierarchical model.
write.files.h2=function(q,use.param,use.weight,append=TRUE,parnames){#q=1;use.param=param;use.weight=weight;append=FALSE;parnames=colnames(param)

  # if (file.exists(subDir)){
  #   #setwd(file.path(mainDataDir, subDir))
  # }
  if (!file.exists(subDir)) {
    dir.create(file.path(mainDataDir, subDir))
    #setwd(file.path(mainDataDir, subDir))
  }
  write(round(use.param[q,],6),paste(mainDataDir,subDir, "chain",q,"_lower.txt",sep=""),ncolumns=length(parnames),append=append)
  write(round(use.weight[q],8),paste(mainDataDir,subDir,"chain",q,"_weights.txt",sep=""),ncolumns=1,append=append)
  #setwd(mainDataDir)
}

write.files=function(q,use.paramlist,use.weight,append=TRUE){
  
  # if {
  #   setwd(file.path(mainDataDir, subDir))
  # } 
  if (!file.exists(subDir)){
    dir.create(file.path(mainDataDir, subDir))
    #setwd(file.path(mainDataDir, subDir))
  }
  for(j in 1:S)write(round(use.paramlist[q,,j],6),paste(mainDataDir,subDir, "chain",q,"_sub",j,"_lower.txt",sep=""),ncolumns=n.pars,append=append)
  write(round(use.weight[q,],8),paste(mainDataDir,subDir,"chain",q,"_weights.txt",sep=""),ncolumns=S,append=append)
  #setwd(mainDataDir)
}

printvar<-function(x){
  print(paste0(x,": ",get(x)))
}

crossover=function(i,par_ind,use.param,use.like,prior,log.dens.like,log.dens.prior,...){
  printv(paste0("crossover i:",i))
  #use.param is nChains*nParam
  #use.like is nChains
  require(msm)
  #we have to change this line to handle multiple subjects 
  use.weight=use.like[i] + log.dens.prior(x=use.param[i,],prior=prior)
  gamma = runif(1,.5,1)
  index=sample(c(1:n.chains)[-i],2,replace=F)
  param=use.param[i,]
  param[par_ind]=use.param[i,par_ind] + gamma*(use.param[index[1],par_ind]-use.param[index[2],par_ind]) + runif(1,-b,b)
  param=matrix(param,1,length(param))
  like=log.dens.like(x=param,...)
  weight=like + log.dens.prior(x=param,prior=prior)
  if(is.na(weight)){weight=-Inf;printvar("use.like")}
  if(runif(1) < exp(weight-use.weight)) {							
    use.param[i,]=param
    use.like[i]=like
  }
  c(use.like[i],use.param[i,])
}

# 
# crossover=function(i,pars,use.paramlist,use.like,prior,log.dens.like,...){
#   #i=1;pars=1:n.pars;use.paramlist=array(param,c(n.chains,n.parinstances));
#   #use.like=weight;use.data=data;prior=prior;log.dens.like=log.dens.like.f;log.dens.prior=log.dens.prior.f
#   require(msm)
#   use.weight=use.like[i] + log.dens.prior(x=use.paramlist[i,],prior=prior)
#   printvar("use.weight")
#   gamma = runif(1,.5,1)
#   printvar("gamma")
#   index=sample(c(1:n.chains)[-i],2,replace=F)
#   printvar("index")
#   paramlist=use.paramlist[i,]						
#   paramlist[pars]=use.paramlist[i,pars] + gamma*(use.paramlist[index[1],pars]-use.paramlist[index[2],pars]) + runif(1,-b,b)
#   paramlist=matrix(paramlist,1,length(paramlist))
#   printvar("paramlist")
#   like=log.dens.like(x=paramlist,use.data)
#   printvar("like")
#   ######THIS IS WHERE WE'RE STUCK. 
#   #PASSING A NULL VALUE INTO "WEIGHT"
#   #I WONDER IF IT'S WORTH REVERTING CROSSOVER TO PREVIOUS FORM AND TRYING THE CONVERSION AGAIN, THIS TIME A BIT MORE CAREFULLY.
#   
#   weight=like + log.dens.prior(x=paramlist,prior=prior)
#   if(is.na(weight))weight=-Inf
#   if(runif(1) < exp(weight-use.weight)) {							
#     use.paramlist[i,]=paramlist
#     use.like[i]=like
#   }
#   printvar("weight")
#   c(use.like[i],use.paramlist[i,])
# }

wrapper=function(x,use.paramlist,use.like,log.dens,use.data,prior,...)migrate(use.paramlist=array(use.paramlist[,,x],c(n.chains,n.pars)),use.like=use.like[,x],log.dens=log.dens,use.data=use.data[[x]],prior=prior,...)

migrate=function(use.paramlist,use.like,log.dens,...){
  pars=dim(use.paramlist)[2]
  lnum1=sample(c(1:n.chains),1)										# determine how many groups to work with
  lnum2=sort(sample(c(1:n.chains),lnum1,replace=F))							# which groups we will work with
  paramset=matrix(NA,lnum1,pars)									# initialize
  currentset=propset=propw=currw=numeric(lnum1)
  index=numeric(lnum1)
  for(i in 1:lnum1){
    index[i]=sample(1:n.chains,1,replace=F)	
    paramset[i,]=use.paramlist[lnum2[i],] + runif(1,-b,b)				# create a set of these particles to swap
    propset[i]=log.dens(paramset[i,],...)
    currentset[i]=use.like[lnum2[i]]
    propw[i]=propset[i]
    currw[i]=currentset[i]
  }
  if(runif(1) < exp(propw[lnum1] - currw[1])){
    use.paramlist[lnum2[1],]=paramset[lnum1,]							# swap the first with the last (creating a circle)
    use.like[lnum2[1]]=propset[lnum1]
  }
  if(lnum1!=1){											# make sure we are not done yet
    for(i in 1:(lnum1-1)){		
      if(runif(1) < exp(propw[i] - currw[i+1])){
        use.paramlist[lnum2[i+1],]=paramset[i,]							# swap the first with the last (creating a circle)
        use.like[lnum2[i+1]]=propset[i]
      }}}
  list(weight=use.like,theta=use.paramlist)
}
