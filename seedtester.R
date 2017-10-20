#the question: are near-seed values more predictable than far-seed values?
#sample.int(maxval, 1)
set.seed(1154763)
maxval<-round(sample.int(.Machine$integer.max-10000,1))
t.tests<-sapply(1:100,function(x){
  #method set a new seed, each time iterating it just by once, and resample
  consec.vals<-sapply(1:10000,function(t){
    myseed<-t+sample.int(maxval, 1)
    set.seed(myseed)
    val<-runif(1,0,1)
    return(val)
  })
  if(mod(x,10)==0){
    cat(".")
  }
  #method just sampling repeatedly without setting consecutive seeds
  nonconsec.vals<-sapply(1:10000*10,function(t){
    myseed<-sample.int(maxval, 1)
    val<-runif(1,0,1)
    return(val)
  })
  #check to see if there's a difference in these two samples
  return(t.test(consec.vals,nonconsec.vals)$p.value)
})
#then repeat that 100 times.

#great! so we might expect a difference in means if there's any bias
hist(t.tests)
#Nope. seems to be evenly distributed. We're good.