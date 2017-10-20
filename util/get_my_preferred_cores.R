get_my_preferred_cores<-function(){
  if(Sys.info()["nodename"]=="MSM1" || Sys.info()["nodename"]=="MSM2" || Sys.info()["nodename"]=="MSMServer"){
    return(ceiling(parallel::detectCores()))#use all the cores on these machines.
  }
  return(ceiling(parallel::detectCores()/2))#half otherwise.
}
#