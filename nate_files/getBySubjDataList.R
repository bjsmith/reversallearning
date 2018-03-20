library(data.table)
#takes a dataList from fitGroupsV3Onegroup.
getBySubjDataList <- function(dataList){
  #nSubs=dim(rl.all.subjects.list[,.N,by=subid][N>0])[1]
  warning("currently the subject datalist only includes values actually needed in DE-MCMC. It doesn't include the wider set of values that we pass to stan. Many of those are redundant.")
  #dataList <- list()
  #names(dataList)
  #so, let's 
  bySubjDataList <- vector("list",dataList$N)
  for (s in 1:length(bySubjDataList)){

    #everything we need for a single-run model
    bySubjDataList$choice<-dataList$choice[s,]
    bySubjDataList$outcome<-dataList$outcome[s,]
    bySubjDataList$cue<-dataList$cue[s,]
    
    #for a hierarchical moel we'll also need:
    #s<-2
    bySubjDataList[[s]]<-list()
    bySubjDataList$run_id<-dataList$run_id[s,]
    bySubjDataList$run_ot<-dataList$run_id[s,]
    
  }
  
  
}

