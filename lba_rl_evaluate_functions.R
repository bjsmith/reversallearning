runs_missing<-function(rawdata,rsdt,printdata=TRUE){
  #we need to quantify how many runs missed out because we couldn't calculate good values for them.
  all.runs<-rawdata[,.N,.(subid,Motivation,runid)]
  included_runs<-rsdt[,.N,.(sid,motivation,rid)]
  length(unique(included_runs$sid))
  all.subjs<-rawdata[,.N,.(subid)]
  included.subjs<-rsdt[,.N,.(sid)]
  rmd<-list("runs_missing"=dim(all.runs)[1]-dim(included_runs)[1],
            "subs_missing"=dim(all.subjs)[1]-dim(included.subjs)[1])
  
  if(printdata){
    print(paste0(as.character(rmd$runs_missing), 
                 " runs missing and ",as.character(rmd$subs_missing)," subjects missing altogether because we couldn't calculate parameters for them. (alternatively, estimation isn't complete yet)"))
    #that's an awful lot of runs! We need to get them one by one and work out what's going wrong because I don't think I can proceed without these in the calculations. It could be a substantial bias to the model to just throw out all the data that doesn't fit.
  }else{
    return(rmd)
  }
  
}

do_matrix_t_tests<-function(theta_range=1:2,delta_range=3:6,theta_names=c("RPE","EV")){
  ttests<-vector("list",length(theta_range)*length(delta_range))
  for (t in theta_range){
    for(i in delta_range){#i<-2
      tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
      testres.dt<-data.table(t(data.table(unlist(tesres))))
      colnames(testres.dt)<-names(unlist(tesres))
      testres.dt$Region<-freesurfer_region_naming(regions)[i-2]
      testres.dt$Theta<-theta_names[t]
      ttests[[i-2+(t-1)*max(delta_range)]]<-testres.dt
    }
  }
  
  testres.dt<-rbindlist(ttests)
  return(testres.dt)
}
