outcome_types=factor(c("Reward","Punishment"),ordered=TRUE)
outcome_types_abbrev=factor(c("rew","pun"),ordered=TRUE)
parameters=factor(c("alpha","beta"),ordered=TRUE)


data_summarize_double_update_rpo_repeated_runs<- function(extracted.data,comprehensive=FALSE){
  grouplevel.df<-NULL
  
  #create a data table storing subject-level values.
  for (ot in 1:length(outcome_types)){
    #print(ot)
    #print(outcome_types[ot])
    #cycle through runs,
    #create df
    item.count<-dim(extracted.data$mu_p)[1]
    for (p in 1:length(parameters)){
      parameter<-parameters[p]
      for (r in 1:dim(extracted.data$mu_p)[2]){
        if(r==1){
          sigma_vals<-extracted.data$sigma[,ot,p] #20171027: Seems to be correctly ordered
          sigma_stat<-"sigma"
        }else{
          sigma_vals<-extracted.data$sigma_rm[,ot,r-1,p] #20171027: Seems to be correctly ordered
          sigma_stat<-"diff_sigma"
        }
        #print(paste(r,p))
        otr.df<-rbind(
          data.frame("iter"=1:item.count,
                     "Motivation"=rep(outcome_types[ot],item.count),
                     "Run"=rep(r,item.count),
                     "Statistic"="mu",
                     "Parameter"=rep(parameter,item.count),
                     "Value"=extracted.data[[paste0("mu_",parameter)]][,r,ot]
                     #20171027: there was a mistake here, before; I have hopefully fixed it now.seems to be a mistake!
          ),
          data.frame("iter"=1:item.count,
                     "Motivation"=rep(outcome_types[ot],item.count),
                     "Run"=rep(r,item.count),
                     "Statistic"=sigma_stat,
                     "Parameter"=rep(parameter,item.count),
                     "Value"=sigma_vals
          )
        )
        #add the data
        if(is.null(grouplevel.df)){
          grouplevel.df<-otr.df
        }else{
          grouplevel.df<-rbind(grouplevel.df,otr.df)
        }
      }
      #runs weren't recorded for these.
      if(comprehensive){
        #we won't usually need this, but I'm trying to do some error checking
        #our generated quantities for Run2, beta mu (particularly) are not going well
        #and I want to see what's wrong. bjs20171027
        otr.df.comprehensive<-rbind(data.frame(
          "iter"=1:item.count,
          "Motivation"=as.character(ot),
          "Run"=NA,
          "Statistic"="mu_p",
          "Parameter"=as.character(p),
          "Value"=extracted.data[["mu_p"]][,ot,p]
        ),data.frame(
          "iter"=1:item.count,
          "Motivation"=as.character(ot),
          "Run"=NA,
          "Statistic"="mu_p_rm",
          "Parameter"=as.character(p),
          "Value"=extracted.data[["mu_p_rm"]][,ot,1,p]
        )
        )
        grouplevel.df<-rbind(grouplevel.df,otr.df.comprehensive)
      }
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rp<- function(extracted.data,run=NA){
  grouplevel.df<-NULL
  #create a data table storing subject-level values.
  for (ot in 1:length(outcome_types)){
    #print(ot)
    #print(outcome_types[ot])
    # #cycle through runs,
    # for (r in 1:dim(extracted.data$mu_p)[2]){
      #create df
      item.count<-dim(extracted.data$mu_p)[1]
      for (p in 1:length(parameters)){
        parameter<-parameters[p]
        sigma_vals<-extracted.data$sigma[,ot,p]
        sigma_stat<-"sigma"
        
        otr.df<-rbind(
          data.frame("iter"=1:item.count,
                     "Motivation"=rep(outcome_types[ot],item.count),
                     "Run"=run,
                     "Statistic"="mu",
                     "Parameter"=rep(parameter,item.count),
                     "Value"=extracted.data[[paste0("mu_",parameter,"_",outcome_types_abbrev[ot])]]
          ),
          data.frame("iter"=1:item.count,
                     "Motivation"=rep(outcome_types[ot],item.count),
                     "Run"=run,
                     "Statistic"=sigma_stat,
                     "Parameter"=rep(parameter,item.count),
                     "Value"=sigma_vals
          )
        )
        #add the data
        if(is.null(grouplevel.df)){
          grouplevel.df<-otr.df
        }else{
          grouplevel.df<-rbind(grouplevel.df,otr.df)
        }
      # }
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rev2_repeated_runs<- function(extracted.data,outcome.type=NA,run=NA,comprehensive=FALSE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  
  parameters=factor(c("alpha","beta"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$mu_p)[1]#how many iterations of the sample are we looking at?
    otr.df<-rbind(
      data.frame("iter"=1:sample.count,
                 #"Motivation"=outcome_types[outcome.type],#no motivation
                 "Run"=run,
                 "Statistic"="mu",
                 "Parameter"=as.factor(rep(parameter,sample.count)),
                 "Value"=extracted.data[[paste0("mu_",parameter)]]
      ),data.frame("iter"=1:sample.count,
                   #"Motivation"=outcome_types[outcome.type],#no motivation
                   "Run"=run,
                   "Statistic"="sigma",
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data[[paste0("sigma_",parameter)]]
      ))
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rev3<- function(extracted.data,outcome.type=NA,comprehensive=FALSE,run.level.data=TRUE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  
  parameters=factor(c("alpha","beta"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$s_mu_g_mu)[1]#how many iterations of the sample are we looking at?
    otr.df<-rbind(
      data.frame("iter"=1:sample.count,
                 #"Motivation"=outcome_types[outcome.type],#no motivation
                 "Run"="All",
                 "Statistic"="mu",
                 "Parameter"=as.factor(rep(parameter,sample.count)),
                 "Value"=extracted.data$s_mu_g_mu[,p]
      ),data.frame("iter"=1:sample.count,
                   #"Motivation"=outcome_types[outcome.type],#no motivation
                   "Run"="All",
                   "Statistic"="sigma",
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data$s_mu_g_sigma[,p]
      ))
    
    #now add the run level data if it is wanted.
    if(run.level.data){
      for (r in 1:2){
        otr.df<-cbind(
          otr.df,
          rbind(
            data.frame("iter"=1:sample.count,
                       #"Motivation"=outcome_types[outcome.type],#no motivation
                       "Run"="All",
                       "Statistic"="mu",
                       "Parameter"=as.factor(rep(parameter,sample.count)),
                       "Value"=extracted.data$s_mu_g_mu[,p]
            ),data.frame("iter"=1:sample.count,
                         #"Motivation"=outcome_types[outcome.type],#no motivation
                         "Run"="All",
                         "Statistic"="sigma",
                         "Parameter"=as.factor(rep(parameter,sample.count)),
                         "Value"=extracted.data$s_mu_g_sigma[,p]
            )))
      }
      
    }
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rev4<- function(extracted.data,outcome.type=NA,comprehensive=FALSE,run.level.data=TRUE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  parameters=factor(c("alpha","beta"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$group_mu_alpha)[1]#how many iterations of the sample are we looking at?
    otr.df<-rbind(
      data.frame("iter"=1:sample.count,
                 "Run"="All",
                 "Statistic"="mu",
                 "Parameter"=as.factor(rep(parameter,sample.count)),
                 "Value"=extracted.data[[paste0("group_mu_",parameter)]]
      ),data.frame("iter"=1:sample.count,
                   "Run"="All",
                   "Statistic"="sigma",
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data[[paste0("group_sigma_",parameter)]]
      ))
    
    #now add the run level data if it is wanted.
    if(run.level.data){
      for (r in 1:2){
        otr.df<-cbind(
          otr.df,
          rbind(
            data.frame("iter"=1:sample.count,
                       "Run"="All",
                       "Statistic"="mu",
                       "Parameter"=as.factor(rep(parameter,sample.count)),
                       "Value"=extracted.data[[paste0("group_mu_",parameter)]]
            ),data.frame("iter"=1:sample.count,
                         "Run"="All",
                         "Statistic"="sigma",
                         "Parameter"=as.factor(rep(parameter,sample.count)),
                         "Value"=extracted.data[[paste0("group_sigma_",parameter)]]
            )))
      }
      
    }
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rev5<- function(extracted.data,outcome.type=NA,comprehensive=FALSE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  parameters=factor(c("alpha","beta"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$group_mu_alpha)[1]#how many iterations of the sample are we looking at?
    otr.df<-NULL
    for (stat in c("mu", "sigma", "rew_mu","pun_mu")){
      otr.df.set<-
        data.frame("iter"=1:sample.count,
                   "Run"="All",
                   "Statistic"=stat,
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data[[paste0("group_",stat,"_",parameter)]]
        )
      if (is.null(otr.df)){
        otr.df<-otr.df.set
      }else{
        otr.df<-rbind(otr.df,otr.df.set)
      }
    }
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  
  #we oughtta do a comprehensive analysis. What would per-subject data look like?
  #or maybe for this, we don't need the summary, we need the original fits.
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rev5pain1<- function(extracted.data,outcome.type=NA,comprehensive=FALSE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  parameters=factor(c("alpha","beta","pain_effect"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$group_mu_alpha)[1]#how many iterations of the sample are we looking at?
    otr.df<-NULL
    if (parameter=="pain_effect"){
      stat_list<-c("mu", "sigma")
    }else{
      stat_list<-c("mu", "sigma", "rew_mu","pun_mu")
    }
    for (stat in stat_list){
      otr.df.set<-
        data.frame("iter"=1:sample.count,
                   "Run"="All",
                   "Statistic"=stat,
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data[[paste0("group_",stat,"_",parameter)]]
        )
      if (is.null(otr.df)){
        otr.df<-otr.df.set
      }else{
        otr.df<-rbind(otr.df,otr.df.set)
      }
    }
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  
  #we oughtta do a comprehensive analysis. What would per-subject data look like?
  #or maybe for this, we don't need the summary, we need the original fits.
  return(data.table(grouplevel.df))
}


data_summarize_double_update_rev6a<- function(extracted.data,outcome.type=NA,comprehensive=FALSE){
  grouplevel.df<-NULL
  #we want...
  #group level parameters
  
  parameters=factor(c("alpha","beta_1","beta_2"),ordered=TRUE)
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals <- extracted.data$sigma
    sample.count<-dim(extracted.data$group_mu_alpha)[1]#how many iterations of the sample are we looking at?
    otr.df<-NULL
    for (stat in c("mu", "sigma", "rew_mu","pun_mu")){
      otr.df.set<-
        data.frame("iter"=1:sample.count,
                   "Run"="All",
                   "Statistic"=stat,
                   "Parameter"=as.factor(rep(parameter,sample.count)),
                   "Value"=extracted.data[[paste0("group_",stat,"_",parameter)]]
        )
      if (is.null(otr.df)){
        otr.df<-otr.df.set
      }else{
        otr.df<-rbind(otr.df,otr.df.set)
      }
    }
    
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  
  #we oughtta do a comprehensive analysis. What would per-subject data look like?
  #or maybe for this, we don't need the summary, we need the original fits.
  return(data.table(grouplevel.df))
}

data_summarize_double_update<- function(extracted.data,outcome.type=NA,run=NA,comprehensive=FALSE){
  print(outcome.type)
  print(run)
  grouplevel.df<-NULL
  #outcome_types=factor(c("Reward","Punishment"),ordered=TRUE)
  #outcome_types_abbrev=factor(c("rew","pun"),ordered=TRUE)
  parameters=factor(c("alpha","beta"),ordered=TRUE)
  #create a data table storing subject-level values.
  item.count<-dim(extracted.data$mu_p)[1]
  for (p in 1:length(parameters)){
    parameter<-parameters[p]
    sigma_vals<-extracted.data$sigma[,p]
    sigma_stat<-"sigma"
    
    otr.df<-rbind(
      data.frame("iter"=1:item.count,
                 "Motivation"=outcome_types[outcome.type],
                 "Run"=run,
                 "Statistic"="mu",
                 "Parameter"=rep(parameter,item.count),
                 "Value"=extracted.data[[paste0("mu_",parameter)]]
      ),
      data.frame("iter"=1:item.count,
                 "Motivation"=outcome_types[outcome.type],
                 "Run"=run,
                 "Statistic"=sigma_stat,
                 "Parameter"=rep(parameter,item.count),
                 "Value"=sigma_vals
      )
    )
    if(comprehensive){
      otr.df.comprehensive<-data.frame(
        "iter"=1:item.count,
        "Motivation"=outcome_types[outcome.type],
        "Run"=run,
        "Statistic"="mu_p",
        "Parameter"=as.character(p),
        "Value"=extracted.data[["mu_p"]][,parameter]
      )
      otr.df<-rbind(otr.df,otr.df.comprehensive)
    }
    #add the data
    if(is.null(grouplevel.df)){
      grouplevel.df<-otr.df
    }else{
      grouplevel.df<-rbind(grouplevel.df,otr.df)
    }
  }
  return(data.table(grouplevel.df))
}



learningRateTable<-function(f,group_name){rbind(
  data.table(
    mean=f$mu_alpha_rew,
    variance=f$sigma[,1,1],mode="reward",Group=group_name),
  data.table(
    mean=f$mu_alpha_pun,
    variance=f$sigma[,2,1],
    mode="punishment",Group=group_name))
}


inverseTemperatureTable<-function(f,group_name){rbind(
  data.table(
    mean=f$mu_beta_rew,
    variance=f$sigma[,1,2],
    mode="reward",Group=group_name),
  data.table(
    mean=f$mu_beta_pun,
    variance=f$sigma[,2,2],
    mode="punishment",Group=group_name))
}

