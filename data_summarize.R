outcome_types=factor(c("Reward","Punishment"),ordered=TRUE)
outcome_types_abbrev=factor(c("rew","pun"),ordered=TRUE)
parameters=factor(c("alpha","beta"),ordered=TRUE)
data_summarize_double_update_rpo_repeated_runs<- function(extracted.data){
  grouplevel.df<-NULL
  
  #create a data table storing subject-level values.
  for (ot in 1:length(outcome_types)){
    print(ot)
    print(outcome_types[ot])
    #cycle through runs,
    for (r in 1:dim(extracted.data$mu_p)[2]){
      #create df
      item.count<-dim(extracted.data$mu_p)[1]
      for (p in 1:length(parameters)){
        parameter<-parameters[p]
        if(r==1){
          sigma_vals<-extracted.data$sigma[,ot,p]
          sigma_stat<-"sigma"
        }else{
          sigma_vals<-extracted.data$sigma_rm[,ot,r-1,p]
          sigma_stat<-"diff_sigma"
        }
        
        otr.df<-rbind(
          data.frame("iter"=1:item.count,
                     "Motivation"=rep(outcome_types[ot],item.count),
                     "Run"=rep(r,item.count),
                     "Statistic"="mu",
                     "Parameter"=rep(parameter,item.count),
                     "Value"=extracted.data[[paste0("mu_",parameter)]][,ot,r]
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
    }
  }
  return(data.table(grouplevel.df))
}

data_summarize_double_update_rp<- function(extracted.data,run=NA){
  grouplevel.df<-NULL
  #create a data table storing subject-level values.
  for (ot in 1:length(outcome_types)){
    print(ot)
    print(outcome_types[ot])
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

data_summarize_double_update<- function(extracted.data,outcome.type=NA,run=NA){
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
                 "Motivation"=outcome_types_abbrev[outcome.type],
                 "Run"=run,
                 "Statistic"="mu",
                 "Parameter"=rep(parameter,item.count),
                 "Value"=extracted.data[[paste0("mu_",parameter)]]
      ),
      data.frame("iter"=1:item.count,
                 "Motivation"=outcome_types_abbrev[outcome.type],
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

