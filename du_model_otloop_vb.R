
#We will estimate both double update and double update_rp_rr
#double update will be estimated separately, 4 times, for each of Run1_rew, run2_rew, Run1_pun, run2_pun.
#just the two subject groups.
print("initializing...")
source("util/get_my_preferred_cores.R")
options(mc.cores = get_my_preferred_cores())
#options(mc.cores = NULL)
#source files
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")

#set settings.
models_to_run<-c("double_update_rpo_repeated_runs_ntp_otmod","double_update_rpo_repeated_runs_notrialpost")
estimation_methods<-ESTIMATION_METHOD.VariationalBayes#c(as.character(ESTIMATION_METHOD.MCMC))#rev(ESTIMATION_METHODS)

subject_groups<-2:3


times_to_run<-12
#run.
summaryfilepath<-paste0(localsettings$data.dir,"du_model_allruns_rp_otloop_vb12_20171023.RData")

model.summaries <- vector("list", length(subject_groups)*times_to_run*(2))
model.stanfits <- vector("list", length(subject_groups)*times_to_run*(2))

if(file.exists(summaryfilepath)){
  load(file=summaryfilepath)
}

if(any(sapply(model.summaries,is.null))){
  for (em in estimation_methods){
    if (em==as.character(ESTIMATION_METHOD.MCMC)){
      iterations<-5000
      warmup_iter=1000
    }else if (em==as.character(ESTIMATION_METHOD.VariationalBayes)){
      iterations<-50000
      warmup_iter=NA
    }else{
      times_to_run=0
    }
    for (t in 1:times_to_run){
      for (m in models_to_run){
        for (g in subject_groups){
          print (paste0(g,m,t,collapse=", "))
          #we will loop just once for the model that handles everything at once
          #but four times for the model that needs to process runs and reward/punishment separately.
          if(m %in% c("double_update_rpo_repeated_runs_notrialpost", "double_update_rpo_repeated_runs_ntp_otmod")){
            runlist<-list(c(1,2))
            rp_list<-list(c(1,2))
          }else if (m %in% c("double_update_notrialpost")){
            #only run reward and punishment when we can
            runlist<-list(c(1),c(2))
            rp_list<-list(c(1),c(2))
          }
          
          #for the model that takes both simultaneously, this will pass all the parameters at once
          #but for the model that needs them separately, this will run through four times.
          for (rp in rp_list){
            for(runs in runlist){
              print(paste0("rp:",paste(rp),"; runs:",paste(runs)))
              
              generatePosteriorTrialPredictions=NA
              #run the model
              fitpackage<-lookupOrRunFit(
                run=runs,groups_to_fit=g, model_to_use=m,includeSubjGroup = FALSE,
                rp=rp,
                model_rp_separately=TRUE,model_runs_separately = TRUE, include_pain=FALSE,
                fastDebug=FALSE,
                fileSuffix=paste0("compare_otp_loop_vb_20171023",as.character(t)),
                estimation_method = em,
                bseed=t+2012941342,#sample.int(.Machine$integer.max, 1)
                collateTrialData=FALSE,
                chainNum = 12,
                iterations = iterations,
                warmup_iter = warmup_iter
              )
              
              cat("...model loaded. Extracting...")
              #save just the output we want.
              first_empty_list_pos<-min(which(sapply(model.summaries,is.null)))
              print(paste("first_empty_list_pos is", as.character(first_empty_list_pos)))
              
              #get the non-extracted fit, and the stanfit
              first_empty_list_pos.stanfits<-min(which(sapply(model.stanfits,is.null)))
              model.stanfits[[first_empty_list_pos]]<-fitpackage$fit
              
              extractedfit<-rstan::extract(fitpackage$fit)
              # first_empty_list_pos.extractedfit<-min(which(sapply(model.extractedfits,is.null)))
              # model.extractedfits[[first_empty_list_pos]]<-extractedfit
              
              if(m %in% c("double_update_rpo_repeated_runs", "double_update_rpo_repeated_runs_notrialpost", "double_update_rpo_repeated_runs_ntp_otmod")){
                model.summaries[[first_empty_list_pos]]<-
                  list("summaryObj"=data_summarize_double_update_rpo_repeated_runs(extractedfit),
                       "g"=g,"m"=m,"t"=t,"EstimationMethod"=em,"elapsedTime"=fitpackage$general_info$estimation_duration)
              }else if(m=="double_update" || m=="double_update_notrialpost"){
                model.summaries[[first_empty_list_pos]]<-
                  list("summaryObj"=data_summarize_double_update(extractedfit,
                                                                 outcome.type = rp,
                                                                 run = runs),
                       "g"=g,"m"=m,"t"=t,"EstimationMethod"=em,"elapsedTime"=fitpackage$general_info$estimation_duration)
              }else{
                print(m)
                stop("f^<%! I don't recognize that model")
              }
              #remove the fit object from memory, because it is pretty large!
              rm(fitpackage)
              print("...summary data extracted.")
            }
          }
        }
      }
    }
  }
  save(model.summaries,model.stanfits,file=summaryfilepath)
}
