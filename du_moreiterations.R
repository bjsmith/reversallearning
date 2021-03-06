#Purposes:
#1) Compare stan notrial posterior doubleupdate model with one with reward/punishment and both runs
#2) verify that the latter will actually run on MSMServer.
#3) Compare results for VariationalBayes and MCMC
print("initializing...")
source("util/get_my_preferred_cores.R")
options(mc.cores = get_my_preferred_cores())
#options(mc.cores = NULL)
#source files
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")

#set settings.
models_to_run<-c("double_update_notrialpost","double_update_rpo_repeated_runs_notrialpost")
estimation_methods<-ESTIMATION_METHODS#c(as.character(ESTIMATION_METHOD.MCMC))
times_to_run=3
subject_groups<-2:3

#should update this to show VariationalBayes too! 

#run.
summaryfilepath<-paste0(localsettings$data.dir,"compare_notrialpost_du_du_rp_rr.RData")

model.summaries <- vector("list", length(subject_groups)*length(models_to_run)*times_to_run*length(estimation_methods))
model.extractedfits <- vector("list", length(subject_groups)*length(models_to_run)*times_to_run)
model.stanfits <- vector("list", length(subject_groups)*length(models_to_run)*times_to_run)

if(file.exists(summaryfilepath)){
  load(file=summaryfilepath)
}

if(any(sapply(model.summaries,is.null))){
  for (t in 1:times_to_run){
    for (m in models_to_run){
      for (em in estimation_methods){
        for (g in subject_groups){
          print (paste0(g,m,t,collapse=", "))
          #only run reward and punishment when we can
          if(m %in% c("double_update_rpo_repeated_runs","double_update_rpo_repeated_runs_notrialpost")){
            rp<-c(1,2)
          }else{
            rp<-c(2)
          }
          #only run multiple runs when we can
          if(m %in% c("double_update_rpo_repeated_runs","double_update_rpo_repeated_runs_notrialpost")){
            runs=c(1,2)
            generatePosteriorTrialPredictions=FALSE
          }else{
            runs=c(1)
            
          }
          generatePosteriorTrialPredictions=NA
          #run the model
          fitpackage<-lookupOrRunFit(
            run=runs,groups_to_fit=g, model_to_use=m,includeSubjGroup = FALSE,
            rp=rp,
            model_rp_separately=TRUE,model_runs_separately = TRUE, include_pain=FALSE,
            fastDebug=FALSE,
            fileSuffix=paste0("20171005_compare_basic_extended_test_iteration",as.character(t)),
            estimation_method = em,
            bseed=t+503489401,#sample.int(.Machine$integer.max, 1)
            collateTrialData=FALSE
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

          if(m %in% c("double_update_rpo_repeated_runs", "double_update_rpo_repeated_runs_notrialpost")){
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
  save(model.summaries,model.extractedfits,model.stanfits,file=summaryfilepath)
}
