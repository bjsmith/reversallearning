print(paste0("running on ",Sys.info()["nodename"]))
options(mc.cores = ceiling(parallel::detectCores()/2))
#options(mc.cores = NULL)
#source files
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")
#set settings.
models_to_run<-c("double_update","double_update_notrialpost")
estimation_methods<-ESTIMATION_METHODS
times_to_run=3

#run.
summaryfilepath<-paste0(localsettings$data.dir,"comparedoubleUpdate_mcmc_notrialposteriors.RData")

model.summaries <- vector("list", 2*length(models_to_run)*times_to_run*length(estimation_methods))
#model.extractedfits <- vector("list", 2*length(models_to_run)*times_to_run)
model.stanfits <- vector("list", 2*length(models_to_run)*times_to_run)
if(file.exists(summaryfilepath)){
  print("Loading cached version of this.")
  load(file=summaryfilepath)
}

if(any(sapply(model.summaries,is.null))){
  for (t in 1:times_to_run){
    for (m in models_to_run){
      for (em in estimation_methods){
        for (g in 2:3){
          print (paste0(g,m,em,t,sep=", "))
          #only run reward and punishment when we can
          if(models_to_run %in% c("double_update_rpo_repeated_runs")){
            rp<-c(1,2)
          }else{
            rp<-c(2)
          }
          #only run multiple runs when we can
          if(models_to_run %in% c("double_update_rpo_repeated_runs")){
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
            fileSuffix=paste0("20171005_mcmc_test_iteration",as.character(t)),
            estimation_method = em,
            bseed=t+972689397,
            collateTrialData=FALSE
            )
          
          cat("...model loaded. Extracting...")
          #save just the output we want.
          first_empty_list_pos<-min(which(sapply(model.summaries,is.null)))
          print(paste("first_empty_list_pos is", as.character(first_empty_list_pos)))
          
          
          if(m=="double_update_rpo_repeated_runs"){
            model.summaries[[first_empty_list_pos]]<-
            list("summaryObj"=data_summarize_double_update_rpo_repeated_runs(rstan::extract(fitpackage$fit)),
            "g"=g,"m"=m,"t"=t,"elapsedTime"=fitpackage$general_info$estimation_duration)
          }else if(m=="double_update_rp_erroneous" || m=="double_update_rp_fixed"){
            model.summaries[[first_empty_list_pos]]<-
            list("summaryObj"=data_summarize_double_update_rp(rstan::extract(fitpackage$fit),
            run = runs),
            "g"=g,"m"=m,"t"=t,"elapsedTime"=fitpackage$general_info$estimation_duration)
          }else if(m=="double_update" || m=="double_update_notrialpost"){
            extractedfit<-rstan::extract(fitpackage$fit)
            model.summaries[[first_empty_list_pos]]<-
            list("summaryObj"=data_summarize_double_update(extractedfit,
            outcome.type = rp,
            run = runs),
            "g"=g,"m"=m,"t"=t,"elapsedTime"=fitpackage$general_info$estimation_duration)
            if(m=="double_update_notrialpost"){#far too resource intesnive to do this for tiral posteriors too
              first_empty_list_pos.extractedfit<-min(which(sapply(model.extractedfits,is.null)))
              first_empty_list_pos.stanfits<-min(which(sapply(model.stanfits,is.null)))
              model.stanfits[[first_empty_list_pos]]<-fitpackage$fit
              #model.extractedfits[[first_empty_list_pos]]<-extractedfit
            }
          }else{
            stop("f^<%! I don't recognize that model.")
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
