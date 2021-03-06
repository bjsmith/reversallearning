#Purposes:
#1) See whether running a model with four runs seems to be better than the two model run previously run.

print("initializing...")
source("util/get_my_preferred_cores.R")
options(mc.cores = get_my_preferred_cores())
#options(mc.cores = NULL)
#source files
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")

#set settings.
models_to_run<-c("double_update_notrialpost",#"double_update_nov_rev2-d",#"double_update_nov_rev2-c",
                 "double_update_nov_rev2-a-a")
estimation_methods<-c(as.character(ESTIMATION_METHOD.MCMC),as.character(ESTIMATION_METHOD.VariationalBayes))#rev(ESTIMATION_METHODS)

subject_groups<-2:3


times_to_run<-3
#run.
summaryfilepath<-paste0(localsettings$data.dir,"du_model_revised_4runs_20171206_2aa.RData")

models.with.4.separate.runs.count<-1
models.with.runs.considered.together.count<-1
total.models.count<-length(estimation_methods)*times_to_run*length(subject_groups)*(models.with.4.separate.runs.count*4+models.with.runs.considered.together.count)
model.summaries <- vector("list", total.models.count)
model.stanfits <- vector("list", total.models.count)

if(file.exists(summaryfilepath)){
  load(file=summaryfilepath)
}

#print("Starting main loop...")
if(any(sapply(model.summaries,is.null))){
  for (em in estimation_methods){
    if (em==as.character(ESTIMATION_METHOD.MCMC)){
      iterations<-5000
      warmup_iter=1000
    }else if (em==as.character(ESTIMATION_METHOD.VariationalBayes)){
      iterations<-10000
      warmup_iter=NA
    }else{
      times_to_run=0
    }
    for (t in 1:times_to_run){
      for (m in models_to_run){
        for (g in subject_groups){
          print (paste0("g:",g,";m:",m,";t:",t,collapse=", "))
          variable_run_lengths=FALSE
          #we will loop just once for the model that handles everything at once
          #but four times for the model that needs to process runs and reward/punishment separately.
          if(m %in% c("double_update_rpo_repeated_runs_notrialpost", "double_update_rpo_repeated_runs_ntp_otmod",
                      "double_update_nov_rev2","double_update_nov_rev2-a-a","double_update_nov_rev2-b",
                      "double_update_nov_rev2-c","double_update_nov_rev2-d")){
            runlist<-list(c(1,2))
            rp_list<-list(c(1,2))
            if(m %in% c("double_update_nov_rev2","double_update_nov_rev2-a-a","double_update_nov_rev2-b",
                        "double_update_nov_rev2-c","double_update_nov_rev2-d")){
              rl_unique_runids=TRUE
              if(m %in% c("double_update_nov_rev2-a-a")){
                variable_run_lengths=TRUE
              }
            }
          }else if (m %in% c("double_update_notrialpost")){
            #only run reward and punishment when we can
            runlist<-list(c(1),c(2))
            rp_list<-list(c(1),c(2))
            rl_unique_runids=NA
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
                model_rp_separately=FALSE,model_runs_separately = TRUE, include_pain=FALSE,
                fastDebug=FALSE,
                fileSuffix=paste0("rev2aa_20171206",as.character(t)),
                estimation_method = em,
                bseed=t+2028347558,#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
                collateTrialData=FALSE,
                chainNum = 12,
                iterations = iterations,
                warmup_iter = warmup_iter,
                rl_unique_runids=TRUE,
                variable_run_lengths=variable_run_lengths
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
                  list("summaryObj"=data_summarize_double_update_rpo_repeated_runs(extractedfit,comprehensive=TRUE),
                       "g"=g,"m"=m,"t"=t,"EstimationMethod"=em,"elapsedTime"=fitpackage$general_info$estimation_duration)
              }else if(m=="double_update" || m=="double_update_notrialpost"){
                model.summaries[[first_empty_list_pos]]<-
                  list("summaryObj"=data_summarize_double_update(extractedfit,comprehensive=TRUE,
                                                                 outcome.type = rp,
                                                                 run = runs),
                       "g"=g,"m"=m,"t"=t,"EstimationMethod"=em,"elapsedTime"=fitpackage$general_info$estimation_duration)
              }else if(m %in% c("double_update_nov_rev2","double_update_nov_rev2-a-a","double_update_nov_rev2-c","double_update_nov_rev2-d")){
                model.summaries[[first_empty_list_pos]]<-
                  list("summaryObj"=data_summarize_double_update_rev2_repeated_runs(extractedfit,comprehensive=TRUE,
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
              # first_empty_list_pos<-min(which(sapply(model.summaries,is.null)))

            }
          }
        }
      }
    }
  }
  save(model.summaries,model.stanfits,file=summaryfilepath)
}
