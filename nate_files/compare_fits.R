source("Misc/plot_model.R")
#this file loads each of the fits and calculates how well they do in order to compare them to one another.

stored.fits<-list.files("Fits/",".RData",ignore.case = TRUE)
for (fn in stored.fits){
  #fn<-stored.fits[[1]]
  load(paste0("Fits/",fn))
  print(fn)
  print(dim(fit_data$plot_object))
  print(paste0("subjects:",
      length(unique(fit_data$plot_object$subjID))))
  #print(summary(fit_data$plot_object))
  #alright what else do we want to know about these ?
  #Would be interesting to plot each one.
  tryCatch({
    plot_model(fit_data)
    },
    error=function(err){
      print (paste0("couldn't fit model; error:",err))
    })
}