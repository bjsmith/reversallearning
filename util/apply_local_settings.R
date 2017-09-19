apply_local_settings<-function(reldir=""){
  if (file.exists(paste0(reldir,"Rlocalsettings.RData"))){
    load(paste0(reldir,"Rlocalsettings.RData"))
  }else{
    stop("Couldn't find local settings. Check that the current directory is project home!")
  }
}