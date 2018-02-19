apply_local_settings<-function(reldir=""){
  if (file.exists(paste0(reldir,"Rlocalsettings.RData"))){
    load(paste0(reldir,"Rlocalsettings.RData"),envir = .GlobalEnv)
  }else{
    stop("Couldn't find local settings. Check that the current directory is project home!")
  }
}


configure_local_settings<-function(){
  setting_key<-readline("Please enter the KEY for the setting you want to change, or enter a blank string to cancel.")
  setting_value <- readline("Please enter the VALUE for the setting you want to change, or enter a blank string to cancel.")
  if(setting_key=="" || setting_value==""){
    print("Aborted.")
  }else{
    load("Rlocalsettings.RData",envir = .GlobalEnv)
    localsettings[[setting_key]] = setting_value
    save(localsettings,file="Rlocalsettings.RData")
    print("Thank you. You might need to restart your project for some settings to take effect.")
  }
  
}