configure_local_settings<-function(){
  data.dir <- readline("Please enter the directory for data or enter a blank string to cancel.")
  if(data.dir==""){
    print("Aborted.")
  }else{
    localsettings<-list(data.dir=data.dir)
    save(localsettings,file="Rlocalsettings.RData")
    print("Thank you. You might need to restart your project for some settings to take effect.")
  }
  
}