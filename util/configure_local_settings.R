configure_local_settings<-function(){
  data.dir <- readline("Please enter the directory for data.")
  localsettings<-list(data.dir)
  save(localsettings,file="Rlocalsettings.RData")
  print("Thank you. You might need to restart your project for some settings to take effect.")
}