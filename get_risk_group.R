get_risk_group <- function (){
  cwd<-getwd()
  group.data<-NULL
  file.data<-paste0(localsettings$data.dir,"group.data.csv")
  if (file.exists(file.data)){
    group.data<-read.csv(file=file.data)
  }else{
    source("/Users/benjaminsmith/Documents/msm-project/behavioral-analysis/load_real_life_measures_v2.R")
    measures.rl = load_real_life_measures_v2()
    group.data<-measures.rl[,c("Adjusted_subid","RiskCat","RiskLabel")]
    group.data$MethUse<-!grepl("No Meth",as.character(group.data$RiskLabel))
    group.data$SexRisk<-grepl("Risky",as.character(group.data$RiskLabel))
    write.csv(group.data,file = file.data,row.names = FALSE)
  }
  setwd(cwd)
  return(group.data)
}
