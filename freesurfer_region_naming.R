freesurfer_region_naming<-function(textcode){
  return(unlist(sapply(regions,freesurfer_region_name)))
}

freesurfer_region_name<-function(textcode){
  regname<-gsub("ROI ", "",gsub("\\.", " ",gsub("_"," ",gsub("ROI_ctx_","", textcode))))
  print(regname)
  
  if (length(grep(" G and S ",regname))>0){
    #take it out
    regname<-paste0(gsub(" G and S","",regname), " gyrus and sulcus")
    #append "Right" on end.
  }
  
  if (length(grep(" G ",regname))>0){
    #take it out
    regname<-paste0(gsub(" G","",regname), " gyrus")
    #append "Right" on end.
  }
  if (length(grep(" S ",regname))>0){
    #take it out
    regname<-paste0(gsub(" S","",regname), " sulcus")
    #append "Right" on end.
  }
  
  if (length(grep("rh ",regname))>0){
    #take it out
    regname<-paste0(gsub("rh ","",regname), " (Right)")
    #append "Right" on end.
  }
  if (length(grep("lh ",regname))>0){
    #take it out
    regname<-paste0(gsub("lh ","",regname), " (Left)")
  }
  
  if (length(grep("Left ",regname))>0){
    #take it out
    regname<-paste0(gsub("Left ","",regname), " (Left)")
    #append "Right" on end.
  }
  if (length(grep("Right ",regname))>0){
    #take it out
    regname<-paste0(gsub("Right ","",regname), " (Right)")
  }
  
  
  return(regname)
}

