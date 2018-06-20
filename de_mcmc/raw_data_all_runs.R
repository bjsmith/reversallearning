
source("raw_data_wrapper_for_brandon_amendment2.R")

#all runs.
#rawdata<-rawdata[rawdata$Motivation=="reward" & rawdata$runid==1,]
subs=sort(unique(rawdata[,"subid"]))
S=length(subs)

rawdata.dt<-data.table(rawdata)
rawdata.dt[,runmotive:=paste0(Motivation,runid)]

data=NULL
# i'm pretty sure some of the run information needs to be in here. 
for(j in 1:S){#j=1
  s<-subs[j]
  subj_group<-gsub(pattern = " ","",as.character(unique(rawdata.dt[subid==subs[j]]$RiskLabel)))
  #rename groups to make them easier to process - take the spaces out of the names.
  
  data[[j]]<-list("group"=subj_group,"runs"=list(),"SubID"=paste0("SUB",s))
  
  s_unique_motivations<-unique(rawdata.dt[subid==s,Motivation])
  #list the Motivations this subject has
  for (m in s_unique_motivations){#m="reward"
    s_unique_runids<-unique(rawdata.dt[subid==s & Motivation==m,runid]) #list the run IDS for this subject*motivation.
    for (m_r in s_unique_runids){#r=1
      #for now just focus on reward run 1
      #if(r==1 & m=="reward"){

      #i'm going to do this hierarchically.
      temp.data=rawdata.dt[subid==s & runid==m_r & Motivation==m,]
      rt=temp.data[,reaction_time]
      rt[rt==0] <- NA
      #we're going to create a run. it will be indexed 
      data[[j]][["runs"]][[length(data[[j]][["runs"]])+1]]=list(
        "cue"=as.vector(temp.data[,cue]),
        "choice"=as.vector(temp.data[,choice]),
        "rt"=rt,"outcome"=as.vector(temp.data[,outcome]),
        "motivation"=m,
        "runid"=m_r
        )
      #}
    }
  }
}

#take a look to see if thsi has correctly formed.
#seems to have.
#lapply(data,function(x){table(unlist(lapply(x[["runs"]],function(y){y[["motivation"]]})))})
print(table(unlist(lapply(data,function(x){paste(unlist(lapply(x[["runs"]],function(y){y[["motivation"]]})),collapse=", ")}))))

