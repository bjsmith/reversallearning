
source("raw_data_wrapper_for_brandon.R")

#for now just focus on reward run 1
rawdata<-rawdata[rawdata$Motivation=="reward" & rawdata$runid==1,]
subs=sort(unique(rawdata[,"subid"]))
S=length(subs)

rawdata.dt<-data.table(rawdata)
rawdata.dt[,runmotive:=paste0(Motivation,runid)]

data=NULL
# i'm pretty sure some of the run information needs to be in here. 
for(j in 1:S){#j=1
  s<-subs[j]
  for (r in unique(rawdata.dt[subid==s,runid])){#r=1
    for (m in unique(rawdata.dt[subid==s&runid==r,Motivation])){#m="reward"
      #for now just focus on reward run 1
      if(r==1 & m=="reward"){
        temp.data=rawdata.dt[subid==s & runid==r & Motivation==m,]
        rt=temp.data[,reaction_time]
        rt[rt==0] <- NA
        data[[j]]=list("cue"=as.vector(temp.data[,cue]),"choice"=as.vector(temp.data[,choice]),"rt"=rt,"outcome"=as.vector(temp.data[,outcome]))
      }
    }
  }
}
