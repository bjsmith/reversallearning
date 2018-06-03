library(data.table)
library(dplyr)
source("raw_data_wrapper_for_brandon_amendment1.R")

#for now just focus on reward run 1
#rawdata<-rawdata[rawdata$Motivation=="reward" & rawdata$runid==1,]
subs=sort(unique(rawdata[,"subid"]))
S=length(subs)

rawdata.dt<-data.table(rawdata)
#so let's just throw out the second run for the one subject which repeated a run.

rawdata.dt<-rawdata.dt[fileid!="sub352_run2_19-Sep_12-56",]
subj.to.clean<-unique(rawdata.dt[,.(CueBySubCount=.N),by=.(subid,cue)] %>% .[CueBySubCount>13,subid])
if(length(subj.to.clean)>0)stop("found a subject with cues across more than one run. gotta stop this shit.")


data=NULL
# i'm pretty sure some of the run information needs to be in here. 
for(j in 1:S){#j=1
  s<-subs[j]
  subj_group<-as.character(unique(rawdata.dt[subid==subs[j]]$RiskLabel))
  #for (r in unique(rawdata.dt[subid==s,runid])){#r=1
#    for (m in unique(rawdata.dt[subid==s&runid==r,Motivation])){#m="reward"
      #for now just focus on reward run 1
      #if(r==1 & m=="reward"){
      #temp.data=rawdata.dt[subid==s & runid==r & Motivation==m,]
      temp.data=rawdata.dt[subid==s,]
      
        #here we're covering all runs at once!
        #this works because there's no overlap between different cues between runs. Every run uses an entirely distinct set of cues. See
      #temp.data[order(image),.N,by=.(image,Motivation,runid)]
      
      rt=temp.data[,reaction_time]
      rt[rt==0] <- NA
      
      data[[j]]=list("cue"=as.vector(temp.data[,cue]),"choice"=as.vector(temp.data[,choice]),"rt"=rt,"outcome"=as.vector(temp.data[,outcome]),
                     "group"=subj_group)
      #}
    #}
  #}
}
