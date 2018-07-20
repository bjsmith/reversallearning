
source("raw_data_wrapper_for_brandon_amendment2.R")

#for now just focus on reward run 1
#rawdata<-rawdata[rawdata$Motivation=="reward" & rawdata$runid==1,]
subs=sort(unique(rawdata[,"subid"]))
S=length(subs)

rawdata.dt<-data.table(rawdata)
rawdata.dt[,runmotive:=paste0(Motivation,runid)]
rawdata.dt[,.(TrialCount=.N),.(cue,subid)] %>% .[TrialCount>13,]
View(rawdata.dt[,,subid==352])
table(rawdata.dt[subid==352,.(fileid=as.character(fileid),cue=cue)])
#so let's just throw out the second run
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

max.cue.count<-sapply(data,function(d){max(table(d$cue))})
max(max.cue.count)
