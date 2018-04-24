library(dplyr)
hyper_param <- function(param.names,group.names,tnmc,n.chains){
  file.location<-paste0(mainDataDir, subDir)
  #phi<-list()
  # for(g.name in l2.groups.list){
  #   for (p in level2.par.names){
  #     item<-list()
  #     item$group<-g
  #     item$param<-p
  #     item$data<-array(NA,c(n.chains,))
  #     phi_index<-length(phi)+1
  #     phi[[phi_index]]<-item
  #   }
  # }
  #make it array of dim
  
  phi=array(data = NA,
            dim = c(n.chains,tnmc,length(param.names),length(group.names)),
            dimnames = list(NULL,
                         NULL,
                         param.names,
                         group.names))
  
  for(q in 1:n.chains){#q<-1
    phi[q,,,] = as.matrix(read.table(paste(file.location,"/chain",q,"_hyper.txt",sep=""),header=F)) %>%
      array(dim = c(dim(.)[1],length(param.names),length(group.names),1)) %>% aperm(c(4,1,2,3)) %>% .[,-1,,]
    

    #we gotta stretch out temp. By convention, it will be lined up with each group together, cycling through params.
    #temp_rs<-array(temp,

  }
  dimnames(phi)<-list(NULL,NULL,param.names,group.names)
  #now, how do we want this?
  #should be:
  #intuitive
  #reasonably size-efficient
  #fast
  #I think we oughtta have a list of items; each item in the list contains properties and a set of chains associated with that particular item.
  
  return(phi)
}

