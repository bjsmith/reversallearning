library(dplyr)
library(hBayesDM)
library(tidyr)
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


hyper_param_3l <- function(param.l2.names,groups.l2.list,param.l3.names,groups.l3.list,tnmc,n.chains){
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
  
  param.l2.N<-length(param.l2.names)
  param.l3.N<-length(param.l3.names)
  groups.l2.N<-length(groups.l2.list)
  groups.l3.N<-length(groups.l3.list)
  phi.s=array(NA,dim=c(n.chains,tnmc,param.l2.N,groups.l2.N),
                                  dimnames=list(NULL,
                                                NULL,
                                                param.l2.names,
                                                groups.l2.list))
  
  phi.g=array(NA,dim=c(n.chains,tnmc,param.l3.N,groups.l3.N),
              dimnames=list(NULL,
                            NULL,
                            param.l3.names,
                            groups.l3.list))
  
  for(q in 1:n.chains){#q<-1
    phi.s[q,,,] = as.matrix(read.table(paste(file.location,"/chain",q,"_hyper_phi_s.txt",sep=""),header=F)) %>%
      array(dim = c(dim(.)[1],param.l2.N,groups.l2.N,1)) %>% aperm(c(4,1,2,3)) %>% .[,-1,,]
    
    phi.g[q,,,] = as.matrix(read.table(paste(file.location,"/chain",q,"_hyper_phi_g.txt",sep=""),header=F)) %>%
      array(dim = c(dim(.)[1],param.l3.N,groups.l3.N,1)) %>% aperm(c(4,1,2,3)) %>% .[,-1,,]
    
    #we gotta stretch out temp. By convention, it will be lined up with each group together, cycling through params.
    #temp_rs<-array(temp,
    
  }
  #dimnames(phi)<-list(NULL,NULL,param.names,group.names)
  #now, how do we want this?
  #should be:
  #intuitive
  #reasonably size-efficient
  #fast
  #I think we oughtta have a list of items; each item in the list contains properties and a set of chains associated with that particular item.
  
  return(list("phi.s"=phi.s,"phi.g"=phi.g))
}

