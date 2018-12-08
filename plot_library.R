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
  
  phi=array(data = as.numeric(NA),
            dim = c(n.chains,tnmc,length(param.names),length(group.names)),
            dimnames = list(NULL,
                         NULL,
                         param.names,
                         group.names))
  
  for(q in 1:n.chains){#q<-1
    mymat<-read.table(paste(file.location,"/chain",q,"_hyper.txt",sep=""),header=F) %>% as.matrix %>%
      array(dim = c(dim(.)[1],length(param.names),length(group.names),1)) %>% aperm(c(4,1,2,3)) %>% .[,-1,,]
    
    if(dim(phi[q,,,])[1]!=dim(mymat)[1]){
      print(paste(dim(mymat),dim(phi[q,,,])))
      warning("Different number of lines in input data than expected.")
    }
      
    maxval<-min(dim(phi[q,,,])[1],dim(mymat)[1])
    phi[q,1:maxval,,] = as.numeric(mymat[1:maxval,,])
    

    #we gotta stretch out temp. By convention, it will be lined up with each group together, cycling through params.
    #temp_rs<-array(temp,

  }
  dimnames(phi)<-list(NULL,NULL,param.names,group.names)
  #phi<-as.numeric(phi)
  #now, how do we want this?
  #should be:
  #intuitive
  #reasonably size-efficient
  #fast
  #I think we oughtta have a list of items; each item in the list contains properties and a set of chains associated with that particular item.
  
  return(phi)
}

param_l1 <- function(param.names,tnmc,n.chains,subs){
  file.location<-paste0(mainDataDir, subDir)
  #make it array of dim
  if(length(subs)==1){
    n.subs<-subs
    sub.names<-paste0("SUB",1:n.subs)
  }else{
    n.subs<-length(subs)
    sub.names<-as.character(subs)
  }
  
  param=array(data = as.numeric(NA),
            dim = c(n.chains,tnmc,length(param.names),n.subs),
            dimnames = list(NULL,
                            NULL,
                            param.names,
                            sub.names))
  
  for(q in 1:n.chains){#q<-1
    for (s in 1:n.subs){#s<-1
      mymat<-read.table(paste(file.location,"/chain",q,"_sub",s,"_lower.txt",sep=""),header=F) %>% as.matrix #%>%
        #array(dim = c(dim(.)[1],length(param.names),1)) %>% aperm(c(4,1,2)) %>% .[,-1,,]
      
      if(dim(param[q,,,s])[1]!=dim(mymat)[1]){
        print(paste(dim(mymat),dim(param[q,,,s])))
        warning("Different number of lines in input data than expected.")
      }
      
      maxval<-min(dim(param[q,,,s])[1],dim(mymat)[1])
      param[q,1:maxval,,s] = as.numeric(mymat[1:maxval,])
      
      #we gotta stretch out temp. By convention, it will be lined up with each group together, cycling through params.
      #temp_rs<-array(temp,
    }
  }
  dimnames(param)<-list(NULL,NULL,param.names,sub.names)
  #phi<-as.numeric(phi)
  #now, how do we want this?
  #should be:
  #intuitive
  #reasonably size-efficient
  #fast
  #I think we oughtta have a list of items; each item in the list contains properties and a set of chains associated with that particular item.
  
  return(param)
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

