library(ggplot2)
library(tidyr)
library(gridExtra)
#groups

dim(phi_g)

phi_g_dt<-data.table("Value"=rep(as.double(NA),length(phi_g)),
                     "Group"=rep(as.integer(NA),length(phi_g)),
                     "Param"=rep(as.character(NA),length(phi_g)),
                     "Chain"=rep(as.integer(NA),length(phi_g))
                     )

n=1
for (i in 1:dim(phi_g)[1]){
  for (j in 1:dim(phi_g)[2]){
    for (k in 1:dim(phi_g)[3]){
      #i=j=k=n=1
      phi_g_dt[n,`:=`(Value=phi_g[i,j,k],Group=k,Param=param.l3.names[j],Chain=i)]
      n=n+1
    }
  }
}

group.level.plot<-ggplot(phi_g_dt,aes(x=Value,color=as.factor(Group)))+geom_density()+facet_grid(~Param,scales = "free_x")

#and phi_s
dim(phi_s)
phi_s_primes<-sum(sapply(1:dim(phi_s)[3],is.prime))
phi_s_tablesize<-prod(dim(phi_s)[1:2])*phi_s_primes
phi_s_dt<-data.table("Value"=rep(as.double(NA),phi_s_tablesize),
                     "Subject"=rep(as.integer(NA),phi_s_tablesize),
                     "Param"=factor(rep(as.character(NA),phi_s_tablesize),levels=unique(group_by_subject)),
                     "Chain"=rep(as.integer(NA),phi_s_tablesize),
                     "Group"=factor(rep(as.character(NA),phi_s_tablesize),levels=unique(group_by_subject))
)
n=1

for (i in 1:dim(phi_s)[1]){
  for (j in 1:dim(phi_s)[2]){
    for (k in 1:dim(phi_s)[3]){
      #i=j=k=n=1
      if (is.prime(k)){
        phi_s_dt[n,`:=`(Value=phi_s[i,j,k],Subject=k,Param=param.l2.names[j],Chain=i,Group=group_by_subject[k])]
        n=n+1
      }
    }
  }
}

if(length(unique(phi_s_dt$Subject))<20){
  subject.level.plot<-ggplot(phi_s_dt,aes(x=Value,color=as.factor(Subject),group=as.factor(Subject)))+geom_density()+facet_grid(~Param,scales = "free")
}else{
  subject.level.plot<-ggplot(phi_s_dt,aes(x=Value,color=as.factor(Group),group=as.factor(Subject)))+geom_density()+facet_grid(~Param,scales = "free")
}


#Nothing *obviously* off with these. 
#what about the run-level distributions?


dim(theta)
theta_primes<-sum(sapply(1:dim(phi_s)[3],is.prime))
theta_tablesize<-prod(dim(theta)[c(1:2,4)])*phi_s_primes

theta_dt<-data.table("Value"=rep(as.double(NA),theta_tablesize),
                     "Subject"=rep(as.integer(NA),theta_tablesize),
                     "Param"=factor(rep(as.character(NA),theta_tablesize),levels=unique(group_by_subject)),
                     "Chain"=rep(as.integer(NA),theta_tablesize),
                     "Run"=rep(as.integer(NA),theta_tablesize),
                     "Group"=factor(rep(as.character(NA),theta_tablesize),levels=unique(group_by_subject)))

n=1
for (i in 1:dim(theta)[1]){
  for (j in 1:dim(theta)[2]){
    for (k in 1:dim(theta)[3]){
      for (l in 1:dim(theta)[4]){
        #i=j=k=n=1
        if (is.prime(k)){
          theta_dt[n,`:=`(Value=theta[i,j,k,l],Subject=k,Param=param.l2.names[j],Run=l,Chain=i,Group=data[[k]]$group)]
          n=n+1
        }
      }
    }
  }
}

if(length(unique(theta_dt$Subject))<20){
  run.level.plot<-ggplot(theta_dt,aes(x=Value,color=as.factor(Subject),group=as.factor(Subject)))+geom_density()
}else{
  run.level.plot<-ggplot(theta_dt,aes(x=Value,color=as.factor(Group),group=as.factor(Subject)))+geom_density(alpha=0.3)
}

run.level.plot<-run.level.plot+
  coord_cartesian(xlim=c(-5,5),ylim=c(0,2))+
  facet_grid(Run~Param,scales = "free")


print(grid.arrange(grobs=list(group.level.plot, subject.level.plot,run.level.plot), layout_matrix=
                          rbind(c(1,3),
                                c(2,3))))
