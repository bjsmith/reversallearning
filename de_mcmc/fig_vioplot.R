

plot.lower=TRUE
plot.weights=TRUE
start=2
start.weights=2

#ps1,    2,    3,    4,   5,   6,    7,    8
# tt, n1n1, n2n2, n3n3, n1t, tn1, n1n2, n2n1

version="m5"

conds=c("hold","shift","split")

samples=list()
for(zz in 1:length(conds)){
save.name <- paste("output_",version,"_",conds[zz],sep="")
load(paste(save.name,".RData",sep=""))

par(mfrow=c(1,2),ask=FALSE)
source("fig_base.r")
samples[[zz]]=list("theta"=theta)
}

######################################################

require(vioplot)

par(mfrow=c(2,2))
for(j in 1:3){
  plot(NA,xlim=c(1,9),ylim=c(0,1),main=conds[j],xlab="pterm",ylab="weight",xaxt="n")
for(i in 1:n.pterms){
vioplot(as.numeric(samples[[j]]$theta[,i+2,,]),at=i,col="light blue",add=TRUE)
}
  temp=theta[,3:10,1,]
  cont=1-apply(temp,c(1,3),sum)
  vioplot(as.numeric(cont),at=9,col="light blue",add=TRUE)
  axis(1,at=1:9,labels=c("tt", "n1n1", "n2n2", "n3n3", "n1t", "tn1", "n1n2", "n2n1","cont"))
  }

######################################################

par(mfrow=c(1,1))
plot(NA,xlim=c(0,15),ylim=c(0,15),xlab=c("color concentration"),ylab=c("angle concentration"))
cols=c("black","red","blue")
for(j in 1:3){
points(as.numeric(samples[[j]]$theta[,1,,]),as.numeric(samples[[j]]$theta[,2,,]),col=cols[j])
}
abline(0,1)
