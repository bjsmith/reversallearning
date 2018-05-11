
setwd(file.path(mainDir, subDir))

tnmc=length(keep.samples)

if(plot.lower==TRUE)theta=array(NA,c(n.chains,n.pars,S,tnmc))
if(plot.weights==TRUE)weights=array(NA,c(n.chains,S,tnmc))

for(q in 1:n.chains){
if(plot.lower==TRUE){
for(j in 1:S){
temp=t(as.matrix(read.table(paste("chain",q,"_sub",j,"_lower.txt",sep=""),header=F)))
theta[q,,j,]=temp[,1:tnmc]
}}
if(plot.weights==TRUE){
temp=t(as.matrix(read.table(paste("chain",q,"_weights.txt",sep=""),header=F)))
weights[q,,]=temp[,1:tnmc]
}
print(round(q/n.chains*100))
}

setwd(file.path(mainDir))

#######################################################################################

breaks=50

if(plot.lower==TRUE){
#par(mfrow=c(2,2),ask=T)
for(j in 1:S){
for(k in 1:n.pars){
matplot(t(theta[,k,j,start:tnmc]),type="l",lty=1,main=paste("Subject ",j),ylab=par.names[k])
#abline(h=true$vec[k],lwd=3)
hist(theta[,k,j,start:tnmc],prob=T,breaks=breaks,main=paste("Subject ",j),xlab=par.names[k])
#abline(v=true$vec[k],lwd=3,col="red")
}}}


if(plot.weights==TRUE){
#par(mfrow=c(2,2),ask=T)
for(j in 1:S){
matplot(t(weights[,j,start.weights:tnmc]),type="l",lty=1,main=paste("Subject ",j),ylab="log likelihood")
}}



