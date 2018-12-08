myvar1<-10
myvar2<-10
myvar3<-10
myfunc<-function(x){
  myvar1<-myvar1+1
  myvar2=myvar2+1
  myvar3<<-myvar3+1
}

lapply(1:10,myfunc)