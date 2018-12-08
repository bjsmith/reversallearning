#create a fixed dataset
N=200
v1<-rnorm(N,0,1)
v2<-rnorm(N,0,1)
v3<-v1+v2+rnorm(N,0,1)
v4<-v1+rnorm(N,0,1)
v5<-v2+rnorm(N,0,1)
Deltas<-data.frame(v1,v2,v3,v4,v5)
cov(Deltas)
cor(Deltas)
#create correlated variables. 
v6<-rnorm(N,0,1)
v7<-rnorm(N,0,1)
UnconnectedData<-data.frame(v6,v7)
#Pass both in.
#Estimate the correlation between them. This is like estimating two deltas.
#now create a variable that's sampled from one of those; measure its correlation, with and without the Cholesky matrix.

options(mc.cores = 3)
sim_data <- list(Deltas = Deltas, 
                 N = nrow(Deltas), 
                 D_N=ncol(Deltas),UD_N=ncol(UnconnectedData),UnconnectedData= UnconnectedData)

fit_SigmaOmega <- stan(file='lkj6.stan', 
                       data = sim_data,
                       warmup = 600, 
                       iter = 800,
                       chains = 6)
