library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)
library(data.table)

load(file = paste0(localsettings$data.dir,"double_update_rev7"))
m1<-stan_model(paste0("nate_files/Final_Models/double_update_rev7.stan"))
bseed=135325234
fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1,iter=iterations,seed=bseed)
  