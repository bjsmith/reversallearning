want = c("parallel","compute.es","rstan","loo","dplyr","pROC","ggplot2","tidyr","data.table")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
