select_rawdata_cols_for_run<-function(rawdata,sid,r,m){
  stop("this needs to order the data, but it doesn't.")
  return(rawdata[subid==sid & Motivation==m & runid==r,])
}
