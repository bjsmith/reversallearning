# Function to replace cue index with it's cumulative frequency
get_cue_index_from_cum_freq <- function(x,by_var=NULL) {
  if (is.null(by_var)) {
    new_x <- vector(length=length(x))
    un_x <- unique(x)
    for (i in un_x) {
      idx <- which(x %in% un_x[i])
      new_x[idx] <- seq_along(idx)
    }
  } else {
    new_x <- NULL
    for (n in unique(by_var)) {
      tmp_x     <- x[by_var==n]
      tmp_new_x <- vector(length=length(tmp_x))
      tmp_un_x  <- unique(tmp_x)
      for (i in tmp_un_x) {
        idx <- which(tmp_x %in% i)
        tmp_new_x[idx] <- seq_along(idx)
      }
      new_x <- c(new_x, tmp_new_x) 
    }
  }
  return(new_x)
}
