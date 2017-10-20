gg_labelledcormap<-function(d,title){
  print(title)
  require(ggplot2)
  require(reshape2)
  #get teh corrleation matrix
  cormat <- round(cor(d),2)

  #use hierarchical clustering to reorder
  # cormat2=tryCatch({
  #   return(reorder_cormat(cormat))
  # }, error = function(e) {
  #   print(e)
  #   print("couldn't reorder using hierarchical clustering")
  #   return(cormat)
  # })
  
  #and only extract the upper triangle
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  

  
  matrix.fontsize<-min(4,4*sqrt(6/(dim(d)[2])))
  
  #plot
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()+
    geom_text(aes(Var2, Var1, label = value), color = "black", size = matrix.fontsize)+#max(4,4*6/(dim(d)[2]))) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))+
    labs(title=title)
  return(ggheatmap)
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
