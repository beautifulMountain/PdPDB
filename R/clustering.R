clustering <- function(fastaPatterns, path, fileName, debug){
  
  fasta<-apply(fastaPatterns,1, paste, collapse="")
  dist<-adist(fasta) # Compute Levenshtein distance matrix
  rownames(dist) <- fasta # assign names from the fastaPatterns 
  clusters<-hclust(as.dist(dist), method = "ward.D2") # hierarchical clustering 
  
  dend<-as.dendrogram(clusters)
  
  plot(dend, xlab="", ylab="height", main="Cluster Dendrogram", axes=FALSE)
  axis(side = 2, at = seq(0, max(clusters$height), 1), col = "black", labels = FALSE, lwd = 2, )
  mtext(seq(0, max(clusters$height), 1), side = 2, at = seq(0, max(clusters$height), 1), line = 1, col = "black", las = 2)

  # at which eight the dendrogram has to be cut? This affect number of clusters...
  height<-1000 # calculate root sequence
  if((debug==0)||(debug==2))
    height<-as.integer(readline("Type in the height at which you want to cut the dendrogram: "))
  
  df.clusters <- data.frame(fasta,cutree(clusters,h=height))
  names(df.clusters)<-c("seq", "points")
  
  colors_to_use<-as.numeric(df.clusters$points)
  colors_to_use<-colors_to_use[order.dendrogram(dend)]
  labels_colors(dend)<-colors_to_use
  labels_cex(dend)<-0.5
  
  plot(dend, xlab="", ylab="height", main="Cluster Dendrogram", axes=FALSE)
  par(cex=1)
  axis(side = 2, at = seq(0, max(clusters$height), 1), col = "black", labels = FALSE, lwd = 2, )
  mtext(seq(0, max(clusters$height), 1), side = 2, at = seq(0, max(clusters$height), 1), line = 1, col = "black", las = 2)
  
  # draw rectangles per each cluster obtained and draw a line were user decided to cut
  abline(h = height, lty = 2)
  
  rectY<-0
  if(debug==0)
    rectY<-as.integer(readline("Do you want rectangles that highlight clusters on dendrogram? [Yes = 1] "))
  
  if(debug==2)
    rectY<-1
  
  if (rectY == 1) rect.hclust(clusters, h = height)
  
  # writing content to SVG file
  tmp <- file.path(path, fileName) # temp physical name of the file
  dev.copy(svg,tmp)
  dev.off()
  
  df.clusters
    
}