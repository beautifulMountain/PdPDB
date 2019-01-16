trimAlignment <- function(alignment, perc, header, verbose){
  
  print(paste("Trimming the alignment to", perc, "% frequency across the sequences"))
  
  missingRes<-0
  
  rows<- length(alignment[,1])
  cols<- length(alignment[1,])
  
  hitNum = rows-((rows*perc)/100) # number of missing elements in a column
  
  columnsToKeep<-rep(NA, cols)
  
  for(j in 1:cols){
    for(i in 1:rows){
      if ((alignment[i,j])=="*") missingRes<-missingRes+1
    }
    
    if(verbose==1) print(paste("Column", j, "of the alignements misses", missingRes))
    
    if (missingRes >= hitNum) {if(verbose==1) print(paste("column", j, "will be discarded"))}
    else{ columnsToKeep[j] <- j}
    
    missingRes<-0
  }
  
  trimmed<-alignment[ ,columnsToKeep[!is.na(columnsToKeep)]]
  names(trimmed)<-header[columnsToKeep[!is.na(columnsToKeep)]]
  trimmed

}