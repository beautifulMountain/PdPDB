toFasta <- function(patterns, dictionary, verbose){
  
  print("Converting to FASTA...")
  
  fasta_patterns <- rep(list(NA),length(patterns[,1]))
  
  k<-1 
  
  for(i in 1:length(patterns[,1])){
    
    translated <-""
    
    for(j in 1:ncol(patterns)){
      
      if(!is.na(patterns[i,j])){
        translated <- AAAtoA(patterns[i,j], dictionary, verbose)
        if(!is.na(translated))
          fasta_patterns[[k]][j] <- translated
      }
    }
    
    k<-k+1
    
  }
  
  df_fasta_patterns<-data.frame(ldply(fasta_patterns, rbind), stringsAsFactors=FALSE) # convert list to data frame
  df_fasta_patterns<-as.data.frame(naLas(df_fasta_patterns)) # move NAs to the right side
  df_fasta_patterns <- df_fasta_patterns[,colSums(is.na(df_fasta_patterns))<nrow(df_fasta_patterns)] # purge NA columns
  df_fasta_patterns
  
}