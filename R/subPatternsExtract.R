subPatternsExtract<-function(block, sbsp, n, verbose){
  
  subBlock<-NA
  subPatterns<-NA
  patterns<-NA
  
  currentBlock<-NA
  nextBlock<-NA
  
  header<-NA
  
  for(i in 1:length(sbsp)){ # for all blocks
    
    currentBlock<-as.integer(sbsp[i]) # get starting point 
    
    if (i<length(sbsp)){
      if(verbose==1) print(paste("block", i))
      nextBlock<-as.integer(sbsp[i+1]-1) # get end point
    }
    else{
      if(verbose==1) print(paste("block", i))
      nextBlock<-length(block[,1]) # this works only for last block
    }
    
    
    subBlock<-block[currentBlock:nextBlock, ] # select subblocks
    
    subPatterns<- arrangePatterns(subBlock, n) # arrange patterns of a subblock
    
    subPatterns<-subPatterns[!duplicated(subPatterns),] # remove duplicates
    
    
    if (length(colnames(subPatterns)) > length(header)) { header<- colnames(subPatterns) }
    
    if (verbose==1) print(subPatterns)
    
    patterns<-rbind.fill(as.data.frame(patterns), as.data.frame(subPatterns)) # cumulate sub patterns
    
  }
  
  patterns
  
}