alignLigands<-function(patterns, n, verbose){

  print("Aligning Ligands...")
  
  blockLen <- (n*2)+2 # +1 for L +1 for dash or plus sign
  rows <- as.integer(length(patterns[,1]))  # max col length
  cols <- as.integer(length(patterns[1,]))  # max row length
  
  patternsMat<-as.matrix(patterns)          # make an input copy and turns it to a matrix
  
  # creating the matrix that will contain the aligned ligands
  new.cols= length(patterns[1,])*blockLen # worst case scenario: all the residues are ligands
  new.patterns <- matrix(NA,nrow=rows, ncol=new.cols) 
  
  # creating the header for new.patterns
  header<-rep("x", new.cols) # replicate X
  ligandPos<-seq(blockLen-n-1, new.cols-n, by = blockLen) # generate positions of ligands
  header[ligandPos] <- "L" # substitute L in the x replication
  
  k<-0
  
  print("Calculating new positions...")
  
  for (i in 1:rows){
    k<-1
    
    for (j in 1:cols){
      # if it's a ligand and header[j]!="L"
      if(((identical(substr(as.character(patternsMat[i,j]), 1, 1), "("))&&((!(header[k]=="L"))&&(!(is.na(header[k]=="L")))))){
        # insert a shift
        shiftPos<- shift(k, closestCenterPos(k, ligandPos))
        k<-k+shiftPos
        
        if(verbose == 1) print(paste("shift",patternsMat[i,j],"to position",k))
        
      }

      new.patterns[i, k] <- patternsMat[i, j]
      k<-k+1
    }
    
    
  }

  
  df.patterns<-as.data.frame(new.patterns, stringsAsFactors = FALSE)  # convert to a data.frame
  names(df.patterns)<-header                 # update the header of the data.frame
  # df.patterns <- df.patterns[,colSums(is.na(df.patterns))<nrow(df.patterns)]
  df.patterns[is.na(df.patterns)] <- "*"
  
  df.patterns
  
}