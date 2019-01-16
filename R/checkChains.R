checkChains <- function(aligned, centers, dictionary, verbose){
  
  # this function fixes errors due to ligand alignment
  
  print("Be patient, PdPDB is checking chains mismatches... this might take some time!")
  
  alignedMat<-as.matrix(aligned)
  
  rows<-length(alignedMat[,1])
  cols<-length(alignedMat[1,])
  
  for(i in 1:rows){
    for(j in 1:(cols-1)){
      
      # fix errors about peptide small ligands mixtures
      if(itsALigand(as.character(alignedMat[i,j]))){
        
        # check that next ligand it's not a small ligand
        if(!(as.character(alignedMat[i,(closestCenterPos(j+1,centers))]) %in% c("(O)", "(Z)", itIsDefined(as.character(alignedMat[i,(closestCenterPos(j+1,centers))]), dictionary, verbose)))){
          
          # if current ligand it's a small ligand slide *
          if(as.character(alignedMat[i,j]) %in% c("(O)", "(Z)", itIsDefined(as.character(alignedMat[i,j]), dictionary, verbose))){ # if the ligand is a small molecule
            print("PdPDB rearranges small ligands...")
            alignedMat[i,(j+1):(closestCenterPos(j+1,centers)-1)]<-slide(alignedMat[i,(j+1):(closestCenterPos(j+1,centers)-1)], verbose) # slide *
          }

          # if current ligand it's not a small ligand split residues
          if(!(as.character(alignedMat[i,j]) %in% c("(O)", "(Z)", itIsDefined(as.character(alignedMat[i,j]), dictionary, verbose)))){
            print("PdPDB rearranges residues...")
            alignedMat[i,(j+1):(closestCenterPos(j+1,centers)-1)]<-splitChains(alignedMat[i,(j+1):(closestCenterPos(j+1,centers)-1)], verbose)
          }
          
        }
      }
      
      # fix errors on the first L column
      if(alignedMat[i,(which(colnames(alignedMat)=="L")[1]-1)]=="*") # if position 1 - first ligand position (ecluded) vector ends with *
        alignedMat[i,1:(which(colnames(alignedMat)=="L")[1]-1)]<-slide(alignedMat[i,1:(which(colnames(alignedMat)=="L")[1]-1)], verbose) # this applies also to left hand side of the pattern table
        
    }
  }
  
  
  df.patterns<-as.data.frame(alignedMat, stringsAsFactors = FALSE)  # convert to a data.frame
  names(df.patterns)<-colnames(aligned)                 # update the header of the data.frame
  
  df.patterns
  
}