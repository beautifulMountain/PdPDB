enrichment <- function(trimmed, numberOfPDB, reference, numberOfSamples, averageLength, verbose){
  
  numCol<-length(reference[,1])
  
  trimmedMat<-as.matrix(trimmed)
  
  noBrackets<-matrix(0, nrow = length(trimmed[,1]), ncol = length(trimmed[1,]))
  for(i in 1:length(trimmed[,1])){
    for(j in 1:length(trimmed[1,])){
      noBrackets[i,j] <- suppressBrackets(as.character(trimmedMat[i,j]))
    }
  }
    
  noBrackets.df<-as.data.frame(noBrackets)
  colnames(noBrackets.df)<-colnames(trimmed)
  
  totalPositionsSpecimen<- sum(lengths(noBrackets.df[!noBrackets.df=="*"]))
  
  enriched<-matrix(0, nrow=numCol, ncol= ncol(noBrackets.df))
  
  for (i in 1:ncol(noBrackets.df)){
    stats.old<-table(noBrackets.df[i])[rownames(table(noBrackets.df[i]))!="*"] # making frequencies stats and removing columns labeled with *
  
    stats<-stats.old
    if(length(stats.old)==1){
      stats<-as.data.frame(stats.old)[1, , drop=FALSE] # percent a dataframe with a single row to be converted to a vector and hence to loose its label
    }
    
    # just select aminos
    aminoStats<-stats[rownames(stats) %in% c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")]
    
    
    k<-1
    for(j in 1:numCol){
      
      if((identical(as.character(rownames(aminoStats)[k]), as.character(rownames(reference)[j])))&&(!is.na(identical(as.character(rownames(aminoStats)[k]), as.character(rownames(reference)[j]))))){
        enriched[j,i] <- as.numeric(aminoStats[k])
        k<-k+1
      }
      
      else{
        enriched[j,i]<-0
      }
        
    }
    
  }
  
  total<-NA
  percent<-NA
  zpstd<-NA
  z<-NA
  p<-NA
  stdErr<-NA
  proportions<-NA
  proportions.pvalues<-NA
  proportions.spec<-NA
  proportions.ref<-NA
  refProp<-NA
  
  
  
  positionsInReference<-numberOfSamples*averageLength # average number of positions in the reference specimen
  
  trials<-c(totalPositionsSpecimen, positionsInReference)
  
  print(paste("Specimen population (i.e. No. of sequences extracted from the PDBs)", numberOfPDB))
  print(paste("Reference population: ", numberOfSamples))
  print(paste("Number of positions in specimen", totalPositionsSpecimen))
  print(paste("Number of positions in reference: ", positionsInReference))
  
  
  # do statistical tests...
  for(i in 1:numCol){  
    
    total[i] <- sum(enriched[i,])
    
    percent[i] <- (total[i]/totalPositionsSpecimen)*100
    
    if(verbose==1)
      print(paste("test on",total[i], as.numeric(reference$referenceVec[i]/100*positionsInReference)))
    
    refProp[i]<-round(as.numeric(reference$referenceVec[i])/100*positionsInReference)
    tests<-c(total[i], refProp[i])
    
    proportions<-prop.test(tests, trials)
    proportions.pvalues[i]<-proportions$p.value
    proportions.spec[i]<-as.data.frame(proportions$estimate)[1,] # proportion on PDB specimen
    proportions.ref[i]<-as.data.frame(proportions$estimate)[2,] # proportion on reference
    
    zpstd<-zScore(percent[i], as.numeric(reference$referenceVec[i]), numberOfPDB, numberOfSamples)
    z[i]<- zpstd[[1]][1]
    # p[i]<- zpstd[[2]][1]
    stdErr[i]<- zpstd[[3]][1]
  }
  
  
  
  enriched.df<-as.data.frame(enriched)
  rownames(enriched.df)<-rownames(reference)
  
  names1<-colnames(trimmed)
  names2<-c("total", "perc", "reference", "reference_perc", "prop","prop.ref", "prop.test.p.value","z_score", "stdErr")
  
  enriched.df<-cbind(enriched.df, total, percent, refProp, reference, proportions.spec, proportions.ref, proportions.pvalues, z, stdErr)
  colnames(enriched.df)<-c(names1, names2)
  
    
  enriched.df
}