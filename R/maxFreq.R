maxFreq <- function(trimmed, header, verbose){
  
  if(verbose==1) print("Building consensus sequence...")  
  
  maxFreqSequence<- vector("list", length(trimmed[1,])) 
  
  for(i in 1:length(trimmed[1,])) {
    maxInCol<-names(which(table(trimmed[,i])==max(table(trimmed[,i])))) # get the most frequent amino per each column
    
    if (length(maxInCol)==1){ # if only an amino has the highest freq
      maxFreqSequence[[i]]<-c(maxFreqSequence[[i]], names(which(table(trimmed[,i])==max(table(trimmed[,i])))))
    }
    else{ # else there are residues with the same frquency
      maxFreqSequence[[i]]<-c(maxFreqSequence[[i]], "X")
    } 
    
  }
  
  maxFreq<-as.data.frame(lapply(maxFreqSequence, suppressBrackets))
  names(maxFreq)<-header
  options(stringsAsFactors=FALSE)
  maxFreq
  
}