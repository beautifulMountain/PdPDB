splitChains <- function(vettore, verbose){

  nonGaps<-vettore[which(vettore!="*")]
  
  endsWithAsterisks<-FALSE
  if(identical(as.character(vettore[length(vettore)]), "*")) endsWithAsterisks<-TRUE
  
  startsWithAsterisks<-FALSE
  if(identical(as.character(vettore[1]), "*")) startsWithAsterisks<-TRUE
  
  j<-0
  
  if((length(nonGaps)<length(vettore))&&(length(vettore)>1)&&(endsWithAsterisks)&&(!startsWithAsterisks)){
    
    startPoint<-length(nonGaps)
    stopPoint<-trunc((length(nonGaps)/2)+1)
    
    for(i in startPoint:stopPoint){
      
      if(verbose==1) print(paste(vettore[i], "is moved..."))
      
      vettore[length(vettore)-j]<-nonGaps[i] # last non gap position is moved to the end of the vector, second last to the end-1 position and so forth
      vettore[i]<-"*" # replace with gap
      j<-j+1
    }
  }
  
  vettore
  
}