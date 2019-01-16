moveNALeft<-function(vettore){

  spia<-NA
  
  for(j in 1:(length(vettore)-1)){
    
    for(i in 1:(length(vettore)-1))
      if(is.na(vettore[i])){
        spia<-vettore[i+1]
        vettore[i+1]<-NA
        vettore[i]<-spia
      }
    
  }
  
  vettore
}