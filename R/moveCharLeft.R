moveCharLeft<-function(vettore, verbose){
  
  if(verbose==1){
    print(paste(vettore, "################"))
  }
  
  spia<-NA
  
  for(j in 1:(length(vettore)-1)){
    
    for(i in 1:(length(vettore)-1))
      if(vettore[i]=="*"){
        spia<-vettore[i+1]
        vettore[i+1]<-"*"
        vettore[i]<-spia
      }
    
  }
  
  vettore
}