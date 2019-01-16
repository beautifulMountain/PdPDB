slide <- function(vettore, verbose){
  
  right <- FALSE
  last <- FALSE
  if(identical(as.character(vettore[1]), "*")) { right <-TRUE }
  if(identical(as.character(vettore[length(vettore)]), "*")) { last<-TRUE}
  
  if(right&&last){
    # skip!
    if(verbose == 1) print("skipping * sliding...")
  }
  else{
    
    if(verbose == 1){
      
      print(vettore)
      
      if(right){
        print("Sliding * to right")
      }
      else{
        print("Sliding * to left")
      }
    }
    
    if(right){ # first position is an *
      j<-1
      for(i in 1:length(vettore)){
        if(!(identical(as.character(vettore[i]),"*"))){
          vettore[j]<-vettore[i]
          vettore[i]<-"*"
          j<-j+1
        }
      }
      
    }
    
    else if((last)&&!(right)) { # last is an *
      j<-length(vettore)
      for(i in length(vettore):1){
        if(!(identical(as.character(vettore[i]),"*"))){
          vettore[j]<-vettore[i]
          vettore[i]<-"*"
          j<-j-1
        }
      }
      
    }
    
  }
  
 vettore
  
}