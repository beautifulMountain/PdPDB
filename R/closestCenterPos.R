closestCenterPos <- function (pos, centers){
  
  nextCenter <- 0
  i<-1
  
  repeat{
    
    nextCenter <- centers[i]
    
    if((centers[i]>=pos)&&(!is.na(centers[i]>=pos))){
      break
    }
    
    i<-i+1
  }
  
  nextCenter
  
}