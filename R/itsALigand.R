itsALigand <- function(word){
  
  itsaligand <- FALSE
  
  if(identical(substr(word,1,1),"("))
    itsaligand<-TRUE
  
  itsaligand
  
}