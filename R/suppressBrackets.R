suppressBrackets <- function(carattere){
  
  withNoBrackets<-carattere
  
  if (identical(substr(carattere, 1, 1), "("))
    withNoBrackets<-substr(carattere, 2, 2)
  else if (identical(substr(carattere, 1, 1), "*"))
    withNoBrackets<-substr(carattere, 1, 1)
  
  withNoBrackets
  
}