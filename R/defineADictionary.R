defineADictionary <- function(numberOfSymbols){
  
  print("Define your own dictionary (symbols have to be composed of 3, 2 or 1 letter(s)): ")
  
  if(numberOfSymbols > 9){
    print("9 is the highest number of sybols you can define")
    numberOfSymbols<-9
  } 
  
  yourDictionary<-vector("character", length = numberOfSymbols)
  
  for (i in 1:numberOfSymbols){
    yourDictionary[i]<-toupper(as.character(readline("Type in the [1-3] letter code: ")))
  }
  
  dictionary<-as.data.frame(yourDictionary)
  names(dictionary)<-c("PDB")
  dictionary
}