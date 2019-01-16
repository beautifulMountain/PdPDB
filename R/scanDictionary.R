scanDictionary <- function(word, dictionary, verbose){
  
  found<-NA
  ligand<-FALSE
  
  if(!any(is.na(dictionary))){
    
    ligand<-itsALigand(word)
    
    # check if the word it's a ligand first
    if(ligand){
      if(nchar(word)==5){ core<-substr(word,2,4); }
      else if(nchar(word)==4){ core<-substr(word,2,3); }
      else if(nchar(word)==3){ core<-substr(word,2,2); }
    }
    else core<-word
    
    if (verbose==1) print(paste("Searching for word:", core))
    
    for(i in 1:length(dictionary[,1]))
      if(identical(toupper(core), toupper(dictionary[i,]))) # make case-insensitive and compare core with dictionary
        found<-as.character(i)
  }
  
  if(!is.na(found)&&(ligand)) found<-paste("(",found,")",sep="")
    
  found
  
}