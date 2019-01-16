itIsDefined <- function(symbol, dictionary, verbose){
  
  oldw <- getOption("warn") # getting default warning level
  options(warn = -1) # silencing warnings
  
  defined<- "(Z)"
  numericCore<-0
  core<-NA
  
  if (verbose==1) print(symbol)
  
  if(!any(is.na(dictionary))){
    
    if(nchar(symbol)==5){ core<-substr(symbol,2,4); }
    else if(nchar(symbol)==4){ core<-substr(symbol,2,3); }
    else if(nchar(symbol)==3){ core<-substr(symbol,2,2); }
    
    if((!is.na(as.numeric(core)))&&(is.numeric(as.numeric(core)))){
        
        numericCore<-as.numeric(core)
        if(((numericCore>=1)&&((numericCore<=length(dictionary[,1]))))&&(!is.na(numericCore>=1)&&((numericCore<=length(dictionary[,1])))))
          defined<-(paste("(",as.character(core),")", sep = ""))

      
    }
  }
  
  options(warn = oldw) # recovery default warning level
  
  defined
  
}