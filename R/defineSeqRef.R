defineSeqRef<- function(type, void){
  
  if (void!=1) print("Provide values (%) per each residue!")
  
  if(type == 1)
    referenceNames<-"ACDEFGHIKLMNPQRSTVWY"
  else if(type == 2)
    referenceNames<-"acgtu"
  else
    referenceNames<-"O"
  
  referenceVec<-vector(mode = "double")
  
  name<-strsplit(referenceNames, split = "")[[1]]
  
  for(i in name){
    if (void == 1)
      referenceVec[i]<-0
    else
      referenceVec[i]<-as.numeric(readline(paste(i, ": ")))
  }
    
  
  reference<-as.data.frame(referenceVec)
  
  reference
  
}