fillNAsPIDlist<-function(pidlist){
  pdbs<-grep("pdb", pidlist, ignore.case = TRUE)
  nas<-grep("pdb", pidlist, ignore.case = TRUE, invert = TRUE)
  
  pidlist.tmp<-rep(NA, length(pidlist))
  
  for(i in 1:length(pidlist)){
    
   for(j in 1:length(pdbs)){
     
     if(((i>=as.numeric(pdbs[j]))&&(i<as.numeric(pdbs[j+1])))&&(!is.na((i>=as.numeric(pdbs[j]))&&(i<as.numeric(pdbs[j+1]))))) {
       pidlist.tmp[i]<-pidlist[pdbs[j]]
     }
     
   }
    
  }
  
  pidlist.tmp[which(is.na(pidlist.tmp))]<-pidlist[pdbs[length(pdbs)]]
  
  pidlist.tmp
}