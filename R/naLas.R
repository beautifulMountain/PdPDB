naLas<-function(matrice){
  
  spia<-NA
  
  for(k in 1:length(matrice[,1]))
  {
    matrice[k,]<-moveNALeft(matrice[k,])
  }
  
  matrice
  
}