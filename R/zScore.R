zScore<-function(p1, p2, n1, n2){
  
  n1<-as.numeric(n1)
  n2<-as.numeric(n2)
  p1<-as.numeric(p1)/100
  p2<-as.numeric(p2)/100
  
  p1bar<-p1/n1
  p2bar<-p2/n2
  
  pbar<-(p1+p2)/(n1+n2)
  
  standardError<-sqrt(pbar*(1-pbar)*(1/n1+1/n2))
  
  z<-(p1bar-p2bar)/standardError
  
  pvalue<-pnorm(-abs(z))
  
  list(z, pvalue, standardError)
  
}