enrichmentAnalysis <- function(trimmed, numberOfPDB, reference, numberOfSamples, averageLength, path, fileName, verbose){
  
  enriched<-enrichment(trimmed, numberOfPDB, reference, numberOfSamples, averageLength, verbose)
  
  if(verbose==1)
    print(paste("##### Bonferroni correction on... ", enriched$propTest.p.value))
  
  bonferroniCorrection<-as.data.frame(p.adjust(enriched$`prop.test.p.value`, method="bonferroni")) # default n = length(p)
  names(bonferroniCorrection)<-"bonferroni.p.value"
  
  wtVec<-rep(" ", 20)
  wt<-wilcox.test(enriched$total, enriched$reference, exact = TRUE) # Mann-Whitney-Wilcoxon Test
  wtVec[1]<-wt$p.value
  
  jbsVec<-rep(" ", 20)
  jbs<-jarque.bera.test(enriched$total) # jarque-bera test for specimen
  jbsVec[1]<-jbs$p.value
  
  jbrVec<-rep(" ", 20)
  jbr<-jarque.bera.test(enriched$reference) # jarque-bera test for reference
  jbrVec[1]<-jbr$p.value
  
  jbsVec.df<-as.data.frame(jbsVec)
  names(jbsVec.df)<-"jarque.bera.p.value"
  
  jbrVec.df<-as.data.frame(jbrVec)
  names(jbrVec.df)<-"jarque.bera.reference.p.value"
  
  wtVec.df<-as.data.frame(wtVec)
  names(wtVec.df)<-"wilcoxon.p.value"
  
  # output section
  write.table(cbind(enriched, bonferroniCorrection, wtVec.df, jbsVec.df, jbrVec.df), file = file.path(path,paste(fileName, "enrichment.csv", sep="_")),row.names=TRUE, na="", sep=" ", quote = FALSE, col.names = NA)
  
  plotEnrichment(enriched$prop, 0,rownames(enriched), path,paste(fileName, "proportions.svg", sep="_"))
  plotEnrichment(enriched$`z_score`, enriched$stdErr,rownames(enriched), path,paste(fileName, "standardized.svg", sep="_"))
  
}