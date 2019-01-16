makeTheLogo <- function (trimmed, header, consensusSequence, dictionary, path, fileName) {
  
  logo<-lapply(trimmed, count) # calculate frequency of appearence per each position
  
  # set up plot params
  logoWidth=17 # width in inches
  logoHeight=13
  borderWidth=5 # width in inches
  
  end<-length(colnames(trimmed))
  
  # for very big sequences...
  if(end>16){
    logoWidth=34 # width in inches
    logoHeight=26
  }
  
  # axesLength<-c(1:length(logo))
  axesLength<-c(1:end)
  # axesLimit<-c(1,length(logo))
  axesLimit<-c(1,end)
  numberOfSymbols<-c(1:37) # 20AA, 9 dictionary, 1 gaps, 1 water, 1 Unknown, 5
  par(mar=c(borderWidth,borderWidth,borderWidth,borderWidth)) # set up margins
  
  # writing content to SVG file
  tmp <- file.path(path, fileName) # temp logical name of the file
  svg(tmp, width = logoWidth, height = logoHeight, bg = "white") # about 1024x768
  
  
  plot(numberOfSymbols, numberOfSymbols, xlim = axesLimit, type="n", axes=FALSE, xlab="Consensus Sequence", ylab=paste(length(trimmed[,1]), " sequences have been used to generate this motif"))
  axis(side = 1, at = axesLength, labels = consensusSequence, col = "grey", cex.axis=2) 
  axis(side = 3, at = axesLength, labels = header, col = "grey", cex.axis=2) # add top axis
  abline(v=seq(1,length(axesLength),1), col='grey', lty='dotted') # add grid 
  
  # add text to the plot - normalize characters to the highest frequency and amplify to 1000% (cex=10)
  for(i in axesLength){
    for(j in 1:length(logo[[i]]$x))
      text(i,j, suppressBrackets(logo[[i]]$x [j]), cex = (normalizeInterval((logo[[i]]$freq [j]), 0, sum(logo[[i]]$freq))*25), col=getColor(logo[[i]]$x [j]))
  }

  # color legend
  legend("top", legend = c("non-polar", "polar-unch", "polar-acid", "polar-basic", "nucleotides", "water", "gaps","undefined"), col = c('orange', 'green3', 'red', 'blue', "pink", "cyan", "snow2","black"), lwd=15, cex = 1, horiz = TRUE, box.lwd = 0,box.col = "transparent",bg = "transparent")
  
  # symbol legend
  label<-c("* Gaps","O Water", "Z Undefined")
  if((length(dictionary)>=1)&&(!is.na(dictionary))){
    lab<-as.vector(dictionary$PDB)
    for(i in 1:length(lab)) label[i+3]<-(paste(i,lab[i]))
    legend("right", legend = label, box.lwd = 0,box.col = "white",bg = "transparent")
  }
  else{
    legend("right", legend = c("* Gaps","O Water", "Z Undefined"), box.lwd = 0,box.col = "white",bg = "transparent")
  }

  
  dev.off()
  
}