plotEnrichment <- function (enrichedVec, enrichedErr, labelVec, path, fileName){
  
  # writing content to SVG file
  tmp <- file.path(path, fileName) # temp logical name of the file
  svg(tmp, width = 17, height = 13, bg = "white") # about 1024x768

  enrichedBarPlot<-barplot(enrichedVec, axes = FALSE, ylab = "Z-score", names.arg = labelVec, width = 1, space = NULL, legend.text = FALSE)
  axis(2)
  
  arrows(enrichedBarPlot, enrichedVec+enrichedErr, enrichedBarPlot, enrichedVec-enrichedErr, angle = 90, code = 3)
  
  dev.off()
}
