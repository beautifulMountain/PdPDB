generateReport <- function(path, verbose){
  
  currentWD<-getwd()
  setwd(path)
  on.exit(setwd(currentWD))

  write("\\documentclass{article}
  \\usepackage{graphicx}
  \\usepackage[document]{ragged2e}
  \\title{PdPDB Report}
  \\begin{document}
  \\maketitle
  \\centering
  \\newpage", file = file.path(path, "report"))
  
  # Generate tables
  tableList<-NULL
  figureList<-NULL
  captioN<-NA

  tableList<-system(paste("ls -1 ", file.path(path,"*.csv"), "| sort -g | grep -vi patterns.csv"), intern = TRUE, input = tableList)

  for (i in 1:length(tableList)){

    captioN<-basename(file_path_sans_ext(tableList[i])) # get just the file name with no ext and path

    if (verbose == 1)
      print(paste("######## TAB", captioN))

    toTexFile<-xtable(as.data.frame(read.csv(tableList[i], header = TRUE, stringsAsFactors = FALSE, sep = "", as.is = TRUE, dec=".", check.names = FALSE, colClasses = c("character"))), caption = "captioN", label = i) # make latex code for the table
    write(paste("\\begin{flushleft} \\textbf{Fig.", i, "}", gsub("[[:punct:]]", " ", captioN),"\\end{flushleft}"), file = file.path(path, "report"), append=TRUE)
    print(toTexFile, file.path(path, "report"), type = "latex", file = file.path(path, "report"), append=TRUE, caption.placement = "top", include.rownames = FALSE, quote = FALSE)
    write("\\newpage", file = file.path(path, "report"), append=TRUE)
  }



  # include pictures
  figureList<-system(paste("ls -1 ", file.path(path,"*.svg"), "| sort -g | grep -iv dendrogram"), intern = TRUE, input = tableList)
  system("for i in *.svg; do convert -size 800x600 $i ${i%.*}.pdf; done")
  
  write("\\newpage
      \\begin{figure}[ht!]
          \\includegraphics[scale= 0.5]{dendrogram.pdf}
          \\caption{Dendrogram}
    \\end{figure}", file = file.path(path, "report"), append=TRUE)

  for (i in 1:length(figureList)){

    captioN<-basename(file_path_sans_ext(figureList[i]))

    if (verbose == 1)
      print(paste("########", captioN))

    write(paste("
      \\newpage
      \\begin{figure}[ht!]",
         paste("\\includegraphics[scale= 0.1]{",paste(captioN,".pdf", sep=""),"}", sep=""),
         paste("\\caption{",gsub("[[:punct:]]", " ", captioN),"}", sep=""), 
      "\\end{figure}"), file = file.path(path, "report"), append=TRUE)
  }


  #close the report file
  write("\\end{document}", file = file.path(path, "report"), append = TRUE)
  
  
  # compile the file with latex
  tex_file<-paste(file.path(path, "report"), ".tex", sep="") # creates the logical file tex file
  file.copy(file.path(path, "report"), tex_file) # copy the template to create a physical file
  
  system(paste("pdflatex ", tex_file)) # make a pdf
  system(paste("rm ", file = file.path(path, "report"))) # remove the template
  system("ls -1 *.pdf | grep -v report.pdf | xargs rm") # remove temporary files
  system("ls -1 report.* | grep -iv 'report.pdf' | xargs rm") 
  system("ls -1 *.svg | xargs rm") 
  system("ls -1 *.csv | grep -Eiv 'patterns' | xargs rm") 
  
}