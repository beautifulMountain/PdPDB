#' Pattern Discovery in PDB Structures of Metalloproteins
#'
#'
#' @author Luca Belmonte, Sheref S. Mansy
#' @usage PdPDB(path, metal, n, perc, interactive, dropsReplicate)
#' @keywords metalloproteins, PDB, coordinating patterns, metal, alignment, ligand alignment, motifs
#' @description Looks for amino acid and/or nucleotide patterns coordinated to a given prosthetic centre. It also accounts for small molecule ligands. Patterns are aligned, clustered and translated to logo-like sequences to infer coordination motifs.
#' @references Belmonte L, Mansy SS Patterns of Ligands Coordinated to Metallocofactors Extracted from the Protein Data Bank, Journal of Chemical Information and Modeling (accepted)
#' @param path A string containing the path to the PDB directory.
#' @param metal A string containing the PDB chemical symbol of the target prosthetic centre; e.g. SF4 for [4Fe-4S] cluster, ZN for zinc. The PDB chemical symbol is case sensitive for macOS.
#' @param n A numerical value that contains the number or residue in following/preceding n positions from the ligated amino acid or nucleotide; if n=1 PdPDB searches for x(L)x motif-like chains, if n=2 for xx(L)xx. (L)igand.
#' @param perc A numerical value about the minimum percent of letters in a column otherwise residues are dropped.
#' @param interactive A numerical value. 0 interactive, 1 automated (will not cut dendrogram), 2 user decided cut. In mode 1 and 2 ExPASy amino acid frequencies are used as reference.
#' @param dropsReplicate A numerical value. 0 keeps replicated patterns, 1 drops replicated patterns entry by entry, 2 keeps only unique patterns. WARNING: If you are going to use PdPDB-3D, the use of option 0 is mandatory!
#' @return PdPDB generates a list of ".csv" and ".svg" files that will be stored in the same folder of the analyzed pdb/cif files (see "path"), its output is as follows:
#' \item{frequency.csv}{PDB-like patterns (i.e. with PDB chem Ids). "-" and "+" are used for residues out of the n inspecting window or from different monomers, respectively. Patterns come along with their frequency.}
#' \item{alignment.csv}{Ligand-aligned patterns with dashes, plus signs and gaps ("*"). See 'frequency.csv'.}
#' \item{following_X_enrichment.csv}{n files. Each file contains enrichment score, z-score and statistics at up to n following positions. X is the +position from ligated residue.}
#' \item{ligands_enrichment.csv}{Enrichment scores and statistics for ligands.}
#' \item{notLigands_enrichment.csv}{Enrichment statistics for the whole specimen but ligands.}
#' \item{preceeding_X_enrichment.csv}{As for "following" but this is meant for residues preceeding ligands. See "following_X_enrichment.csv."}
#' \item{root_enrichment.csv}{Overall enrichment score.}
#' \item{logo_Y.csv}{Y files. Each file contains the logo and consensus sequence for a cluster. Y is the cluster number.}
#' \item{dendrogram.svg}{The dendrogram along with the user deciced cutoff and clusters.}
#' \item{following_X_proportions.svg}{Plot of the enrichment score per each amino acid in following positions.}
#' \item{ligands_proportions.svg}{Plot of the enrichment score per each amino acid in ligated position.}
#' \item{notLigands_proportions.svg}{Plot of the enrichment score per each amino acid in non ligated position.}
#' \item{preceeding_X_proportions.svg}{Plot of the enrichment score per each amino acid in preceeding positions.}
#' \item{root_proportions.svg}{Plot of the root enrichment score.}
#' \item{logo_Y.svg}{Plot of the logo and consensus sequence of the Yth cluster. The complete aligned cluster is given as homonym '.csv' file. Sequences come along with percentages. If the dendrogram is not cut the root logo is given.}
#' \item{following_X_standardized.svg}{Plot of the z-score per each amino acid in following positions.}
#' \item{ligands_standardized.svg}{Plot of the z-score per each amino acid in ligated position.}
#' \item{notLigands_standardized.svg}{Plot of the z-score per each amino acid in non ligated position.}
#' \item{preceeding_X_standardized.svg}{Plot of the z-score per each amino acid in preceeding positions.}
#' \item{root_standardized.svg}{Plot of the root z-score.}
#' \item{patterns.csv}{PDB like extracted patterns along with the PDB ID and metal IDs. Useful for debbugging. Needed for restore.}
#' \item{report.pdf}{A report file consisting of all the files but patterns.csv.}
#' \item{PdPDB.log}{PdPDB log file. Useful for debbugging. Needed for restore.}
#' @note Files have to be in the local file system and contain the ".pdb" or ".cif" extension. Output files use brackets to highlight ligands and/or 'L' in heading line.
#' @importFrom utils count.fields head read.csv read.table write.table
#' @importFrom plyr count ldply rbind.fill
#' @importFrom graphics axis legend plot points text mtext par abline arrows barplot
#' @importFrom stats as.dist cutree hclust rect.hclust as.dendrogram order.dendrogram pnorm p.adjust prop.test wilcox.test
#' @importFrom utils adist Sweave write.csv 
#' @importFrom grDevices dev.copy dev.off svg
#' @importFrom dendextend labels_colors<- labels_cex<-
#' @importFrom tseries jarque.bera.test
#' @importFrom xtable xtable print.xtable
#' @importFrom tools file_path_sans_ext
#' @examples
#' ################ Defining path to PDBs
#' path_to_PDB="inst/extdata/PDB" # this is where pdb/cif files are stored
#'
#' ################ Research Parameters
#' metal="SF4"  # searches for [4fe-4s] coordinating patterns
#' n=1  # searches for x(L)x patterns, (L) coordinates to SF4
#' perc=20  # drops residues with less than the 20% of frequency
#' interactive= 0 # interactive. User decided references and dendrogram cut
#' dropsReplicate=0 # do not remove replicated patterns 
#' 
#' ################ Launch PdPDB
#' PdPDB(path_to_PDB,metal,n, perc, interactive, dropsReplicate)
#' @export
PdPDB <- function(path, metal, n, perc, interactive, dropsReplicate){
  
  path_to_commands = file.path(find.package("PdPDB"),"exec/")
  
  if (dropsReplicate>2) dropsReplicate<-2
  if (dropsReplicate<0) dropsReplicate<-0

  # formatting output path
  path_to_out<-paste(paste("cd", path, sep=" "), ";", sep="")

  print("########################################################## ")
  print("          PdPDB: Pattern discovery in PDB files")
  print("########################################################## ")
  
  
  ################ 
  saveLogoText <- 1
  verbose <- 0 # turn to 1 for debug
  
  ################ Long tests
  
  if((interactive > 2)||(interactive<0))
     interactive <- 2
  
  debug<-interactive
  
  if((debug==1)||(debug==2)){
    
    print(paste("Interactive mode: ", debug))
  
    reference<-as.data.frame(c(8.26,1.37,5.46,6.74,3.86,7.08,2.27,5.93,5.82,9.65,2.41,4.06,4.72,3.93,5.53,6.60,5.35,6.86,1.09,2.92))  # expasy
    colnames(reference)<-"referenceVec"
    rownames(reference)<-c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
    numberOfSamples<-554515 # expasy
    averageLength<-357 # expasy
    
    defineSeq<-1
    defineDictionary<-0
      
  }
  
  
  
  # for debug comment the following if section and setup recovery however you prefer
  if (file.exists(file.path(path,"patterns.csv"))){ 
    print("")
    print(paste("PdPDB has found a job to recover!!! ",file.path(path,"patterns.csv")))
    print("If you want to start from scratch remove it and restart!")
    print("WARNING: check n before to proceed. It must be compliant with previous run for PdPDB to work properly!!!")
    print("")
    
    recovery <-1
  }
  else { 
    recovery <-0 
  }
  ################
  
  if((n>5)&&(perc<40)) { 
    print("For optimal results consider to increase Trim frequency or reduce n...")
    print(" ")
  }

  if (verbose==1) { print("VERBOSE MODE ON...")}

  # check if bash scripts are installed correctly - if not escape!
  noScripts <-0
  if ((!file.exists(file.path(path_to_commands,"fileManager")))||(!file.exists(file.path(path_to_commands,"findPatterns")))){
    print(file.path("Some of the core functions of PdPDB are missing, please check paths:", path_to_commands))
    noScripts <-1
  }

  # Ask permission for file system access, if denied abort execution!
  # writeOnFS <- readline(paste(paste("Do you want PdPDB writes results in ", path, sep=" "), "? [1= Yes, 0=No] ", sep=" "))
  writeOnFS <- 1
  
  if (dropsReplicate==2) print("PdPDB now keeps only unique entries...")
  if (dropsReplicate==1) print("PdPDB now drops replicated patterns in PDB entries...")
  if (dropsReplicate==0) print("PdPDB now keeps replicated patterns...")

  if (!(is.na(as.numeric(writeOnFS)==0))&&(as.numeric(writeOnFS)==0)){
    print("PdPDB ABORTED!!!")
    noScripts <-1
  }

  # Safe mode - Backup directories are created
  if (noScripts==0){
    
    command1=paste(path_to_out, (paste(path_to_commands, "fileManager", sep="")))
    system(command1)

    # Verbosity
    if (verbose==1){
      print(command1)
    }
  
    # launch findPatterns
    filesFound <-TRUE
    
    if (recovery==0){
      
      print("PdPDB runs from scratch!")
      
      pdbNames<-list.files(path = path,pattern = "\\.pdb$")
      cifNames<-list.files(path = path,pattern = "\\.cif$")
      filesFound <-FALSE
      
      if((length(pdbNames)>0)||(length(cifNames)>0)) {
        if((length(pdbNames))>(length(cifNames))){
          print("PDB entries correctly located")
          command2=paste(path_to_out, paste(path_to_commands, paste("findPatterns",paste(metal,n,sep=" "),sep=" "), sep=""), sep=" ")
        }
        else{
          print("CIF entries correctly located")
          command2=paste(path_to_out, paste(path_to_commands, paste("findPatternsX",paste(metal,n,sep=" "),sep=" "), sep=""), sep=" ")
        }
        
        if (verbose == 1){ print(command2) }
        
        system(command2)
        filesFound <-TRUE
      }
      else{
        print("Invalid PATH! No PDB entries in this directory!")
      }
      
    } # eo if recover
    
    
    ############### Pattern analysis 
    
    if (isTRUE(filesFound)){
      
      print ("Patterns analysis START!!!!")
      
      # check if file exist before recovery
      if (file.exists(file.path(path,"patterns.csv"))){
        
        # check if the generated files are suitable for analysis before to launch the pipeline, is it > 10b?
        datFileSize<-file.info(file.path(path,"patterns.csv"))$size
        
        # check if file is empty
        if(datFileSize>10){
          if (mean(count.fields(file.path(path,"patterns.csv"))) > 1 ){
            block <- read.csv (file.path(path,"patterns.csv"), header=FALSE, sep=" ", dec=".", stringsAsFactors = FALSE)
            
            if (dropsReplicate!=1) {
              patterns<- arrangePatterns(block, n)
              
              if (dropsReplicate==2) {
                print("---> Drop option 2 <---")
                patterns<-patterns[!duplicated(patterns),]
              }
              
            }
            
            # this will iterate on all the pdb entries
            else  {
              # get starting positions
              print("---> Drop option 1 <---")
              print("Please be patient this option takes time...")
              
              sbsp<-grep("pdb",block[,1]) # subBlockStartPoints = sbsp
              patterns<-subPatternsExtract(block, sbsp, n, verbose)
              
              # pid<-patterns$pid
              # patterns<-patterns[, which(!(colnames(patterns)=="pid"))]
              # patterns<-cbind(patterns, pid)  
              # 
              # patterns<-patterns[,2:length(patterns[1,])]
              
            }
            
            defineDictionary<-0
            
            if(debug==0)
              defineDictionary<-as.integer(readline("Type in the number of additional symbols to be used: [ 0 = no symbols ] "))
            
            if((defineDictionary>=1)&&(!is.na(defineDictionary>=1))){
              dictionary<-defineADictionary(defineDictionary)
            }
            else{
              dictionary<-NA
            }
            
            
            pidlist<-patterns$pid 
            patterns<-patterns[which(!(colnames(patterns)=="pid"))]
            
            fastaPatternsAligned<-alignLigands(toFasta(patterns, dictionary, verbose), n, verbose) # translate to FASTA and align ligands 
            
            # calculate frequencies
            print("Calculating frequencies...")
            
            if (dropsReplicate==1) patterns<-patterns[2:length(patterns[,1]),] # remove the first line - contains only NAs
            
            df<-as.data.frame(count(patterns))
            df.new <- df[with(df, order(df$freq, decreasing = TRUE)), ]
            numberOfPDB<-sum(df.new$freq)
            df.new<-as.data.frame(cbind(df.new, df.new$freq/numberOfPDB*100))
            names(df.new)[length(names(df.new))]<-"freq_%"
            write.table(df.new, file = file.path(path,"frequencies.csv"),row.names=FALSE, na="", sep=" ", quote = FALSE)
            
            fastaPatterns <-checkChains(fastaPatternsAligned, which(colnames(fastaPatternsAligned)=="L"), dictionary, verbose)

            #  trim and plot logo
            percAbs<-abs(perc)
            trimmed<-trimAlignment(fastaPatterns, percAbs, colnames(fastaPatterns), verbose) # removing rows that contains perc% of "*"; ie missing positions
            
            lastCol<-length(trimmed[1,])
            
            for(i in 1:length(trimmed[,1])){
              lastLigand<-(max(which(substr(trimmed[i,],1,1)=="("))+1) # it is the position next to the last L
              
              if(is.finite(lastLigand)){
                if(lastLigand<lastCol)
                  trimmed[i, lastLigand:lastCol]<-moveCharLeft(trimmed[i, lastLigand:lastCol],verbose)
              }
              else print(paste("WARNING at line of the alignment!!! ", i))
                
              
            }
            
            
            if (dropsReplicate==0) {
              trimmedNoDrops<-cbind(trimmed, pidlist)
              write.table(trimmedNoDrops, file = file.path(path,"alignment.csv"),row.names=FALSE, na="", sep=" ", quote = FALSE)
            }
            if (dropsReplicate==1) {
              write.table(trimmed[2:length(trimmed[1,]),], file = file.path(path,"alignment.csv"),row.names=FALSE, na="", sep=" ", quote = FALSE)
            }
            else write.table(trimmed, file = file.path(path,"alignment.csv"),row.names=FALSE, na="", sep=" ", quote = FALSE)
         
            
            
               
            # clustering
            df.clusters<-clustering(trimmed, path, "dendrogram.svg", debug)
            
            # order clusters
            df.clusters.new<-df.clusters[order(df.clusters$points, decreasing = FALSE), ]
            
            numberOfClusters <- max(df.clusters.new$points)
            
            print(paste("This action results in", numberOfClusters,"clusters!", sep=" "))
            
            # make a logo for each cluster
            for(i in 1:numberOfClusters){
              cluster<-subset(df.clusters.new, points==i) # get sequences clustered together - i.e. with the same score
              df.cluster<-(as.data.frame(matrix(unlist(lapply(gsub("\\(|\\)","",cluster$seq), function(x) strsplit(x, split = ""))), nrow=length(cluster[,1]), byrow = TRUE))) # convert the string to a suitable data frame for make logo function
              
              maxFreqSeq<-maxFreq(df.cluster, colnames(trimmed), verbose)
              
              colnames(df.cluster)<-colnames(trimmed)
              
              makeTheLogo(df.cluster, colnames(df.cluster), maxFreqSeq, dictionary, path,paste("logo_",i,".svg",sep=""))
              
              if(saveLogoText==1){
                
                if(verbose==1) print("PdPDB is going to save logos in text format")
                
                df.logo<-as.data.frame(count(df.cluster))
                df.logo.new <- df.logo[with(df.logo, order(df.logo$freq, decreasing = TRUE)), ]
                numberOfPDB<-sum(df.logo.new$freq)
                df.logo.new<-as.data.frame(cbind(df.logo.new, df.logo.new$freq/numberOfPDB*100))
                names(df.logo.new)<-c(names(df.cluster),"freq","freq_%") 
                
                
                if(dropsReplicate==0)
                {
                  print(df.logo.new)
                  trimmedNoDrops<-as.data.frame(apply(trimmedNoDrops, c(1,2), suppressBrackets))
                  
                  old_colnames<-colnames(df.logo.new)
                  
                  colnames(df.logo.new)[which(!(colnames(df.logo.new)=="freq"|colnames(df.logo.new)=="freq_%"))]<-rep(1:length(colnames(df.logo.new)[which(!(colnames(df.logo.new)=="freq"|colnames(df.logo.new)=="freq_%"))]))
                  colnames(trimmedNoDrops)[which(!(colnames(trimmedNoDrops)=="pidlist"))]<-rep(1:length(colnames(trimmedNoDrops)[which(!(colnames(trimmedNoDrops)=="pidlist"))]))
                  
                  df.logo.new<-join(df.logo.new, trimmedNoDrops) # this will attache the pid column to the data.frame
                  colnames(df.logo.new)<-c(old_colnames, "pid")
                  write.csv(as.data.frame(df.logo.new), file = file.path(path,paste("logo_",i,".csv",sep="")),row.names=FALSE, col.names=colnames(trimmed), na="", sep=" ", quote = FALSE) 
                }
                else write.csv(as.data.frame(df.logo.new), file = file.path(path,paste("logo_",i,".csv",sep="")),row.names=FALSE, col.names=colnames(trimmed), na="", sep=" ", quote = FALSE)
              }
              
            }
            # EO Clustering
            
            # enrichement analysis
            # bonferroniCorrection <- NA
            if(debug==0) # remove after debug
              defineSeq<-as.numeric(readline("Do you want to define a reference sequence for enrichment analysis? [ Yes = 1 ] ")) # turn on after debug
            
            if((defineSeq==1)&&(!is.na(defineSeq==1))){
              
              print("This will save a CSV file containing standardized distribution and Chi-squared p values")
              
              if(debug==0){ 
                
                # definition of the reference
                averageLength <- as.numeric(readline("Type in the average sequence length of the reference:  "))
                numberOfSamplesP <- as.numeric(readline("PROTEINS:      How many samples have been used to define this reference? "))
                
                if((numberOfSamplesP>0)&&(!is.na(numberOfSamplesP))) {
                  reference<-defineSeqRef(1,0)
                  numberOfSamples<-numberOfSamplesP
                }
                
                else{
                  reference<-defineSeqRef(1,0)
                  numberOfSamples<-0
                  print("###### ERROR: a void reference has been defined!!!")
                }
                
              }
              
              # root enrichment analysis
              enrichmentAnalysis(trimmed, numberOfPDB, reference, numberOfSamples, averageLength, path, "root", verbose)
              
              # enrichment for ligands
              ligands<-trimmed[which(colnames(trimmed) %in% "L")]
              enrichmentAnalysis(ligands, numberOfPDB, reference, numberOfSamples, averageLength, path, "ligands", verbose)
              
              notligands<-trimmed[which(colnames(trimmed) %in% "x")]
              enrichmentAnalysis(notligands, numberOfPDB, reference, numberOfSamples, averageLength, path, "notLigands", verbose)
              
              colnumbersP<-NA
              colnumbersF<-NA
              
              # enrichment analysis position-by-position
              for(i in 1:n){
              
                colnumbersP<-(which((substr(colnames(trimmed),1,1)) %in% "L"))-i  # select -i positions
                colnumbersP.filt<-colnumbersP[which(colnumbersP %in% 1:length(trimmed[1,]))] # take the possible ones
                preceed.old<-trimmed[colnumbersP.filt]
                preceed<-preceed.old[substr(colnames(preceed.old),1,1) != "L"] # discard ligands
                enrichmentAnalysis(preceed, numberOfPDB, reference, numberOfSamples, averageLength, path, paste("preceeding",i,sep="_"), verbose)
                
                colnumbersF<-(which((substr(colnames(trimmed),1,1)) %in% "L"))+i  
                colnumbersF.filt<-colnumbersF[which(colnumbersF %in% 1:length(trimmed[1,]))]
                follow.old<-trimmed[colnumbersF.filt]
                follow<-follow.old[substr(colnames(follow.old),1,1) != "L"]
                enrichmentAnalysis(follow, numberOfPDB, reference, numberOfSamples, averageLength, path, paste("following",i,sep="_"), verbose)

              }
              
            } # EO Enrichment
            
            saveReport<-0
            saveReport <- as.numeric(readline("Do you want to generate a single report file? [ Yes = 1 ]"))
            
            if(saveReport==1){
              print("A single pdf file will be generated. CSV and SVG files will be removed...")
              generateReport(path, verbose)
            }
            
            
            
          }
          else print("PdPDB is not able to get suitable PATTERNS... check PdPDB.log file!")
          
        }
        else print("Empty patterns.csv file!")
      }
      else print ("Missing patterns.csv: No file to recovery!")
    }
    
    
    # No patterns analysis
    else {print ("Patterns won't be analyzed!!!")}
    
  }
  
  system(paste("rm -f ", path,"*.tmp", sep = ""))
  system(paste("echo 'Alignment has been trimmed to ", perc,"%' >> ",path,"PdPDB.log", sep = ""))  
  system(paste("echo 'Drop option was ", dropsReplicate,"!' >> ",path,"PdPDB.log", sep = ""))  
  
  if(recovery==1) {
    system(paste("echo 'You recovered an old job' >> ",path,"PdPDB.log", sep = ""))  
    system(paste("echo 'n is now tuned to ", n,"' >> ",path,"PdPDB.log", sep = ""))  
  }
  
  print(" ")
  print("---------------------------------------------------------- ")
  print(paste("Logos and output files (.csv, .svg, .log) are in ",path,sep=" "))
  print("---------------------------------------------------------- ")
  print(" ")
  print("Legend for 'frequencies.csv' file: () = ligand, - = conect residues interleaved by of more than a position in the same chain, ... = conect residues from different chains")
  print("Legend for logos and 'alignment.csv' file: * = gap introduced by ligands alignment")
  print(" ")
  print("########################################################## ")
  system(paste("echo ' ' >> ",path,"PdPDB.log", sep = ""))  
  system(paste("echo 'PdPDB exited gracefully...!!!' >> ",path,"PdPDB.log", sep = ""))  
  print("            PdPDB exited gracefully...!!!")
  print("########################################################## ")


}
