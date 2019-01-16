arrangePatterns <- function(blockRaw, n){

  print("Adjusting patterns...")

  block <- blockRaw
  
  end <- as.integer(length(block[,1]))
  
  patterns <- vector("list", end) 
  pid <- array(NA, dim = c(end,1)) 
  

  i<-1
  k<-0 # number of blocks (pseudo patterns) found in PDB format
  
  for (i in 1:end){
    
    if((identical(substr(block[i,1], 5, 8), ".cif"))||(identical(substr(block[i,1], 5, 8), ".pdb"))||(identical(substr(block[i,1], 1, 7), "Warning"))){
      k<-k+1
      pid[i]<-block[i,1]
    } #end if on block stats
    
    else{
      
      if(((as.character(block[i,4])==as.character(block[i+1,4]))&&(!is.na(as.character(block[i,4])==as.character(block[i+1,4]))))&&((as.integer(block[i,5])==as.integer(block[i+1,5]))&&(!is.na(as.integer(block[i,5])==as.integer(block[i+1,5]))))){ # coordinated to same metal
        
        patterns[[k]] <- c(patterns[[k]], as.character(block[i,1]))
        
        if(identical(substr(toupper(as.character(block[i+1,1])),2,4),"HOH")) {
          patterns[[k]] <- c(patterns[[k]], "+")
        }
        else{
          if(identical(substr(block[i,1], 1, 1), "(")){
            if(nchar(block[i,1])==5){ # aminos
              if(!(checkLigand(substr(block[i,1],2,4)))){
                patterns[[k]] <- c(patterns[[k]], "+")
              } 
            }
            if(nchar(block[i,1])==4){ # rna nucleotides
              if(!(checkLigand(substr(block[i,1],2,3)))){
                patterns[[k]] <- c(patterns[[k]], "+")
              }
            }
            if(nchar(block[i,1])==3){ # dna nucleotides
              if(!(checkLigand(substr(block[i,1],2,2)))){
                patterns[[k]] <- c(patterns[[k]], "+")
              }
            }
          }
          else{
            if (as.character(block[i,2])!=as.character(block[i+1,2])){
              patterns[[k]] <- c(patterns[[k]], "+")
            }
            else{
              if(((as.integer(block[i,3])-as.integer(block[i+1,3]))<(-1))&&(!is.na((as.integer(block[i,3])-as.integer(block[i+1,3]))))){
                patterns[[k]] <- c(patterns[[k]], "-")
              }
            }
          }
        }# not water
        
      }
      else {
        patterns[[k]] <- c(patterns[[k]], as.character(block[i,1]))
        k<-k+1
      } # not coordinated to the same metal
    } # else skip '.pdb'
    
  }
  
  dfpatterns<-data.frame(ldply(patterns, rbind), stringsAsFactors=FALSE) # convert string to data frame and then as file
  
  pid<-as.data.frame(na.omit(pid))
  names(pid)<-"pid"
  
  dfpatterns<-cbind(dfpatterns, pid) # hanno un numero diverso di righe! - ERRORE? - questo può dipendere da come si lancia il tool se conta la frequenza o meno del pattern. In tal caso deve essere possibile inserire il pdb id con fillNAsPIDlist
  dfpatterns.new<-dfpatterns[apply(dfpatterns,1,function(x)any(!is.na(x))),] # delete null values
  
  dfpatterns.new
  
} #EOF
