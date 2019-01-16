checkLigand <- function(y){

  x<-toupper(y)
  
  itis <- FALSE
  
  if (((as.character(x)=="HOH")&&(!(is.na(as.character(x)=="HOH")))) || ((as.character(x)== "ALA")&&(!(is.na(as.character(x)== "ALA")))) || ((as.character(x)== "CYS")&&(!(is.na(as.character(x)== "CYS")))) || ((as.character(x)== "ASP")&&(!(is.na(as.character(x)== "ASP")))) || ((as.character(x)== "GLU")&&(!(is.na(as.character(x)== "GLU"))))   || ((as.character(x)== "PHE")&&(!(is.na(as.character(x)== "PHE")))) || ((as.character(x)== "GLY")&&(!(is.na(as.character(x)== "GLY")))) || ((as.character(x)== "HIS")&&(!(is.na(as.character(x)== "HIS")))) || ((as.character(x)== "ILE")&&(!(is.na(as.character(x)== "ILE")))) || ((as.character(x)== "LYS")&&(!(is.na(as.character(x)== "LYS")))) || ((as.character(x)== "LEU")&&(!(is.na(as.character(x)== "LEU")))) || ((as.character(x)== "MET")&&(!(is.na(as.character(x)== "MET")))) || ((as.character(x)== "ASN")&&(!(is.na(as.character(x)== "ASN")))) || ((as.character(x)== "PRO")&&(!(is.na(as.character(x)== "PRO")))) || ((as.character(x)== "GLN")&&(!(is.na(as.character(x)== "GLN")))) || ((as.character(x)== "ARG")&&(!(is.na(as.character(x)== "ARG")))) || ((as.character(x)== "SER")&&(!(is.na(as.character(x)== "SER")))) || ((as.character(x)== "THR")&&(!(is.na(as.character(x)== "THR")))) || ((as.character(x)== "VAL")&&(!(is.na(as.character(x)== "VAL")))) || ((as.character(x)== "TRP")&&(!(is.na(as.character(x)== "TRP")))) || ((as.character(x)== "TYR")&&(!(is.na(as.character(x)== "TYR")))) || ((as.character(x)== "A")&&(!(is.na(as.character(x)== "A")))) || ((as.character(x)== "C")&&(!(is.na(as.character(x)== "C")))) || ((as.character(x)== "G")&&(!(is.na(as.character(x)== "G")))) || ((as.character(x)== "T")&&(!(is.na(as.character(x)== "T")))) || ((as.character(x)== "U")&&(!(is.na(as.character(x)== "U")))) || ((as.character(x)== "DA")&&(!(is.na(as.character(x)== "DA")))) || ((as.character(x)== "DC")&&(!(is.na(as.character(x)== "DC")))) || ((as.character(x)== "DG")&&(!(is.na(as.character(x)== "DG")))) || ((as.character(x)== "DT")&&(!(is.na(as.character(x)== "DT"))))  ){
    itis <-TRUE
  }
  
  itis

}
