AAAtoA <- function(y, dictionary, verbose){

  if (identical(substr(y,1,1), "(")) translated<-"(Z)"
  else translated<-"Z"
  
  scanRes<-NA
  
  x<-as.character(toupper(y))

  if((x== "ALA")&&(!(is.na(x== "ALA")))) translated<-"A"
  else if((x== "CYS")&&(!(is.na(x== "CYS")))) translated<-"C"
  else if((x== "ASP")&&(!(is.na(x== "ASP")))) translated<-"D"
  else if((x== "GLU")&&(!(is.na(x== "GLU")))) translated<-"E"
  else if((x== "PHE")&&(!(is.na(x== "PHE")))) translated<-"F"
  else if((x== "GLY")&&(!(is.na(x== "GLY")))) translated<-"G"
  else if((x== "HIS")&&(!(is.na(x== "HIS")))) translated<-"H"
  else if((x== "ILE")&&(!(is.na(x== "ILE")))) translated<-"I"
  else if((x== "LYS")&&(!(is.na(x== "LYS")))) translated<-"K"
  else if((x== "LEU")&&(!(is.na(x== "LEU")))) translated<-"L"
  else if((x== "MET")&&(!(is.na(x== "MET")))) translated<-"M"
  else if((x== "ASN")&&(!(is.na(x== "ASN")))) translated<-"N"
  else if((x== "PRO")&&(!(is.na(x== "PRO")))) translated<-"P"
  else if((x== "GLN")&&(!(is.na(x== "GLN")))) translated<-"Q"
  else if((x== "ARG")&&(!(is.na(x== "ARG")))) translated<-"R"
  else if((x== "SER")&&(!(is.na(x== "SER")))) translated<-"S"
  else if((x== "THR")&&(!(is.na(x== "THR")))) translated<-"T"
  else if((x== "VAL")&&(!(is.na(x== "VAL")))) translated<-"V"
  else if((x== "TRP")&&(!(is.na(x== "TRP")))) translated<-"W"
  else if((x== "TYR")&&(!(is.na(x== "TYR")))) translated<-"Y"
  else if((x== "(ALA)")&&(!(is.na(x== "(ALA)")))) translated<-"(A)"
  else if((x== "(CYS)")&&(!(is.na(x== "(CYS)")))) translated<-"(C)"
  else if((x== "(ASP)")&&(!(is.na(x== "(ASP)")))) translated<-"(D)"
  else if((x== "(GLU)")&&(!(is.na(x== "(GLU)")))) translated<-"(E)"
  else if((x== "(PHE)")&&(!(is.na(x== "(PHE)")))) translated<-"(F)"
  else if((x== "(GLY)")&&(!(is.na(x== "(GLY)")))) translated<-"(G)"
  else if((x== "(HIS)")&&(!(is.na(x== "(HIS)")))) translated<-"(H)"
  else if((x== "(ILE)")&&(!(is.na(x== "(ILE)")))) translated<-"(I)"
  else if((x== "(LYS)")&&(!(is.na(x== "(LYS)")))) translated<-"(K)"
  else if((x== "(LEU)")&&(!(is.na(x== "(LEU)")))) translated<-"(L)"
  else if((x== "(MET)")&&(!(is.na(x== "(MET)")))) translated<-"(M)"
  else if((x== "(ASN)")&&(!(is.na(x== "(ASN)")))) translated<-"(N)"
  else if((x== "(PRO)")&&(!(is.na(x== "(PRO)")))) translated<-"(P)"
  else if((x== "(GLN)")&&(!(is.na(x== "(GLN)")))) translated<-"(Q)"
  else if((x== "(ARG)")&&(!(is.na(x== "(ARG)")))) translated<-"(R)"
  else if((x== "(SER)")&&(!(is.na(x== "(SER)")))) translated<-"(S)"
  else if((x== "(THR)")&&(!(is.na(x== "(THR)")))) translated<-"(T)"
  else if((x== "(VAL)")&&(!(is.na(x== "(VAL)")))) translated<-"(V)"
  else if((x== "(TRP)")&&(!(is.na(x== "(TRP)")))) translated<-"(W)"
  else if((x== "(TYR)")&&(!(is.na(x== "(TYR)")))) translated<-"(Y)"
  else if((x== "A")&&(!(is.na(x== "A")))) translated<-"a" # nucleic acids
  else if((x== "C")&&(!(is.na(x== "C")))) translated<-"c"
  else if((x== "T")&&(!(is.na(x== "T")))) translated<-"t"
  else if((x== "G")&&(!(is.na(x== "G")))) translated<-"g"
  else if((x== "U")&&(!(is.na(x== "U")))) translated<-"u"
  else if((x== "Y")&&(!(is.na(x== "Y")))) translated<-"y"
  else if((x== "(A)")&&(!(is.na(x== "(A)")))) translated<-"(a)" 
  else if((x== "(C)")&&(!(is.na(x== "(C)")))) translated<-"(c)"
  else if((x== "(T)")&&(!(is.na(x== "(T)")))) translated<-"(t)"
  else if((x== "(G)")&&(!(is.na(x== "(G)")))) translated<-"(g)"
  else if((x== "(U)")&&(!(is.na(x== "(U)")))) translated<-"(u)"
  else if((x== "(Y)")&&(!(is.na(x== "(Y)")))) translated<-"(y)"
  else if((x== "DA")&&(!(is.na(x== "DA")))) translated<-"a" 
  else if((x== "DC")&&(!(is.na(x== "DC")))) translated<-"c"
  else if((x== "DT")&&(!(is.na(x== "DT")))) translated<-"t"
  else if((x== "DG")&&(!(is.na(x== "DG")))) translated<-"g"
  else if((x== "DU")&&(!(is.na(x== "DU")))) translated<-"u"
  else if((x== "(DA)")&&(!(is.na(x== "(DA)")))) translated<-"(a)" 
  else if((x== "(DC)")&&(!(is.na(x== "(DC)")))) translated<-"(c)"
  else if((x== "(DT)")&&(!(is.na(x== "(DT)")))) translated<-"(t)"
  else if((x== "(DG)")&&(!(is.na(x== "(DG)")))) translated<-"(g)"
  else if((x== "(DU)")&&(!(is.na(x== "(DU)")))) translated<-"(u)"
  else if((x== "(HOH)")&&(!(is.na(x== "(HOH)")))) translated<-"(O)" # WATER
  else if((x== "-")&&(!(is.na(x== "-")))) translated<-"-"
  else if((x== "+")&&(!(is.na(x== "+")))) translated<-"+"
  else if((x== " ")&&(!(is.na(x== " ")))) translated<-NA
  else {
    
    if (verbose==1) print("A non standard 3letter code has been located!!!")
    
    scanRes<-scanDictionary(x, dictionary, verbose) # scan the dictionary
    if(!is.na(scanRes)) translated <- scanRes
  }
  
  translated
}
