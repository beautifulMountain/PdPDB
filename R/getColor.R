getColor <- function(x){
  
  # orange: Non-polar (G, A, V, L, I, F, W, M, P)
  # green3: Polar, uncharged (S, T, C, Y, N, Q)
  # red: Polar, acidic (D, E)
  # blue: Polar, basic (K, R, H)
  # gray: * - missing residues
  
  color<-""
  
  if((x== "A")&&(!(is.na(x== "A")))) color<-"orange" # amino acids
  else if((x== "C")&&(!(is.na(x== "C")))) color<-"green3"
  else if((x== "D")&&(!(is.na(x== "D")))) color<-"red"
  else if((x== "E")&&(!(is.na(x== "E")))) color<-"red"
  else if((x== "F")&&(!(is.na(x== "F")))) color<-"orange"
  else if((x== "G")&&(!(is.na(x== "G")))) color<-"orange"
  else if((x== "H")&&(!(is.na(x== "H")))) color<-"blue"
  else if((x== "I")&&(!(is.na(x== "I")))) color<-"orange"
  else if((x== "K")&&(!(is.na(x== "K")))) color<-"blue"
  else if((x== "L")&&(!(is.na(x== "L")))) color<-"orange"
  else if((x== "M")&&(!(is.na(x== "M")))) color<-"orange"
  else if((x== "N")&&(!(is.na(x== "N")))) color<-"green3"
  else if((x== "P")&&(!(is.na(x== "P")))) color<-"orange"
  else if((x== "Q")&&(!(is.na(x== "Q")))) color<-"green3"
  else if((x== "R")&&(!(is.na(x== "R")))) color<-"blue"
  else if((x== "S")&&(!(is.na(x== "S")))) color<-"green3"
  else if((x== "T")&&(!(is.na(x== "T")))) color<-"green3"
  else if((x== "V")&&(!(is.na(x== "V")))) color<-"orange"
  else if((x== "W")&&(!(is.na(x== "W")))) color<-"orange"
  else if((x== "Y")&&(!(is.na(x== "Y")))) color<-"green3"
  else if((x== "(A)")&&(!(is.na(x== "(A)")))) color<-"orange"
  else if((x== "(C)")&&(!(is.na(x== "(C)")))) color<-"green3"
  else if((x== "(D)")&&(!(is.na(x== "(D)")))) color<-"red"
  else if((x== "(E)")&&(!(is.na(x== "(E)")))) color<-"red"
  else if((x== "(F)")&&(!(is.na(x== "(F)")))) color<-"orange"
  else if((x== "(G)")&&(!(is.na(x== "(G)")))) color<-"orange"
  else if((x== "(H)")&&(!(is.na(x== "(H)")))) color<-"blue"
  else if((x== "(I)")&&(!(is.na(x== "(I)")))) color<-"orange"
  else if((x== "(K)")&&(!(is.na(x== "(K)")))) color<-"blue"
  else if((x== "(L)")&&(!(is.na(x== "(L)")))) color<-"orange"
  else if((x== "(M)")&&(!(is.na(x== "(M)")))) color<-"orange"
  else if((x== "(N)")&&(!(is.na(x== "(N)")))) color<-"green3"
  else if((x== "(P)")&&(!(is.na(x== "(P)")))) color<-"orange"
  else if((x== "(Q)")&&(!(is.na(x== "(Q)")))) color<-"green3"
  else if((x== "(R)")&&(!(is.na(x== "(R)")))) color<-"blue"
  else if((x== "(S)")&&(!(is.na(x== "(S)")))) color<-"green3"
  else if((x== "(T)")&&(!(is.na(x== "(T)")))) color<-"green3"
  else if((x== "(V)")&&(!(is.na(x== "(V)")))) color<-"orange"
  else if((x== "(W)")&&(!(is.na(x== "(W)")))) color<-"orange"
  else if((x== "(Y)")&&(!(is.na(x== "(Y)")))) color<-"green3"
  else if((x== "a")&&(!(is.na(x== "a")))) color<-"pink" # nucleotides
  else if((x== "c")&&(!(is.na(x== "c")))) color<-"pink"
  else if((x== "t")&&(!(is.na(x== "t")))) color<-"pink"
  else if((x== "g")&&(!(is.na(x== "g")))) color<-"pink"
  else if((x== "u")&&(!(is.na(x== "u")))) color<-"pink"
  else if((x== "y")&&(!(is.na(x== "y")))) color<-"pink"
  else if((x== "(a)")&&(!(is.na(x== "(a)")))) color<-"pink"
  else if((x== "(c)")&&(!(is.na(x== "(c)")))) color<-"pink"
  else if((x== "(t)")&&(!(is.na(x== "(t)")))) color<-"pink"
  else if((x== "(g)")&&(!(is.na(x== "(g)")))) color<-"pink"
  else if((x== "(u)")&&(!(is.na(x== "(u)")))) color<-"pink"
  else if((x== "(y)")&&(!(is.na(x== "(y)")))) color<-"pink"
  else if((x== "(O)")&&(!(is.na(x== "(O)")))) color<-"cyan" # WATER
  else if((x== "O")&&(!(is.na(x== "O")))) color<-"cyan" # WATER
  else if((x== "*")&&(!(is.na(x== "*")))) color<-"snow2" # gaps
  else color<-"black"
  
  color
  
}