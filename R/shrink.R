#' Shrink a PDB
#'
#' @author Luca Belmonte
#' @usage shrink(PDBlist, filePath, resName, eleName, n)
#' @description 
#' @references Belmonte L, Mansy SS Patterns of Ligands Coordinated to Metallocofactors Extracted from the Protein Data Bank, Journal of Chemical Information and Modeling (accepted)
#' @param filePath A string. Contains the file name along with the full path of the target PDB.
#' @param resName A string. This is the name of the residue that contains the prosthetic center; e.g. ZN for zinc or FES for 2Fe2S cluster.
#' @param eleName A string. This is the chemical PDB name of the prosthetic center; e.g. ZN for zinc or FES for 2Fe2S cluster.
#' @param n An integer. This is the length of the inspecting window.
#' @importFrom Rpdb read.pdb
#' @importFrom dplyr as_tibble filter select pull

shrink <- function(filePath, resName, eleName, n){
  
  var <- "resid"
  
  pdbFile <- Rpdb::read.pdb(file = filePath, ATOM = T, HETATM = T, CONECT = T)
  
  elementId <- pdbFile$atoms %>% 
    as_tibble() %>% 
    filter(resname == toupper(resName) | elename == toupper(eleName)) %>% 
    select(!!!vars(var))
  
  # genereates a vector of elements id to account for within the inspecting windo
  residuesToAccount <- (elementId %>% pull() - n):(elementId %>% pull() + n)
  
  # shrink the target pdb file in the element window
  shrinkedPdb <- pdbFile$atoms %>% as_tibble() %>% 
    filter(resid == residuesToAccount)
  
  return(shrinkedPdb)

}
