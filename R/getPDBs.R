#' Pattern Discovery in PDB Structures of Metalloproteins
#'
#' @author Luca Belmonte
#' @usage getPDBs(PDBlist, path, ext = "pdb")
#' @description Download a list of PDB files to a local path
#' @references Belmonte L, Mansy SS Patterns of Ligands Coordinated to Metallocofactors Extracted from the Protein Data Bank, Journal of Chemical Information and Modeling (accepted)
#' @param PDBlist A vector of strings. Contains the target PDB files to be downloaded.
#' @param path A string containing the local path to which the PDBs will be downloaded.
#' @param ext A string. Either 'pdb' or 'cif'.
#' @note Files will be downloaded in a local folder as specified in path.
#' @importFrom HelpersMG wget

getPDBs <- function(PDBlist, path, ext = "pdb"){
  
  lapply(PDBlist, 
             function(x) {
               url <- paste0("http://www.rcsb.org/pdb/files/", x, ".", ext, ".gz")
               HelpersMG::wget(url = url, destfile = file.path(path, paste0(x, ".", ext)))
               
            }
               
  )
  
}
