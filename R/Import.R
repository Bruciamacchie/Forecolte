#' Import data.
#'
#' @description Import des informations nécessaires au calcul des volumes mobilisables.
#'
#' @param rep  répertoire conteant les données
#'
#' @import tidyverse
#' @import sf

#' @examples
#' rep = "/Users/maxbruciamacchie/pCloudSync/EnCours/Possibilités/Data/Foret1"
#' ImportForestCrop(rep)
#'
#' @author Bruciamacchie Max
#'
#' @export

ImportForestCrop <- function(rep=getwd()){
  load(paste(rep, "TablesBrutes.RData", sep="/"))
  load(paste(rep, "TablesElaboreesPlac.RData", sep="/"))
  plac  <- st_read(paste(rep, "Plac.gpkg", sep="/"), quiet=T)
  perim <- st_read(paste(rep, "Perim.gpkg", sep="/"), quiet=T)
  Nbre <- dim(plac)[1]
  Surf = Forets$SurfEchan

  out <- list(plac,perim,Nbre,Surf, gfPla,arbres,Forets,EssReg)
  names(out) <- c("plac","perim","Nbre","Surf", "gfPla","arbres","Forets","EssReg")
  return(out)
}
