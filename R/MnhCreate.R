#' MNH par placette
#'
#' @description Extraction par placette des valeurs du MNH puis calcul de la moyenne
#'
#' @import sf
#' @import tidyverse
#' @import raster
#'
#' @examples
#' MnhCreate(mnh, plac)
#'
#' @author Bruciamacchie Max
#'
#' @export
#'



# library(tidyverse)
# library(sf)
# library(raster)

# perim <- st_read("/Users/maxbruciamacchie/pCloudSync/EnCours/Possibilités/Data/Foret3/SIG/Vecteurs/ParroyParc.gpkg")

MnhCreate <- function(mns, mnt, perim) {

  mns <- raster("/Users/maxbruciamacchie/pCloudSync/EnCours/GrandEst/Parroy/Mnh.tif") %>%
    aggregate(fact=5)

  mnt <- raster("/Users/maxbruciamacchie/pCloudSync/EnCours/Possibilités/Data/Foret3/SIG/Rasters/MntParroy.tif") %>%
    resample(mns)

  mnh = mns - mnt
  mnh[mnh < 0] <- 0

  cad <- st_read("/Users/maxbruciamacchie/pCloudSync/EnCours/Ecole/DonneesBrutes/Vecteurs/Parcelles.gpkg") %>%
    filter(commune == "54578" & section == "AN" & numero == "11")

  mnh <- mnh %>%
    crop(perim)
  mnh1 <- mnh %>%
    mask(perim)
  mnh2 <- mnh1
  mnh2[mnh2 < 25] <- 0

  plot(mnh2)

  writeRaster(mnh, "MnhParroy.tif", overwrite=TRUE)
  writeRaster(mnh1, "/Users/maxbruciamacchie/pCloudSync/EnCours/Possibilités/Data/Foret3/SIG/Rasters/Mn1hParroy.tif", overwrite=TRUE)
  writeRaster(mnh2, "/Users/maxbruciamacchie/pCloudSync/EnCours/Possibilités/Data/Foret3/SIG/Rasters/Mn2hParroy.tif", overwrite=TRUE)
}
