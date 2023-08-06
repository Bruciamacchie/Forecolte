#' Dessin krigeage
#'
#' @description Dessin carte issue du krigeage
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#' @import automap
#'
#' @examples
#' KrigeageMnhPlot(r, 20, 2, UG)
#'
#' @author Bruciamacchie Max
#'
#' @export

KrigeageMnhPlot <- function(r, ampli, num, shpUG, courbes=FALSE) {
  b = seq(0, floor((max(r[[num]], na.rm=T))/ampli+0.5)*ampli, ampli)
  plot(r[num], breaks = b, col = hcl.colors(length(b)-1, "Spectral"), reset = FALSE)
  plot(st_geometry(shpUG), pch = 3, add = TRUE, border='blue')
  if(courbes) {
    contour(r[num], breaks = b, add = TRUE, lwd=0.3)
  }
}


