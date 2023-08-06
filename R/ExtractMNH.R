#' MNH par placette
#'
#' @description Extraction par placette des valeurs du MNH puis calcul de la moyenne
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import exactextractr
#'
#' @examples
#' MnhExtract(mnh, plac)
#'
#' @author Bruciamacchie Max
#'
#' @export
#'

MnhExtract <- function(dem, shpPlac, buffer=15) {
  poly = shpPlac %>%
    st_as_sfc() %>%
    st_buffer(dist = buffer)
  values = exact_extract(dem, poly, 'mean')
  return(values)
}


