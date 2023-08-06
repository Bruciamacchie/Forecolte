#' MNH par placette
#'
#' @description Extraction par placette des valeurs du MNH puis calcul de la moyenne
#'
#' @param num = paramètre d'une loi lognormanle
#'
#' @examples
#' MnhExtract(mnh, plac)
#'
#' @author Bruciamacchie Max
#'
#' @export
#'

Prelvt <- function(x, x0, param) {
  y = plnorm(x, 1, param)/plnorm(x0, 1, param)
  return(y)
}
