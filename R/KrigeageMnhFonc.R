#' Fonction Krigeage avec MNH
#'
#' @description Fonction krigeage pour conversion des données ponctuelles. Prise en compte d'un MNH
#'
#' @import sf
#' @import tidyverse
#' @import stars
#' @import gstat
#' @import automap
#'
#' @examples
#' k <- KrigeageMnhFonc(perim, plac, mnh15, idvar="Vha")
#'
#' @author Bruciamacchie Max
#'
#' @export

KrigeageMnhFonc <- function(shpPerim, shpPlac, mnh, pas = 25, idvar="Gha") {

  # ------------ Fabrication du grid
  grd = st_as_sfc(st_bbox(shpPerim))
  grd = st_as_stars(grd, dx = pas, dy = pas)

  mne = st_warp(src = mnh15, grd, method = "average", use_gdal = TRUE)
  mne = mne[perim]
  mne = st_normalize(mne)
  names(mne) = "mnh15"

  # ------------ Vérification
  if (missing(grd)) stop("missing grd")
  if (missing(shpPlac)) stop("missing shpPlac")
  if(st_crs(shpPlac) != st_crs(grd)) {
    shpPlac <- shpPlac %>% st_transform(st_crs(grd))
  }

  # ------------ Choix de la variable
  pos <- which(names(shpPlac) == idvar)
  names(shpPlac)[pos] <- "Y"
  shpPlac <- shpPlac %>%
    mutate(Y = ifelse(is.na(Y), 0, Y))

  # ------------ Krigeage
  vgmh = autofitVariogram(Y ~ mnh15, as(shpPlac, "Spatial"))
  g = gstat(formula = Y ~ mnh15, model = vgmh$var_model, data = shpPlac)
  z = predict(g, mne)
  names(z)[1] <- idvar
  z$var1.var <- NULL
  z[z < 0] <- 0
  # Gha <- z
  # r <- z
  return(z)
}
