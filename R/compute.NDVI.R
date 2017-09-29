#'@title Calcule le NDVI à partir de la réflectance hyperspectrale pour différents
#'     capteurs satellitaires (Landsat4or5-TM, Landsat7-ETM+, Landsat8-OLI,
#'     Sentinel2-MSI, Pleiades and WorldView)
#'
#'@description L'indice de végétation NDVI (Normalized Difference Vegetation Index)
#'     est la différence entre la réflectance dans le proche infrarouge
#'     et la réflectance dans le rouge normalisé par la somme des deux.
#'     Le NDVI varie théoriquement entre -1 et 1.
#'     La fonction considère les différences spectrales entre les capteurs.
#'
#'
#'@param waves est un vecteur de longueur d'onde
#'@param R est la reflectance spectrale
#'@param sensor est le nom du capteur satellitaire,
#'   i.e. TM, ETM+, OLI, MSI, PLEIADES ou WV.
#'   (Par défaut sensor = "OLI)
#'
#'@return Retourne la valeur du NDVI
#'
#'
#' @author Simon Bélanger

compute.NDVI <- function(waves, R, sensor="OLI") {

  if (sensor == 'OLI') {
    ix.red = which(waves >= 635 & waves <= 673)
    ix.NIR = which(waves >= 850 & waves <= 879)
  }

  if (sensor == 'TM') {
    ix.red = which(waves >= 630 & waves <= 690)
    ix.NIR = which(waves >= 760 & waves <= 900)
  }

  if (sensor == 'ETM') {
    ix.red = which(waves >= 630 & waves <= 690)
    ix.NIR = which(waves >= 780 & waves <= 900)
  }

  if (sensor == 'MSI') {
    ix.red = which(waves >= 651 & waves <= 678)
    ix.NIR = which(waves >= 856 & waves <= 874)
  }

  if (sensor == 'PLEIADES') {
    ix.red = which(waves >= 600 & waves <= 720)
    ix.NIR = which(waves >= 750 & waves <= 950)
  }

  if (sensor == 'WV') {
    ix.red = which(waves >= 630 & waves <= 690)
    ix.NIR = which(waves >= 770 & waves <= 895)
  }


  R.red = mean(R[ix.red], na.rm=T)
  R.NIR = mean(R[ix.NIR], na.rm=T)

  NDVI = (R.NIR-R.red)/(R.NIR+R.red)

  return(NDVI)


}
