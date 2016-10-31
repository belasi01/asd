#' Calcule la réflectance spectrale à partir de deux mesures d'ASD 
#' (surface et spectralon). 
#' Compute spectral reflectance from two raw ASD measurements
#'
#' @param spectralon.file est le nom complet du fichier ASD de la mesure du spectralon. Is the full name and path of the spectralon file
#' @param surface.file  est le nom complet du fichier ASD de la mesure de la surface. Is the full name and path of the surface file
#' @param rho.spectralon est la réflectance du spectralon (valeur par défaut étant de 0.98). Is the reflectivity of the spectralon panel. Default is 0.98
#' @return Elle retourne une liste de cinq varaibles. It returns a list of five variables:
#'
#'
#' waves= Les longueur d'onde. The wavelenght vector;
#'
#'
#' L.surf=Luminance de la surface normalisée par le temps d'intégration. Raw radiance normalized by the integration time of the surface;
#'
#'
#'   L.spectralon=Luminance du spectralon normalisée par le temps d'intégration. Raw radiance normalized by the integration time of the spectralon;
#'
#'
#' rho= La réflecntance de la surface. The surface reflectance;
#'
#'
#' deltaTime = La différence de temps entre les mesures respectives du spectralon et de la surface. Is the time interval between the surface and spectralon measurements
#' @example
#'
#' @author Simon Bélanger
#'
compute.ASD.rho <- function(spectralon.file,
                            surface.file,
                            rho.spectralon=0.98)
  {

  spectralon = read.ASD(spectralon.file)
  surface = read.ASD(surface.file)

  rho = surface$L.norm / (spectralon$L.norm /rho.spectralon)

  deltaTime = difftime(spectralon$DateTime, surface$DateTime)

  return(list(waves=spectralon$waves,
              L.surf=surface$L.norm,
              L.spectralon = spectralon$L.norm,
              rho = rho,
              deltaTime = deltaTime))
}
