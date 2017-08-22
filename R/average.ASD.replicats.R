#'  Calcule la moyenne et l'écart-type des réplicats mesurés avec l'ASD
#'  
#'
#' @param file.list est la liste des noms de fichier ASD. 
#'
#' @return Elle retourne une liste avec les variables suivantes.
#' L.norm= Luminance normalisée par le temps d'intégration (moyenne). 
#' L.norm.sd= Luminance normalisée par le temps d'intégration (ecart-type). 
#' waves= Les longueur d'onde. The wavelenght vector;
#' DateTime= La date et l'heure de l'acquisition en format POSIXct.
#' IntTime= Le temps d'intégration en millisecondes. 
#'
#' @author Simon Bélanger
#'
#'
average.ASD.replicats <- function(file.list){
  nb.file = length(file.list)
  
  # Read the first file 
  asd = read.ASD(file.list[1])
  nb.waves = length(asd$waves)
  
  Lum = matrix(ncol=nb.file, nrow=nb.waves)
  IntTime_mat = rep(NA,nb.file)
  DateTime_mat =  rep(NA,nb.file)
  
  for (i in 1:nb.file) {
    print(paste("Reading file: ", file.list[i]))
    asd = read.ASD(file.list[i])
    Lum[,i] = asd$L.norm
    IntTime_mat[i] = asd$IntTime
    DateTime_mat[i] = asd$DateTime
  }
  
  # Calcule la moyenne et l'écartype
  L.norm= apply(Lum, 1, mean, na.rm=T)
  L.norm.sd = apply(Lum, 1, sd, na.rm=T)
  DateTime = mean.POSIXct(DateTime_mat, na.rm=T)
  IntTime = mean(IntTime_mat, na.rm=T)
  
  return(list(L.norm = L.norm,
              L.norm.sd = L.norm.sd,
              waves = asd$waves, 
              DateTime=DateTime, 
              IntTime = IntTime))
  
  
}
  