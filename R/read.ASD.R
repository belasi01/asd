#'  Lit les données ASCII de l'ASD produites par le logiciel ViewSpec Pro.
#'  Reads ASD ASCII files exported from ViewSpec Pro.
#'
#' @param file est le nom complet du fichier ASD. Is the ASD file name
#'
#' @return Elle retourne une liste avec les variables suivantes.
#' Returns a list with the following variables:
#' L.norm= Luminance normalisée par le temps d'intégration. Raw radiance normalized by the integration time;
#' waves= Les longueur d'onde. The wavelenght vector;
#' DateTime= La date et l'heure de l'acquisition en format POSIXct.
#' The date and time in POSIXtc format;
#' IntTime= Le temps d'intégration en millisecondes. The integration time in milliseconds
#'
#' @author Simon Bélanger
#'
#'
read.ASD <- function(file) {

print(file)
  # extract header information
  id = file(file, "r")
  line = strsplit(readLines(con=id, n =1), "\t") # Reads the first header line
  nrec = 0
  line=c("Start","Reading")
  while (strsplit(unlist(line), " ")[[1]][1] != "Wavelength"){
    line = strsplit(readLines(con=id, n =1), "\t") # reads the time and depth for each records
    nrec <- nrec+1
    #print(strsplit(unlist(line), " "))

    if (line != "character(0)") {
      if (strsplit(unlist(line), " ")[[1]][1] == "Integration") IntTime = as.numeric(strsplit(unlist(line), " ")[[1]][4])

      if (strsplit(unlist(line), " ")[[1]][1] == "VNIR" &
          strsplit(unlist(line), " ")[[1]][2] == "integration") IntTime = as.numeric(strsplit(unlist(line), " ")[[1]][5])

      if (strsplit(unlist(line), " ")[[1]][1] == "Spectrum" &
          strsplit(unlist(line), " ")[[1]][2] == "saved:") {
        Date = strsplit(unlist(line), " ")[[1]][3]
        Time = strsplit(unlist(line), " ")[[1]][5]
      }

    } else line=c("Skip", "line")

  }
  close(id)

  df = read.table(file, skip=nrec+1)

  L.norm = df$V2/IntTime
  waves = df$V1

  DateTime = as.POSIXct(paste(Date,Time), format="%m/%d/%Y %H:%M:%S")

  return(list(L.norm = L.norm, waves = waves, DateTime=DateTime, IntTime = IntTime))

}
