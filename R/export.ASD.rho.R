#' Export spectral refelctance in an ASCII file
#'
#' @param asd est une liste retourné par la fonction \code{\link{compute.ASD.rho}}
#' @param file.out est le nom du fichier ASCII ou les données seront écrites
#' @author Simon Bélanger
#'


export.ASD.rho <- function (asd, file.out= "asd.rho.csv") {

  df <- data.frame(wavelenght=asd$waves, rho=asd$rho)

  write.table(df, file=file.out, quote = F, row.names = F, sep=";", dec=",")

}
