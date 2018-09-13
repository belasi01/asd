#'
#'  Figure de la reflectance spectrale de l'ASD
#'
#'  Produit une figure pour la mesure de réflectance ASD. La figure comprend deux paneaux superposés.
#'  Celui du haut montre les spectres de Luminance normalisée par le temps d'intégration.
#'  Celui du bas est la réflectance calculée.
#'
#'  @param asd est une liste produite par la fonction \code{\link{compute.ASD.rho}}
#'  @param type.surf est une chaine de caractère qui apparaitra dans la légende de la figure.
#'  Par défaut type.surf="Surface'
#'  @param PNG est une variable booléenne (TRUE ou FALSE) permet de produire un fichier png.
#'  Par défaut PNG=FALSE
#'
#'  @author Simon Bélanger

plot.ASD.rho <- function (asd, type.surf="Surface", PNG=FALSE) {

  if (PNG) png(paste(type.surf,".png",sep=""), units = "in",
               width = 5, height = 4, res = 300)

  # first plot of the raw radiances on the smae plot
  par(mfrow=c(2,1))
  par(mar=c(2.1,4.1,1.1,2.1))
  plot(asd$waves, asd$L.spectralon,
       xlab = "",
       ylab = "Luminance norm. (C.N)",
       type="l", lwd=3)
  lines(asd$waves, asd$L.surf, col="darkred", lwd=3)
  legend("topright", c("Spectralon", type.surf), col=c("black", "darkred"), lwd=c(3,3))

  # Second plot for the reflectance
  par(mar=c(4.1,4.1,0.2,2.1))
  plot(asd$waves, asd$rho,
       xlab = "Longueur d'onde (nm)",
       ylab = expression(rho),
       type="l", lwd=3)

  if (PNG) dev.off()
  par(mfrow=c(1,1))
  par(mar=c(4.1,4.1,2.1,2.1))
}
