#'
#'  Plot ASD reflectance
#'
#'  Plot the ASD reflectance return by the function compute.ASD.rho
#'
#'  @param asd is a list return by compute.ASD.rho

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
       xlab = "Wavelength",
       ylab = expression(rho),
       type="l", lwd=3)

  if (PNG) dev.off()
}
