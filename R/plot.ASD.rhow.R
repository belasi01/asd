#'  Produit des figures pour la réflectance marine
#'
#'  @param asd est une liste produite par la fonction \code{\link{compute.ASD.rhow}}
#'  @param PNG est une variable booléenne (TRUE ou FALSE) qui permet de produire un fichier png.
#'  Par défaut PNG=FALSE
#'  @param RADIANCES est une variable booléenne (TRUE ou FALSE) qui permet de produire une figure
#'  avec les mesures de luminances et la réflectance de la surface et du ciel. Par défaut RADIANCES=FALSE
#'
#'  @author Simon Bélanger

plot.ASD.rhow <- function (asd, PNG=FALSE, RADIANCES=FALSE) {


  ix.wl = which(asd$waves > 350 & asd$waves <900)

  if (PNG & !dir.exists("PNG")) dir.create("PNG")

  if (RADIANCES) {
    if (PNG) png(paste("PNG/",asd$anc$StationID,"_Radiances.png",sep=""), units = "in",
                 width = 5, height = 4, res = 300)

    # first plot of the raw radiances with error bars

    Df = as.data.frame(cbind(wavelength=asd$waves[ix.wl],
                             Li=asd$Li.mean[ix.wl], Li.sd=asd$Li.sd[ix.wl],
                             Lt=asd$Lt.mean[ix.wl], Lt.sd=asd$Lt.sd[ix.wl],
                             Ed=asd$Ed.mean[ix.wl], Ed.sd=asd$Ed.sd[ix.wl],
                             rhosky=pi*asd$rho.sky*(asd$Li.mean[ix.wl]/asd$Ed.mean[ix.wl]),
                             rhosurf=pi*asd$Lt.mean[ix.wl]/asd$Ed.mean[ix.wl]))

    p1=ggplot(data=Df, aes(x=wavelength, y=Ed))  + geom_line()  + scale_x_continuous(limits = c(350, 950))  + geom_ribbon(aes(ymin=Ed-Ed.sd, ymax=Ed+Ed.sd, x=wavelength), alpha = 0.5)
    p2=ggplot(data=Df, aes(x=wavelength, y=Li))  + geom_line()  + scale_x_continuous(limits = c(350, 950))+ geom_ribbon(aes(ymin=Li-Li.sd, ymax=Li+Li.sd, x=wavelength), alpha = 0.5)
    p3=ggplot(data=Df, aes(x=wavelength, y=Lt))  + geom_line()  + scale_x_continuous(limits = c(350, 950))+ geom_ribbon(aes(ymin=Lt-Lt.sd, ymax=Lt+Lt.sd, x=wavelength), alpha = 0.5)
    p4=ggplot(data=Df, aes(x=wavelength, y=rhosky))  + geom_line()
    p4 = p4  + geom_line(aes(x=wavelength, y=rhosurf), linetype=2) + labs(x=expression(lambda),
                                                              y=expression(paste(rho[sky],rho[surf])))


    pushViewport(viewport(layout = grid.layout(4, 1)))
    print(p1, vp = viewport(layout.pos.row = 1))
    print(p2, vp = viewport(layout.pos.row = 2))
    print(p3, vp = viewport(layout.pos.row = 3))
    print(p4, vp = viewport(layout.pos.row = 4))

    if (PNG) dev.off()

  } else {
    if (PNG) png(paste("PNG/",asd$anc$StationID,"_rhow.png",sep=""), units = "in",
                 width = 5, height = 4, res = 300)

    Df = as.data.frame(cbind(wavelength=asd$waves[ix.wl],
                             None=asd$rhow[ix.wl],
                             Null_900=asd$rhow.NULL[ix.wl],
                             Similarity_720_780=asd$rhow.SIMILARITY1[ix.wl],
                             Similarity_780_870=asd$rhow.SIMILARITY2[ix.wl],
                             NIR = asd$rhow.NIR[ix.wl],
                             UV = asd$rhow.UV[ix.wl]))
    Dfm = melt(Df, id.vars = c("wavelength"))
    names(Dfm) = c("wavelength", "rho_w", "value" )
    p1 <- ggplot(data=Dfm, aes(x=wavelength, y=value, colour=rho_w)) + geom_line()
    p1 <- p1 + scale_x_continuous(limits = c(350, 950))
    p1 <- p1 + labs(x=expression(lambda), y=expression(paste(rho[w])), colour="Correction method")
    p1 <- p1 + ggtitle(paste(asd$anc$StationID, asd$DateTime, "Lat:", asd$anc$lat, "Lon:", asd$anc$lon),
                        subtitle = bquote(rho[Fresnel]^Mobley2015 == .(asd$rho.sky) ~
                                            "   "~ rho[Fresnel]^NIR == .(asd$rho.sky.NIR)~
                                            "   "~ rho[Fresnel]^UV == .(asd$rho.sky.UV)))
  print(p1)
    if (PNG) dev.off()
  }



}
