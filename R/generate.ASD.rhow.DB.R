#' Generate a data base for rhow from the ASD
#'
#'
#' @param path is the path where the file directories.for.ASD.dat containing the data folders
#' to merge in the data base.
#' @param mission is a string for the name of the mission. It will be used for the file names of the output.
#' @param wave.range is a vector of two integers indicating the wavelength range to output in the database.
#' The default is wave.range=c(350,900) to put the data from 350 nm to 900 nm.
#'
#' @return It returns a list object named ASD.DB containing a matrix of rhow (rhow.m) and vectors for
#' StationID, data, lat, lon sunzen, windspeed, rhow.Method.
#'
#' The object ASD.DB is saved in RData format. The data are also saved in ASCII (.dat with comma separator)
#' and a figure showing the measured rho_w spectra of the data base is produced.

generate.ASD.rhow.DB <- function(path="./",mission="XXX", wave.range=c(350,900)) {

    setwd(path)
    path<- getwd()
    dirs <- scan(file = "directories.for.ASD.dat", "", sep = "\n", comment.char = "#")


    ndirs = length(dirs)
    waves = wave.range[1]:wave.range[2]
    nwaves = length(waves)

    #### assess the number of cast found in each data folders.
    ncasts = 0
    for (i in 1:ndirs) {
      setwd(as.character(dirs[i]))
      #cast.info.file <- paste(path, "cast.info.dat", sep = "/")
      cast.info <- read.table(file="cast.info.dat", header=T, comment.char = "#")
      experiment <- nrow(cast.info)
      ncasts = ncasts + experiment
    }

    setwd(path)
    print(paste("The number of casts found is : ", ncasts))

    rhow.m = matrix(ncol=nwaves, nrow = ncasts)
    ID = rep("ID", ncasts)
    date = rep(NA, ncasts)
    sunzen = rep(NA, ncasts)
    lat = rep(NA, ncasts)
    lon = rep(NA, ncasts)
    windspeed = rep(NA, ncasts)
    rhow.Method = rep(NA, ncasts)
    cast=1
    for (i in 1:ndirs) {
      setwd(as.character(dirs[i]))
      cast.info <- read.table(file="cast.info.dat", header=T, comment.char = "#")
      nexperiments <- nrow(cast.info)
      for (j in 1:nexperiments) {
        file.name = paste("./RData/", cast.info$ID[j], ".ASD.rhow.RData", sep="")
        load(file.name)

        # find the indices corresponding to the wavelenght range to output
        ix.min <- which(rhow$waves == wave.range[1])
        ix.max <- which(rhow$waves == wave.range[2])

        rhow.Method[cast] <- cast.info$rhow.Method[j] # retrieve the method from the cast.info file

        if (rhow.Method[cast] == 0) rhow.m[cast,] <- rhow$rhow[ix.min:ix.max]
        if (rhow.Method[cast] == 1) rhow.m[cast,] <- rhow$rhow.NULL[ix.min:ix.max]
        if (rhow.Method[cast] == 2) rhow.m[cast,] <- rhow$rhow.SIMILARITY1[ix.min:ix.max]
        if (rhow.Method[cast] == 3) rhow.m[cast,] <- rhow$rhow.SIMILARITY2[ix.min:ix.max]
        if (rhow.Method[cast] == 4) rhow.m[cast,] <- rhow$rhow.NIR[ix.min:ix.max]
        if (rhow.Method[cast] == 5) rhow.m[cast,] <- rhow$rhow.UV[ix.min:ix.max]
        if (rhow.Method[cast] == 6) rhow.m[cast,] <- rhow$rhow.UV.NIR[ix.min:ix.max]
        if (rhow.Method[cast] == 7) rhow.m[cast,] <- rhow$rhow.COPS[ix.min:ix.max]
        if (rhow.Method[cast] == 8) rhow.m[cast,] <- rhow$rhow.Kutser[ix.min:ix.max]
        if (rhow.Method[cast] == 999) rhow.m[cast,] <- rep(NA,nwaves)

        ID[cast] <- as.character(cast.info$ID[j])
        date[cast] <- rhow$DateTime
        sunzen[cast] <- rhow$anc$ThetaS
        lat[cast] <- rhow$anc$lat
        lon[cast] <- rhow$anc$lon
        windspeed[cast] <- rhow$anc$Windspeed


        rec.info = data.frame(ID[cast],
                              date[cast],
                              lat[cast],
                              lon[cast],
                              sunzen[cast],
                              windspeed[cast],
                              rhow.Method[cast])


        if (cast == 1) {
          all = data.frame(rec.info,t(rhow.m[cast,]))

          col.names = c(paste("rhow_", waves,sep=""))

          names(all) <- c("StationID","DateTime",  "latitude", "longitude", "sunzen", "WindSpeed", "rhow.Method", col.names)
        } else
        {
          rec = data.frame(rec.info,t(rhow.m[cast,]))
          names(rec) <-  c("StationID","DateTime",  "latitude", "longitude", "sunzen", "WindSpeed", "rhow.Method", col.names)
          all = rbind(all,rec)
        }


        cast = cast + 1




      }

    }

    ASD.BD <- list(ID=ID,
                   rhow.m=rhow.m,
                   date=as.POSIXct(date, origin="1970-01-01"),
                   lat=lat,
                   lon=lon,
                   sunzen=sunzen,
                   windspeed=windspeed,
                   rhow.Method=rhow.Method)

    # Save the data
    setwd(path)
    save(ASD.BD, file=paste("ASD.DB.PackageVersion.",packageVersion("asd"),".", mission,".RDATA",sep=""))
    all$DateTime=as.POSIXct(all$DateTime, origin="1970-01-01")
    write.table(all, file = paste("ASD.DB.PackageVersion.",packageVersion("asd"),".", mission,".dat",sep=""), sep=",", quote=F, row.names=F)



    # plot the data

    png(paste("ASD.DB.", mission,".png",sep=""), res=300, height = 6, width = 8, units = "in")

    Df = as.data.frame(cbind(wavelength=waves, t(rhow.m)))
    colnames(Df) <- c("wavelength", ID)
    Dfm = reshape2::melt(Df, id.vars = c("wavelength"))
    names(Dfm) = c("wavelength", "rho_w", "value" )

    p1 <- ggplot(data=Dfm, aes(x=wavelength, y=value, colour=rho_w)) + geom_line()
    p1 <- p1 + labs(x=expression(lambda), y=expression(paste(rho[w])), colour="Station")
    p1 <- p1 + ggtitle(paste(mission))
    print(p1)

    dev.off()

    return(ASD.BD)


}
