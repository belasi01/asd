#' Merge ASD radiances files from the surface, the sky and the spectralon
#' panel taken to determine the diffuse above-water reflectance
#'
#' @param Ltot is a list returned by the function \code{\link{average.ASD.replicats}}
#' containing the radiances of the surface (Ltot)
#' @param Lsky is a list returned by the function \code{\link{average.ASD.replicats}}
#' containing the radiances of the sky (Lsky)
#' @param Lpanel is a list returned by the function \code{\link{average.ASD.replicats}}
#' containing the radiances of the spectralon (Lpanel)
#' @param StationID is the station ID
#'@param lat is the latitude in degree of the measurements
#'@param lon is the longitude in degree of the measurements
#'@param DateTime is the averaged time of the measurements in POSIXct format
#'@param ThetaV is the viewing zenith angle
#'@param Dphi is the sensor azimuth angle relative to the sun
#'@param Windspeed is the wind speed in m/s.
#'
#'
#' @return Returns a list containing all the radiance data of the three
#' type of measurements required for the Rrs calculation of the water
#' diffuse reflectance. In addition, ancillary data list needed to compute the Rrs
#' is also provided (lat,lon, DateTime, ThetaS, ThetaV and DeltaPhi)
#'
#' @author Simon Bélanger


merge.ASD.radiances.for.rhow <- function(Ltot,
                                              Lsky,
                                              Lpanel,
                                              StationID="StationX",
                                              lat,
                                              lon,
                                              DateTime,
                                              ThetaV,
                                              Dphi,
                                              Windspeed) {

  # calculate the sun zenith angle from time and position.
  # Derive data from above information
  # sun-zenith angle
  day <- as.numeric(format(DateTime, format = "%d"))
  month <- as.numeric(format(DateTime, format = "%m"))
  year <- as.numeric(format(DateTime, format = "%Y"))
  hour <- as.numeric(format(DateTime, format = "%H"))
  minute <- as.numeric(format(DateTime, format = "%M"))
  second <- as.numeric(format(DateTime, format = "%S"))
  ah <- hour + minute / 60 + second / 3600

  sunpos <- possol(month, day, ah, lon, lat)
  ThetaS <- sunpos[1]
  PhiS <-sunpos[2]
  anc <- list(StationID=as.character(StationID),
              lat=lat,
              lon=lon,
              DateTime=DateTime,
              ThetaV=ThetaV,
              Dphi=Dphi,
              Windspeed=Windspeed,
              ThetaS= ThetaS,
              PhiS=PhiS)
  ASD = list(Ltot=Ltot,
             Lsky=Lsky,
             Lpanel=Lpanel,
             anc=anc)
  return(ASD)

}
