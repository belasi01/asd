#'@title Retreive sea-surface reflectance from a look-up-tables
#'
#'@description
#'Interpolate within a 4D look-up-table (LUT) of the sea-surface reflectance
#'computed by Curt Mobley (Mobley, 1999, 2015).
#'
#'@param thetaV is the viewing zenith angle
#'@param delta.phi is the sensor azimuth angle relative to the sun
#'@param windspeed is the wind speed in m/s.
#'@param thetaS is the sun zenith angle
#'
#'@details
#'The function performs a linear interpolation in a 4D LUT.
#'It uses the \code{\link{approx3d}} function from the oce package.
#'The current LUT is decribed in details in Mobley, App. Opt (1999), which was updated by
#'Mobley, App. Opt (2015) to include improve sea surface wave statistic and polarisation
#'
#'@examples
#' get.rho550(thetaV=35, delta.phi=120, windspeed=5.5, thetaS=55)
#'
#'@author Simon Bélanger

get.rho550 <- function (thetaV, delta.phi, windspeed,thetaS){

  xthetaV = as.numeric(dimnames(rho550)$thetaV)
  xdelta.phi = as.numeric(dimnames(rho550)$phiV)
  xwindspeed = as.numeric(dimnames(rho550)$windspeed)
  xthetaS = as.numeric(dimnames(rho550)$thetaS)

  if (windspeed >= 15) windspeed = 14.99

  # NOTE : The approx3D function require equally spaced x, y, z
  # I therefore replace the 87.5 by 90
  xthetaS[xthetaS == 87.5] = 90
  xthetaV[xthetaV == 87.5] = 90


  for (i in 1:(length(xwindspeed)-1)) {
    if (windspeed > xwindspeed[i] & windspeed < xwindspeed[i+1]) {
      rho.min = oce::approx3d(xthetaV, xdelta.phi,xthetaS, rho550[,,i,], thetaV, delta.phi,thetaS)
      rho.max = oce::approx3d(xthetaV, xdelta.phi,xthetaS, rho550[,,i+1,], thetaV,delta.phi,thetaS)
      f = (windspeed - xwindspeed[i]) / (xwindspeed[i+1]-xwindspeed[i])
      rho = rho.min * (1-f) + rho.max * f
      return(rho)
    }
    if (windspeed == xwindspeed[i]){
      rho = oce::approx3d(xthetaV, xdelta.phi,xthetaS, rho550[,,i,], thetaV,delta.phi,thetaS)
      return(rho)
    }
  }

}
