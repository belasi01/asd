#'@title Compute the water leaving reflectance from ASD data
#'
#'@description
#'This function compute the remote sensing reflectance for a water surface.
#'
#'
#' @param raw.asd is a long list returned by \code{\link{merge.ASD.radiances.for.rhow}} containing all
#'the data necessary to compute the diffuse water reflectance. That include the raw radiance data,
#'the sun-viewing geometry, the location, the time and the wind speed.
#' @param rho.panel is the reflectivity of the spectralon panel. The Default is 0.985
#' @param quantile.prob is a value (between 0.25 to 1) for the maximum quantile probability
#'for which the surface radiance (Lt) values will be discarded.
#'The quantile probability is the value at which the probability
#'of the random variable being less than or equal to this value.
#'For example, a value of 0.5 means that every Lt value above the
#'50\% quantile probability will be discarded, while a value of 0.8 means
#'that only the values above the 80\% quantile probability will be discarded.
#'The rational is to eliminate outliers resulting from sun glitters, foam, sea ice, etc.
#'The default value is 0.5 to eliminate Lt value above the 50\% quantile probability.
#' @param VERBOSE is a bolean to output the processing steps in the terminal (Default is TRUE).

#'
#'@details See User's Guide (in french) for details
#'
#'
#'@author Simon Bélanger

compute.ASD.rhow <- function(raw.asd,
                            rho.panel = 0.985,
                            quantile.prob = 0.5,
                            VERBOSE=TRUE) {

  Ltot <-raw.asd$Ltot
  Lpanel <-raw.asd$Lpanel
  Lsky <- raw.asd$Lsky
  anc <- raw.asd$anc

  # Trim data to remove high Ltot spectra which may be affected by sun glint
  ix325 = which.min(abs(Ltot$waves - 325))
  ix490 = which.min(abs(Ltot$waves - 490))
  ix720 = which.min(abs(Ltot$waves - 720))
  ix750 = which.min(abs(Ltot$waves - 750))
  ix780 = which.min(abs(Ltot$waves - 780))
  ix870 = which.min(abs(Ltot$waves - 870))
  ix900 = which.min(abs(Ltot$waves - 900))


  if (VERBOSE) print("Averaging Radiance spcetra...")
  ix.Lt.good = which(Ltot$Lum[ix490,] < quantile(Ltot$Lum[ix490,], probs = quantile.prob) &
                    Ltot$Lum[ix490,] > quantile(Ltot$Lum[ix490,], probs = 0.15))

  # Interquantile mean for Lsky and Lpanel
  ix.Lsky.good = which(Lsky$Lum[ix490,] < quantile(Lsky$Lum[ix490,], probs = 0.75) &
                         Lsky$Lum[ix490,] > quantile(Lsky$Lum[ix490,], probs = 0.25))

  ix.Lpanel.good = which(Lpanel$Lum[ix490,] < quantile(Lpanel$Lum[ix490,], probs = 0.75) &
                           Lpanel$Lum[ix490,] > quantile(Lpanel$Lum[ix490,], probs = 0.25))

  Lt    = Ltot$Lum[,ix.Lt.good]
  Li  = Lsky$Lum[,ix.Lsky.good]
  Lpanel= Lpanel$Lum[,ix.Lpanel.good]

  Lt.time.mean = mean.POSIXct(Ltot$DateTime[ix.Lt.good])


  ######### Average data
  Ed.mean = pi*apply(Lpanel, 1, mean)/rho.panel # See eq 5 i Mobley (1999)
  Li.mean = apply(Li, 1, mean)
  Lt.mean = apply(Lt, 1, mean)

  Ed.sd = pi*apply(Lpanel, 1, sd)/rho.panel
  Li.sd = apply(Li, 1, sd)
  Lt.sd = apply(Lt, 1, sd)

  ######## Compute Sky reflectance and rho and apply a smoothing function
  if (VERBOSE) print("Compute Sky and Surface reflectance and apply a smoothing function")
  x = range(Lsky$waves)

  sky = Li.mean/Ed.mean
  mod= loess(sky~waves, data=data.frame(waves=Lsky$waves, sky=sky), span=20/(x[2]-x[1])) # 20 nm window
  sky.smooth = predict(mod, Lsky$waves)

  sea =Lt.mean/Ed.mean
  mod= loess(sea~waves, data=data.frame(waves=Ltot$waves, sea=sea), span=10/(x[2]-x[1])) # 10 nm window
  sea.smooth = predict(mod, Ltot$waves)

  ######### Get rho fresnel from MOBLEY LUT or use a constant rho of 0.0256 if cloudy
  if (sky.smooth[ix750] >= 0.05){
    #Then  CLOUDY SKY (Ruddick et al L&O 2006, eq. 23, 24)
    if (VERBOSE) print("Cloudy sky, Use rho = 0.0256")
    rho = 0.0256
    CLEARSKY = FALSE

  }  else {
    if (VERBOSE) print("Interpolate Mobley LUT for Fresnel reflectance")
      CLEARSKY = TRUE
      rho = get.rho550(anc$ThetaV, anc$Dphi, anc$Windspeed,anc$ThetaS)
  }


  ################### remove the reflected sky
  if (VERBOSE) print("Apply NIR corrections")
  Rrs = sea.smooth - (rho*sky.smooth)

  ###### Estimation of the rho.fresnel assuming BLACK Pixel assumption in the NIR
  rho.sky.BP =  sea.smooth[ix900] / sky.smooth[ix900]
  Rrs.BP = sea.smooth - (rho.sky.BP*sky.smooth)

  ###### Estimation of the rho.fresnel assuming BLACK Pixel assumption in the UV
  rho.sky.UV =  sea.smooth[ix325] / sky.smooth[ix325]
  Rrs.UV = sea.smooth - (rho.sky.UV*sky.smooth)

  # Apply a correction
  #   Estimation of the NIR Rrs offset correction based on Ruddick et al L&O 2006, SPIE 2005
  offset = 2.35*Rrs[ix780] - Rrs[ix720]/(2.35-1)
  Rrs.SIMILARITY1 = Rrs - offset # Apply NIR correction
  offset = 1.91*Rrs[ix870] - Rrs[ix780]/(1.91-1)
  Rrs.SIMILARITY2 = Rrs - offset # Apply NIR correction
  # A standard NULL correction
  offset = Rrs[ix900]
  Rrs.NULL = Rrs - offset # Apply NIR correction


  list.rho = list(
    waves = Ltot$waves,
    rhow = Rrs*pi,
    rhow.NULL = Rrs.NULL*pi,
    rhow.SIMILARITY1 = Rrs.SIMILARITY1*pi,
    rhow.SIMILARITY2 = Rrs.SIMILARITY2*pi,
    rhow.NIR = Rrs.BP * pi,
    rhow.UV = Rrs.UV * pi,
    rho.sky = rho,
    rho.sky.NIR = rho.sky.BP,
    rho.sky.UV = rho.sky.UV,
    Lpanel=Lpanel,
    Lt=Lt,
    Li=Li,
    Ed.mean = Ed.mean,
    Ed.sd = Ed.sd,
    Lt.mean = Lt.mean,
    Lt.sd = Lt.sd,
    Li.mean = Li.mean,
    Li.sd = Li.sd,
    ix.Lt.good=ix.Lt.good,
    ix.Lsky.good=ix.Lsky.good,
    ix.Lpanel.good=ix.Lpanel.good,
    DateTime = Lt.time.mean,
    anc=anc,
    CLEARSKY = CLEARSKY)
  if (VERBOSE) str(list.rho)
  return(list.rho)

}
