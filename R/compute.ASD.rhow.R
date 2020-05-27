#'@title Compute the water leaving reflectance from ASD data
#'
#'@description
#'This function compute the water reflectance (rho_w = pi * Rrs) for a water surface.
#'The sky glint is removed using various methods as detailed in \code{\link{ASD.go}.
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
#' @param COPS is logical parameter to force the water reflectance to pass through the COPS
#' reflectance measurements made a priori. It must be turn on only if COPS data have been
#' processed and validated
#'
#'@details See User's Guide (in french) for details
#'
#'
#'@author Simon Bélanger

compute.ASD.rhow <- function(raw.asd,
                            rho.panel = 0.985,
                            quantile.prob = 0.5,
                            COPS = FALSE,
                            VERBOSE=TRUE) {

  asd.path <- getwd()
  Ltot <-raw.asd$Ltot
  Lpanel <-raw.asd$Lpanel
  Lsky <- raw.asd$Lsky
  anc <- raw.asd$anc

  # Trim data to remove high Ltot spectra which may be affected by sun glint
  ix350 = which.min(abs(Ltot$waves - 350))

  # use this to avoid negativa values of (ix350-5),May 22,2020, Yanqun
  if (ix350<6) ix350 = 6

  ix490 = which.min(abs(Ltot$waves - 490))
  ix720 = which.min(abs(Ltot$waves - 720))
  ix750 = which.min(abs(Ltot$waves - 750))
  ix780 = which.min(abs(Ltot$waves - 780))
  ix870 = which.min(abs(Ltot$waves - 870))
  ix900 = which.min(abs(Ltot$waves - 900))


  if (VERBOSE) print("Averaging Radiance spcetra...")
  ix.Lt.good = which(Ltot$Lum[ix490,] < quantile(Ltot$Lum[ix490,], probs = quantile.prob) &
                    Ltot$Lum[ix490,] > quantile(Ltot$Lum[ix490,], probs = 0.10))

  # Interquantile mean for Lsky and Lpanel
  ix.Lsky.good = which(Lsky$Lum[ix490,] < quantile(Lsky$Lum[ix490,], probs = 0.90) &
                         Lsky$Lum[ix490,] > quantile(Lsky$Lum[ix490,], probs = 0.10))

  ix.Lpanel.good = which(Lpanel$Lum[ix490,] < quantile(Lpanel$Lum[ix490,], probs = 0.90) &
                           Lpanel$Lum[ix490,] > quantile(Lpanel$Lum[ix490,], probs = 0.10))

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

  # Apply a correction

  ###### Method 1.  A standard NULL correction
  offset = Rrs[ix900]
  Rrs.NULL = Rrs - offset # Apply NIR correction

  ###### Methods 2 and 3. Estimation of the NIR Rrs offset correction based on
  #      Ruddick et al L&O 2006, SPIE 2005
  offset = 2.35*Rrs[ix780] - Rrs[ix720]/(2.35-1)
  Rrs.SIMILARITY1 = Rrs - offset # Apply NIR correction
  offset = 1.91*Rrs[ix870] - Rrs[ix780]/(1.91-1)
  Rrs.SIMILARITY2 = Rrs - offset # Apply NIR correction

  ###### Method 4. Estimation of the rho.fresnel assuming BLACK Pixel assumption in the NIR
  rho.sky.NIR =  (mean(sea.smooth[(ix900-5):(ix900+5)], na.rm = T) /
                       mean(sky.smooth[(ix900-5):(ix900+5)], na.rm = T))
  Rrs.BP = sea.smooth - (rho.sky.NIR*sky.smooth)

  ###### Method 5. Estimation of the rho.fresnel assuming BLACK Pixel assumption in the UV
  rho.sky.UV =  (mean(sea.smooth[(ix350-5):(ix350+5)], na.rm = T) /
                  mean(sky.smooth[(ix350-5):(ix350+5)], na.rm = T))
  Rrs.UV = sea.smooth - (rho.sky.UV*sky.smooth)

  ###### Method 6. Estimation of the rho.fresnel assuming BLACK Pixel assumption in both UV and NIR (spectrally dependent)
  rho.sky.UV.NIR = spline(c(350, 900), c(rho.sky.UV, rho.sky.NIR),
                          xout = Ltot$waves)$y
  Rrs.UV.NIR = sea.smooth - (rho.sky.UV.NIR*sky.smooth)

  ###### Method 7. (OPTIONAL). Only if COPS is available
  # this method forces the ASD-derived Rrs to pass through
  # the cops Rrs at two wavelenghts (second shortest and longest respectively)
  Rrs.COPS = NA
  rho.sky.COPS = NA
  if (COPS) {

    # Finding the COPS files avaiblable
    # Check for COPS folder in the parent directory
    ld = list.dirs("..", recursive = F)
    ix.d = grep("COPS", ld)
    if (length(ix.d) >= 1) {
      if (length(ix.d) > 1) {
        print("More than one COPS folder found")
        cops.path = ld[ix.d[1]]
      } else {
        cops.path = ld[ix.d]
      }

      setwd(cops.path)

      remove.file <- "remove.cops.dat"
      select.file <- "select.cops.dat"

      select.file.exists <- FALSE

      if (file.exists(remove.file)) {
        remove.tab <- read.table(remove.file, header = FALSE, colClasses = "character", sep = ";")
        kept.cast <- remove.tab[[2]] == "1"
      }
      if (file.exists(select.file)) {
        select.file.exists <- TRUE
        remove.tab <- read.table(select.file, header = FALSE, colClasses = "character", sep = ";")
        kept.cast <- remove.tab[[2]] == "1"
        Rrs_method <- remove.tab[kept.cast, 3]
      }
      listfile  <- remove.tab[kept.cast, 1]


      setwd("./BIN/")

      nf = length(listfile)
      print(listfile)

      if (nf > 1) {

        mRrs = matrix(ncol=19, nrow = nf)

        for (j in 1:nf) {

          load(paste(listfile[j], ".RData", sep=""))
          waves = cops$LuZ.waves

          # extract Rrs
          if (select.file.exists) {
            #mRrs[j,] = eval(parse(text=paste0("cops$",Rrs_method[j],"[xw]")))
            mRrs[j,] = eval(parse(text=paste0("cops$",Rrs_method[j])))

          } else {
            mRrs[j,] = cops$Rrs.0p.linear
          }


        }

        cops.Rrs.m = apply(mRrs, 2, mean)
      } else {
        load(paste(listfile, ".RData", sep=""))
        waves = cops$LuZ.waves
        if (select.file.exists) {
          #cops.Rrs.m = eval(parse(text=paste0("cops$",Rrs_method,"[xw]")))
          cops.Rrs.m = eval(parse(text=paste0("cops$",Rrs_method)))

        } else {
          cops.Rrs.m = cops$Rrs.0p.linear
        }
      }

      # Find the shortest and longest valid wavelength for the Rrs
      ix.good.Rrs = which(cops.Rrs.m > 0)
      ix.waves.min.cops = min(ix.good.Rrs)
      ix.waves.max.cops = max(ix.good.Rrs)
      waves.max = waves[ix.waves.max.cops]
      waves.min = waves[ix.waves.min.cops]


      ix.waves.min.asd = which.min(abs(Ltot$waves - waves.min))
      if (ix.waves.min.asd<6) ix.waves.min.asd=6

      ix.waves.max.asd = which.min(abs(Ltot$waves - waves.max))

      # Estimate rho.shy at the two wavelenghts selected
      rho.sky.min  = ((mean(sea.smooth[(ix.waves.min.asd-5):(ix.waves.min.asd+5)],na.rm = T) - cops.Rrs.m[ix.waves.min.cops])
                    / mean(sky.smooth[(ix.waves.min.asd-5):(ix.waves.min.asd+5)], na.rm = T))
      rho.sky.max  = ((mean(sea.smooth[(ix.waves.max.asd-5):(ix.waves.max.asd+5)],na.rm = T) - cops.Rrs.m[ix.waves.max.cops])
                      / mean(sky.smooth[(ix.waves.max.asd-5):(ix.waves.max.asd+5)], na.rm = T))

      rho.sky.COPS = spline(c(waves.min, waves.max), c(rho.sky.min, rho.sky.max),
                            xout = Ltot$waves)$y

      Rrs.COPS = sea.smooth - (rho.sky.COPS*sky.smooth)

      setwd(asd.path)

    } else {
      print("No COPS folder found!!!")
      print("Stop processing")
      return(0)
    }

  }


  list.rho = list(
    waves = Ltot$waves,
    rhow = Rrs*pi,
    rhow.NULL = Rrs.NULL*pi,
    rhow.SIMILARITY1 = Rrs.SIMILARITY1*pi,
    rhow.SIMILARITY2 = Rrs.SIMILARITY2*pi,
    rhow.NIR = Rrs.BP * pi,
    rhow.UV = Rrs.UV * pi,
    rhow.UV.NIR = Rrs.UV.NIR * pi,
    rhow.COPS = Rrs.COPS * pi,
    rho.sky = rho,
    rho.sky.NIR = rho.sky.NIR,
    rho.sky.UV = rho.sky.UV,
    rho.sky.UV.NIR = rho.sky.UV.NIR,
    rho.sky.COPS = rho.sky.COPS,
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
