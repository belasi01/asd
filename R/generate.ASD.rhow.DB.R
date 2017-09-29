#' Generate a data base for rhow from the ASD (NOT YET AVAILABLE)
#'
#'
#' @param rhow.Method is an integer (0 to 5) indicating the best method for the specular sky
#' reflectance removal (see User Guide).
#' 0 is for Mobley rho_sky with no NIR correction.
#' 1 is for Mobley rho_sky with NIR correction based on black pixel assumption.
#' 2 is for Mobley rho_sky with NIR correction based on similarity spectrum using 720 and 780 nm.
#' 3 is for Mobley rho_sky with NIR correction based on similarity spectrum using 780 and 870 nm.
#' 4 is for rho_sky estimated using the black pixel assumption in the NIR.
#' 5 is for rho_sky estimated using the black pixel assumption in the UV.

generate.ASD.rhow.DB <- function(dirdat) {

}
