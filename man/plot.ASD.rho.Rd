% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.ASD.rho.R
\name{plot.ASD.rho}
\alias{plot.ASD.rho}
\title{Figure de la reflectance spectrale de l'ASD}
\usage{
plot.ASD.rho(asd, type.surf = "Surface", PNG = FALSE)
}
\description{
Produit une figure pour la mesure de réflectance ASD. La figure comprend deux paneaux superposés.
Celui du haut montre les spectres de Luminance normalisée par le temps d'intégration.
Celui du bas est la réflectance calculée.
}
\details{
@param asd est une liste produite par la fonction \code{\link{compute.ASD.rho}}
@param type.surf est une chaine de caractère qui apparaitra dans la légende de la figure.
Par défaut type.surf="Surface'
@param PNG est une variable booléenne (TRUE ou FALSE) permet de produire un fichier png.
Par défaut PNG=FALSE

@author Simon Bélanger
}
