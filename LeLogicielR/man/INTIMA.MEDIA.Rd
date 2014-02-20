\encoding{latin1}
\name{INTIMA.MEDIA}
\alias{INTIMA.MEDIA}
\docType{data}
\title{Epaisseur de l'intima-media}
\description{
Ce jeu de données provient d'une étude épidémiologique
traitée par une équipe de l'Institut de santé publique
d'épidémiologie et de développement (ISPED) de Bordeaux.
L'épaisseur de l'intima-média est un marqueur
reconnu d'athérosclérose. Elle a été mesurée par échographie sur un échantillon
de 110 sujets en 1999 dans les CHU de Bordeaux. Des informations sur les
principaux facteurs de risque ont aussi été recueillies.
}
\usage{data(INTIMA.MEDIA)}
\format{
  Un data frame avec 110 observations mesurées sur les 9 variables suivantes.
  \describe{
    \item{\code{SEXE}}{un vecteur numérique}
    \item{\code{AGE}}{un vecteur numérique}
    \item{\code{taille}}{un vecteur numérique}
    \item{\code{poids}}{un vecteur numérique}
    \item{\code{tabac}}{un vecteur numérique}
    \item{\code{paqan}}{un vecteur numérique}
    \item{\code{SPORT}}{un vecteur numérique}
    \item{\code{mesure}}{un vecteur numérique}
    \item{\code{alcool}}{un vecteur numérique}
  }
}

\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(INTIMA.MEDIA)
str(INTIMA.MEDIA) 
}
\keyword{datasets}
