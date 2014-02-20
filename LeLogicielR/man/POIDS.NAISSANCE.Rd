\encoding{latin1}
\name{POIDS.NAISSANCE}
\alias{POIDS.NAISSANCE}
\docType{data}
\title{Poids de naissance}
\description{
Ce jeu de données provient d'une étude épidémiologique
traitée par une équipe de l'Institut de santé publique
d'épidémiologie et de développement (ISPED) de Bordeaux. Il
s'agit d'une enquête concernant les facteurs de risque associés au faible
poids de naissance de nourrissons (données collectées au centre médical de Baystate
dans le Massachusetts pendant l'année 1986).
}
\usage{data(POIDS.NAISSANCE)}
\format{
  Un data frame avec 189 observations mesurées sur les 11 variables suivantes.
  \describe{
    \item{\code{ID}}{un vecteur numérique}
    \item{\code{AGE}}{un vecteur numérique}
    \item{\code{LWT}}{un vecteur numérique}
    \item{\code{RACE}}{un vecteur numérique}
    \item{\code{SMOKE}}{un vecteur numérique}
    \item{\code{PTL}}{un vecteur numérique}
    \item{\code{HT}}{un vecteur numérique}
    \item{\code{UI}}{un vecteur numérique}
    \item{\code{FVT}}{un vecteur numérique}
    \item{\code{BWT}}{un vecteur numérique}
    \item{\code{LOW}}{un vecteur numérique}
  }
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(POIDS.NAISSANCE)
str(POIDS.NAISSANCE) 
}
\keyword{datasets}
