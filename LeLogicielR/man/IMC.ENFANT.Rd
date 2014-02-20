\encoding{latin1}
\name{IMC.ENFANT}
\alias{IMC.ENFANT}
\docType{data}
\title{Indice de masse corporelle chez les enfants}
\description{
Ce jeu de données provient d'une étude épidémiologique
traitée par une équipe de l'Institut de santé publique d'épidémiologie et de développement (ISPED) de Bordeaux.
Un échantillon de dossiers d'enfants a été saisi. Ce sont des enfants vus lors
d'une visite en 1ère section de maternelle en 1996-1997 dans des écoles de Bordeaux
(Gironde, France). L'échantillon est constitué de 152 enfants âgés de 3
ou 4 ans.
}
\usage{data(IMC.ENFANT)}
\format{
  Un data frame avec 152 observations mesurées sur les 6 variables suivantes.
  \describe{
    \item{\code{SEXE}}{un facteur ayant les niveaux \code{F} et \code{G}}
    \item{\code{zep}}{un facteur ayant les niveaux \code{N} et \code{O}}
    \item{\code{poids}}{un vecteur numérique}
    \item{\code{an}}{un vecteur numérique}
    \item{\code{mois}}{un vecteur numérique}
    \item{\code{taille}}{un vecteur numérique}
  }
}

\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(IMC.ENFANT)
str(IMC.ENFANT) 
}
\keyword{datasets}
