\encoding{latin1}
\name{NUTRIAGE}
\alias{NUTRIAGE}
\docType{data}
\title{Alimentation chez des personnes agees}
\description{
Ce jeu de données provient d'une étude épidémiologique
traitée par une équipe de l'Institut de santé publique
d'épidémiologie et de développement (ISPED) de Bordeaux.
Un échantillon de personnes âgées résidant à Bordeaux (Gironde, France) a
été interrogé en 2000 dans le cadre d'une enquête nutritionnelle. L'échantillon
est constitué de 226 sujets.
}
\usage{data(NUTRIAGE)}
\format{
  Un data frame avec 226 observations mesurées sur les 13 variables suivantes.
  \describe{
    \item{\code{sexe}}{un vecteur numérique}
    \item{\code{situation}}{un vecteur numérique}
    \item{\code{the}}{un vecteur numérique}
    \item{\code{cafe}}{un vecteur numérique}
    \item{\code{taille}}{un vecteur numérique}
    \item{\code{poids}}{un vecteur numérique}
    \item{\code{age}}{un vecteur numérique}
    \item{\code{viande}}{un vecteur numérique}
    \item{\code{poisson}}{un vecteur numérique}
    \item{\code{fruit_crus}}{un vecteur numérique}
    \item{\code{fruit_legume_cuits}}{un vecteur numérique}
    \item{\code{chocol}}{un vecteur numérique}
    \item{\code{matgras}}{un vecteur numérique}
  }
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(NUTRIAGE)
str(NUTRIAGE) 
}
\keyword{datasets}
