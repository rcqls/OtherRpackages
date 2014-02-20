\encoding{latin1}
\name{INFARCTUS}
\alias{INFARCTUS}
\docType{data}
\title{Etude sur l'infarctus du myocarde}
\description{
Ce jeu de données provient d'une étude épidémiologique
traitée par une équipe de l'Institut de santé publique
d'épidémiologie et de développement (ISPED) de Bordeaux. Il s'agit d'une étude
cas-témoins sur l'infarctus du myocarde
}
\usage{data(INFARCTUS)}
\format{
  Un data frame avec 449 observations mesurées sur les 10 variables suivantes.
  \describe{
    \item{\code{NUMERO}}{un vecteur numérique}
    \item{\code{INFARCT}}{un vecteur numérique}
    \item{\code{CO}}{un vecteur numérique}
    \item{\code{TABAC}}{un vecteur numérique}
    \item{\code{AGE}}{un vecteur numérique}
    \item{\code{POIDS}}{un vecteur numérique}
    \item{\code{TAILLE}}{un vecteur numérique}
    \item{\code{IMC}}{un vecteur numérique}
    \item{\code{ATCD}}{un vecteur numérique}
    \item{\code{HTA}}{un vecteur numérique}
  }
}
\details{
Les données suivantes sont issues d'une enquête cas-témoins dont le but
était d'évaluer l'existence d'un risque plus élevé de survenue d'un infarctus du
myocarde chez les femmes qui utilisent ou ont utilisé des contraceptifs oraux.
L'étude a été menée auprès de 149 femmes ayant eu un infarctus du myocarde
(cas) et 300 femmes n'en n'ayant pas eu (témoins). 
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(INFARCTUS)
str(INFARCTUS) 
}
\keyword{datasets}
