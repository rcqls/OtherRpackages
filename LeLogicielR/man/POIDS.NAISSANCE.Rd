\encoding{latin1}
\name{POIDS.NAISSANCE}
\alias{POIDS.NAISSANCE}
\docType{data}
\title{Poids de naissance}
\description{
Ce jeu de donn�es provient d'une �tude �pid�miologique
trait�e par une �quipe de l'Institut de sant� publique
d'�pid�miologie et de d�veloppement (ISPED) de Bordeaux. Il
s'agit d'une enqu�te concernant les facteurs de risque associ�s au faible
poids de naissance de nourrissons (donn�es collect�es au centre m�dical de Baystate
dans le Massachusetts pendant l'ann�e 1986).
}
\usage{data(POIDS.NAISSANCE)}
\format{
  Un data frame avec 189 observations mesur�es sur les 11 variables suivantes.
  \describe{
    \item{\code{ID}}{un vecteur num�rique}
    \item{\code{AGE}}{un vecteur num�rique}
    \item{\code{LWT}}{un vecteur num�rique}
    \item{\code{RACE}}{un vecteur num�rique}
    \item{\code{SMOKE}}{un vecteur num�rique}
    \item{\code{PTL}}{un vecteur num�rique}
    \item{\code{HT}}{un vecteur num�rique}
    \item{\code{UI}}{un vecteur num�rique}
    \item{\code{FVT}}{un vecteur num�rique}
    \item{\code{BWT}}{un vecteur num�rique}
    \item{\code{LOW}}{un vecteur num�rique}
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
