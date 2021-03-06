\encoding{latin1}
\name{INTIMA.MEDIA}
\alias{INTIMA.MEDIA}
\docType{data}
\title{Epaisseur de l'intima-media}
\description{
Ce jeu de donn�es provient d'une �tude �pid�miologique
trait�e par une �quipe de l'Institut de sant� publique
d'�pid�miologie et de d�veloppement (ISPED) de Bordeaux.
L'�paisseur de l'intima-m�dia est un marqueur
reconnu d'ath�roscl�rose. Elle a �t� mesur�e par �chographie sur un �chantillon
de 110 sujets en 1999 dans les CHU de Bordeaux. Des informations sur les
principaux facteurs de risque ont aussi �t� recueillies.
}
\usage{data(INTIMA.MEDIA)}
\format{
  Un data frame avec 110 observations mesur�es sur les 9 variables suivantes.
  \describe{
    \item{\code{SEXE}}{un vecteur num�rique}
    \item{\code{AGE}}{un vecteur num�rique}
    \item{\code{taille}}{un vecteur num�rique}
    \item{\code{poids}}{un vecteur num�rique}
    \item{\code{tabac}}{un vecteur num�rique}
    \item{\code{paqan}}{un vecteur num�rique}
    \item{\code{SPORT}}{un vecteur num�rique}
    \item{\code{mesure}}{un vecteur num�rique}
    \item{\code{alcool}}{un vecteur num�rique}
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
