\encoding{latin1}
\name{NUTRIAGE}
\alias{NUTRIAGE}
\docType{data}
\title{Alimentation chez des personnes agees}
\description{
Ce jeu de donn�es provient d'une �tude �pid�miologique
trait�e par une �quipe de l'Institut de sant� publique
d'�pid�miologie et de d�veloppement (ISPED) de Bordeaux.
Un �chantillon de personnes �g�es r�sidant � Bordeaux (Gironde, France) a
�t� interrog� en 2000 dans le cadre d'une enqu�te nutritionnelle. L'�chantillon
est constitu� de 226 sujets.
}
\usage{data(NUTRIAGE)}
\format{
  Un data frame avec 226 observations mesur�es sur les 13 variables suivantes.
  \describe{
    \item{\code{sexe}}{un vecteur num�rique}
    \item{\code{situation}}{un vecteur num�rique}
    \item{\code{the}}{un vecteur num�rique}
    \item{\code{cafe}}{un vecteur num�rique}
    \item{\code{taille}}{un vecteur num�rique}
    \item{\code{poids}}{un vecteur num�rique}
    \item{\code{age}}{un vecteur num�rique}
    \item{\code{viande}}{un vecteur num�rique}
    \item{\code{poisson}}{un vecteur num�rique}
    \item{\code{fruit_crus}}{un vecteur num�rique}
    \item{\code{fruit_legume_cuits}}{un vecteur num�rique}
    \item{\code{chocol}}{un vecteur num�rique}
    \item{\code{matgras}}{un vecteur num�rique}
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
