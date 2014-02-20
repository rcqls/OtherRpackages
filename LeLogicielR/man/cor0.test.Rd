\encoding{latin1}
\name{cor0.test}
\alias{cor0.test}
\title{Test du coefficient de correlation}
\description{Test du coefficient de corrélation entre deux variables quantitatives}
\usage{cor0.test(x, y, rho0 = 0, alternative = c("two.sided", "less", "greater"))}
\arguments{
  \item{x}{un vecteur numérique}
  \item{y}{est un vecteur numérique}
  \item{rho0}{réel indiquant la valeur du coefficient de corrélation pour l'hypothèse nulle. Par défaut \code{rho0} vaut 0}
  \item{alternative}{Hypothèse alternative du test. Trois tests sont possibles: bilatéral ("two.sided"), unilatéral inférieur 
  ("less"), unilatéral supérieur ("greater"). Par défaut c'est l'hypothèse "two.sided".}
}
\value{
Retourne une liste :
\item{statistic}{La valeur de la statistique de test}
\item{p.value}{La valeur-p du test}
} 
\seealso{\code{\link{cor.test}}}
\references{
Chapitre 11 (Intervalles de confiance et tests d'hypothèses) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(IMC.ENFANT)
attach(IMC.ENFANT)
cor0.test(poids,taille)
detach(IMC.ENFANT)
}
