\encoding{latin1}
\name{cor0.test}
\alias{cor0.test}
\title{Test du coefficient de correlation}
\description{Test du coefficient de corr�lation entre deux variables quantitatives}
\usage{cor0.test(x, y, rho0 = 0, alternative = c("two.sided", "less", "greater"))}
\arguments{
  \item{x}{un vecteur num�rique}
  \item{y}{est un vecteur num�rique}
  \item{rho0}{r�el indiquant la valeur du coefficient de corr�lation pour l'hypoth�se nulle. Par d�faut \code{rho0} vaut 0}
  \item{alternative}{Hypoth�se alternative du test. Trois tests sont possibles: bilat�ral ("two.sided"), unilat�ral inf�rieur 
  ("less"), unilat�ral sup�rieur ("greater"). Par d�faut c'est l'hypoth�se "two.sided".}
}
\value{
Retourne une liste :
\item{statistic}{La valeur de la statistique de test}
\item{p.value}{La valeur-p du test}
} 
\seealso{\code{\link{cor.test}}}
\references{
Chapitre 11 (Intervalles de confiance et tests d'hypoth�ses) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
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
