\encoding{latin1}
\name{cor2.test.2.sample}
\alias{cor2.test.2.sample}
\title{Comparaison de deux coefficients de correlation}
\description{Test de l'�galit� de deux coefficients de corr�lation}
\usage{cor2.test.2.sample(x1, y1, x2, y2, alternative = c("two.sided", "less", "greater"))}
\arguments{
  \item{x1}{\code{x1} vecteur num�rique associ� � \code{y1}}
  \item{y1}{\code{y1} vecteur num�rique associ� � \code{x1}}
  \item{x2}{\code{x2} vecteur num�rique associ� � \code{y2}}
  \item{y2}{\code{y2} vecteur num�rique associ� � \code{x2}}
  \item{alternative}{Hypoth�se alternative du test. Trois tests sont possibles: bilat�ral ("two.sided"), unilat�ral inf�rieur 
  ("less"), unilat�ral sup�rieur ("greater"). Par d�faut c'est l'hypoth�se "two.sided"}
}


\value{
Retourne une liste contenant
\item{statistic}{La valeur de la statistique de test}
\item{p.value}{La valeur-p du test}
 }
\seealso{\code{\link{cor0.test}}}
\references{
Chapitre 11 (Intervalles de confiance et tests d'hypoth�ses) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(IMC.ENFANT)
attach(IMC.ENFANT)
indf <- which(SEXE=="F")  # Permet de recuperer les indices des filles.
indg <- which(SEXE=="G")  # Permet de recuperer les indices des garcons.
cor2.test.2.sample(taille[indf],poids[indf],taille[indg],poids[indg])
detach(IMC.ENFANT)
}

