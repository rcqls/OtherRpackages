\encoding{latin1}
\name{fleches}
\alias{fleches}
\title{La fonction fleches}
\description{Cette fonction permet d'ajouter une flèche sur les extrémités des axes d'un graphique}
\usage{fleches(x = TRUE, y = TRUE)}
\arguments{
  \item{x}{logique. La valeur par défaut est \code{TRUE} indiquant une flèche sur l'axe des abscisses}
  \item{y}{logique. La valeur par défaut est \code{TRUE} indiquant une flèche sur l'axe des ordonnées}
}
\references{
Chapitre 9 (Statistique descriptive) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
curve(cos(x),xlim=c(-10,10))
fleches()
}
