\encoding{latin1}
\name{tuyauxorgue}
\alias{tuyauxorgue}
\title{Diagramme en tuyaux d'orgues}
\description{Diagramme en tuyaux d'orgues}
\usage{tuyauxorgue(x, col, titre, pareto = FALSE, freq.cumul = FALSE, family = "HersheyScript")}
\arguments{
  \item{x}{variable qualitative}
  \item{col}{vecteur de chaines de caractères pour la couleur de chaque modalité}
  \item{titre}{chaine de caractères pour le titre du graphique}
  \item{pareto}{logique. \code{TRUE} pour un diagramme de Pareto. Par défaut \code{pareto} est \code{FALSE}}
  \item{freq.cumul}{logique. \code{TRUE} pour ajouter la courbe des fréquences cumulées. Par défaut \code{freq.cumul} est \code{FALSE}}
  \item{family}{famille de polices pour le titre. Par défaut, la police utilisée est "HersheyScript"}
}

\value{Un graphique}
\references{
Chapitre 9 (Statistique descriptive) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\seealso{\code{\link{barplot}}}
\examples{
data(NUTRIAGE)
attach(NUTRIAGE)
situation <- as.factor(situation)
levels(situation) <- c("seul","couple","famille","autre")
col <- c("gray","orangered","lightgoldenrodyellow","red")
tuyauxorgue(situation,col)
detach(NUTRIAGE)
}


