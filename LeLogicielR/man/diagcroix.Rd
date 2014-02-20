\encoding{latin1}
\name{diagcroix}
\alias{diagcroix}
\title{Diagramme en croix}
\description{Le diagramme en croix affiche pour chaque observation une petite croix au-dessus de la modalité correspondante}
\usage{diagcroix(x, titre, col, family = "HersheyScript")}
\arguments{
  \item{x}{variable qualitative}
  \item{titre}{chaine de caractères pour le titre du graphique}
  \item{col}{vecteur de chaines de caractères pour la couleur de chaque modalité}
  \item{family}{famille de polices pour le titre. Par défaut, la police utilisée est "HersheyScript".}
}
\value{Un graphique}
\references{
Chapitre 9 (Statistique descriptive) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(NUTRIAGE)
attach(NUTRIAGE)
situation <- as.factor(situation)
levels(situation) <- c("seul","couple","famille","autre")
diagcroix(situation,col=c("orange","darkgreen","black","tan"))
detach(NUTRIAGE)
}

