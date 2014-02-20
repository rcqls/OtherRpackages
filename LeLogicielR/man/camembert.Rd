\encoding{latin1}
\name{camembert}
\alias{camembert}
\title{Diagramme circulaire}
\description{Cette fonction offre une variante de la fonction \code{pie}}
\usage{
camembert(x, col = NULL, family = "HersheyScript")
}
\arguments{
  \item{x}{variable qualitative}
  \item{col}{un vecteur de chaine de caractères pour la couleur de chaque modalité}
  \item{family}{famille de polices pour le titre. Par défaut, la police utilisée est "HersheyScript".}
}

\value{Un graphique}
\references{
Chapitre 9 (Statistique descriptive) du livre: Le logiciel R, Maitriser le langage, Effectuer des analyses statistiques
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\seealso{\code{\link{pie}}}
\examples{
data(NUTRIAGE)
attach(NUTRIAGE)
matgras <- as.factor(matgras)
levels(matgras) <- c("beurre","margarine","arachide","tournesol","olive","Isio4","colza","canard")
require("RColorBrewer")
col <- brewer.pal(8,"Pastel2")
camembert(matgras,col)
detach(NUTRIAGE)
}
