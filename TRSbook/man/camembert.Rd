\name{camembert}
\alias{camembert}
\title{Pie chart}
\description{A variant of the \code{pie} function}
\usage{
camembert(x, col = NULL, family="Courier")
}
\arguments{
  \item{x}{qualitative variable}
  \item{col}{vector of characters for the color of each modality}
  \item{family}{font family for the title. Default is "Courier". Another
    choice can be, e.g., "HersheyScript"}
}

\value{A pie chart}
\references{
Chapter 11 (Descriptive Statistics) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\seealso{\code{\link{pie}}}
\examples{
data(NUTRIELDERLY)
attach(NUTRIELDERLY)
require("RColorBrewer")
col <- brewer.pal(8,"Pastel2")
camembert(fat,col)
detach(NUTRIELDERLY)
}
