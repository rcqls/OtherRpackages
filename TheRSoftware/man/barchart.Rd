\name{barchart}
\alias{barchart}
\title{Bar charts}
\description{Pretty bar charts}
\usage{barchart(x, col, my.title, pareto = FALSE, freq.cumul = FALSE, family = "Courier")}
\arguments{
  \item{x}{qualitative variable}
  \item{col}{vector of characters for the color of each modality}
  \item{my.title}{character. title of the plot}
  \item{pareto}{logical. \code{TRUE} for a Pareto diagram. Default os \code{FALSE}}
  \item{freq.cumul}{logical. \code{TRUE} to add a curve of cumulative
    frequencies. By default \code{freq.cumul} is \code{FALSE}}
  \item{family}{font family for the title. Default is "Courier". Another
    choice can be, e.g., "HersheyScript"}
}

\value{A plot}
\references{
Chapter 11 (Descriptive Statistics) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\seealso{\code{\link{barplot}}}
\examples{
data(NUTRIELDERLY)
attach(NUTRIELDERLY)
fat <- as.factor(fat)
col <- c("yellow","yellow2","sandybrown","orange",
   "darkolivegreen","green","olivedrab2","green4")
barchart(fat,col,pareto=TRUE)
detach(NUTRIELDERLY)
}


