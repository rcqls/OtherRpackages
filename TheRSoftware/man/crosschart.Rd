\name{crosschart}
\alias{crosschart}
\title{A cross chart}
\description{A cross chart displays for each observation a smal cross
  above the associated modality}
\usage{crosschart(x, my.title, col,family="Courier")}
\arguments{
  \item{x}{qualitative variable}
  \item{my.title}{character. title of the plot}
  \item{col}{vector of characters for the color of each modality}
  \item{family}{font family for the title. Default is "Courier". Another
    choice can be, e.g., "HersheyScript"}
}
\value{A cross chart}
\references{
Chapter 11 (Descriptive Statistics) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(NUTRIELDERLY)
attach(NUTRIELDERLY)
situation <- as.factor(situation)
levels(situation) <- c("single","couple","family","other")
crosschart(situation,col=c("orange","darkgreen","black","tan"))
detach(NUTRIELDERLY)
}

