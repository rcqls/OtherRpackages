\name{arrowaxis}
\alias{arrowaxis}
\title{Adding arrows on statistical plots.}
\description{This function add an arrow on the extremities of the axes
  of a plot}
\usage{arrowaxis(x = TRUE, y = TRUE)}
\arguments{
  \item{x}{Logical. Default value \code{TRUE} indicates an arrow on the x-axis}
  \item{y}{Logical. Default value \code{TRUE} indicates an arrow on the y-axis}
}
\references{
Chapter 11 (Descriptive Statistics) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
curve(cos(x),xlim=c(-10,10))
arrowaxis()
}
