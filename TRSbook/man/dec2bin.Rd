\name{dec2bin}
\alias{dec2bin}
\title{Binary representation of a decimal number}
\description{To compute the binary representation of a number written
  in a decimal format}
\usage{dec2bin(x,prec=52)}
\arguments{
  \item{x}{Numeric. Number in a decimal format.}
  \item{prec}{Integer. Precision desired.}
}
\value{Binary representation of the number \code{x}}
\references{
Chapter 5 (Data Manipulation, Functions) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
dec2bin(10.625,3)
}
