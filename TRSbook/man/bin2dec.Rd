\name{bin2dec}
\alias{bin2dec}
\title{Decimal representation of a binary number}
\description{To compute the decimal representation of a number written
  in a binary format}
\usage{bin2dec(x)}
\arguments{
  \item{x}{Numeric. Number in binary format written only with 0s and
    1s. See Example below.}
}
\value{Decimal representation of the number \code{x}}
\references{
Chapter 5 (Data Manipulation, Functions) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
bin2dec(1010.101)
}
