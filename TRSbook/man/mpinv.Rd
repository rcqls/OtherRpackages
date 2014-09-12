\name{mpinv}
\alias{mpinv}
\title{Moore Penrose inverse}
\description{Computes the Moore Penrose inverse of a matrix}
\usage{mpinv(M,eps=1e-13)}
\arguments{
  \item{M}{a matrix}
  \item{eps}{real precision}
}
\value{The Moore-Penrose inverse of \code{M}}
\references{
Chapter 10 (Basic Mathematics: Matrix Operations, Integration, and Optimization) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
A <- matrix(c(2,3,5,4),nrow=2,ncol=2)
solve(A)
mpinv(A)
B <- matrix(c(4,2,8,4),nrow=2,ncol=2)
# solve(B) # gives an error.
mpinv(B)
}

