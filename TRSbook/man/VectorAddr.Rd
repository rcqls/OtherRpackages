\name{VectorAddr}
\alias{VectorAddr}
\alias{print.VectorAddr}
\alias{update.VectorAddr}
\alias{Ops.VectorAddr}
\title{Address of vector}
\description{Object representing an address of numeric vector}
\usage{VectorAddr(x)}
\arguments{
  \item{x}{Vector.}
}
\value{An object of class VectorAddr.}
\references{
Chapter 9 (Managing Sessions) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis
}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
\dontrun{
x <- c(8L,9L)
addr <- VectorAddr(x)		# Gets the address of the first
                            # box of the 64-box block where x
                            # is stored.
addr
update(addr,6L) # Write the integer 6 at this address.
x
update(addr+4L,7L) # An integer is coded over 4 bytes,
                      # hence increment the address by 4 to
                      # get to x[2].
x
x <- c(12.8,4.5)
x
addr <- VectorAddr(x)		# Get the address of the first box
                            # of the 128-box block where x is
                            # stored.
update(addr,6.2)
x
update(addr+8L,7.1) # A double is coded over 8 bytes.
x
}
}
