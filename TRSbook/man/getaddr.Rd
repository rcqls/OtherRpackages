\name{getaddr}
\alias{getaddr}
\title{Retrieve the address in memory of a variable}
\description{Retrieve the address in memory of a numeric variable}
\usage{getaddr(x)}
\arguments{
  \item{x}{numeric}
}
\value{Integer value of the address of \code{x}}
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
addr <- getaddr(x)$addr.int # Gets the address of the first
                            # box of the 64-box block where x
                            # is stored.
addr
writeaddr(addr,6L) # Write the integer 6 at this address.
x
writeaddr(addr+4L,7L) # An integer is coded over 4 bytes,
                      # hence increment the address by 4 to
                      # get to x[2].
x
x <- c(12.8,4.5)
x
addr <- getaddr(x)$addr.int # Get the address of the first box
                            # of the 128-box block where x is
                            # stored.
writeaddr(addr,6.2)
x
writeaddr(addr+8L,7.1) # A double is coded over 8 bytes.
x
}
}

