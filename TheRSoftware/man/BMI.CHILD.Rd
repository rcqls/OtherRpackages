\name{BMI.CHILD}
\alias{BMI.CHILD}
\docType{data}
\title{Body Mass Index of children}
\description{
This data set comes from an epidemiologic study analyzed by a team from
the Institut de sante publique d'epidemiologie et de developpement
(ISPED) de Bordeaux. 
A sample of 152 children (3 or 4 years old) in their first year of kindergarten in
schools in Bordeaux (Gironde, SouthWest France) underwent a physical check-up
in 1996-1997.
}
\usage{data(BMI.CHILD)}
\format{
  A data frame with 152 observations measured on the 6 following variables:
  \describe{
    \item{\code{GENDER}}{a factor with levels \code{F} and \code{M}}
    \item{\code{zep}}{a factor with levels \code{Y} and \code{N}}
    \item{\code{weight}}{numeric}
    \item{\code{years}}{numeric}
    \item{\code{months}}{numeric}
    \item{\code{height}}{numeric}
  }
}

\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(BMI.CHILD)
str(BMI.CHILD) 
}
\keyword{datasets}
