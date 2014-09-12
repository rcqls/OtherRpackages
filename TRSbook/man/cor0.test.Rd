\name{cor0.test}
\alias{cor0.test}
\title{Test of the correlation coefficient}
\description{Test of the correlation coefficient between two
  quantitative variables}
\usage{cor0.test(x, y, rho0 = 0, alternative = c("two.sided", "less", "greater"))}
\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{rho0}{numeric indicating the value of the correlation
    coefficient under the null. Default is \code{rho0} = 0}
  \item{alternative}{Alternative hypothesis for the test. Either two
    sided ("two.sided"), one sided to the left  
  ("less") or one sided to the right ("greater"). Default is "two.sided".}
}
\value{
Returns a list:
\item{statistic}{Value of the test statistic}
\item{p.value}{p-value of the test}
} 
\seealso{\code{\link{cor.test}}}
\references{
Chapter 13 (Confidence Intervals and Hypothesis Testing) from the book:
The R Software, Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
data(BMI.CHILD)
attach(BMI.CHILD)
cor0.test(weight,height)
detach(BMI.CHILD)
}
