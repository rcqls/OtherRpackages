\name{twosample.cor.test}
\alias{twosample.cor.test}
\title{Comparing statistically two correlation coefficients}
\description{Test of the equality of two correlation coefficients}
\usage{
twosample.cor.test(x1, y1, x2, y2, alpha = 0.05,alternative =
c("two.sided", "less", "greater"))
}
\arguments{
  \item{x1}{\code{x1} is a numeric vector associated to \code{y1}}
  \item{y1}{\code{y1} is a numeric vector associated to \code{x1}}
  \item{x2}{\code{x2} is a numeric vector associated to \code{y2}}
  \item{y2}{\code{y2} is a numeric vector associated to \code{x2}}
  \item{alpha}{significance level of the test}
  \item{alternative}{Alternative hypothesis for the test. Either two
    sided ("two.sided"), one sided to the left  
  ("less") or one sided to the right ("greater"). Default is "two.sided".}
}


\value{
Returns a list:
\item{statistic}{Value of the test statistic}
\item{p.value}{p-value of the test}
 }
\seealso{\code{\link{cor0.test}}}
\references{
Chapter 13 (Confidence Intervals and Hypothesis Testing) from the book:
The R Software, Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
data(BMI.CHILD)
attach(BMI.CHILD)
indf <- which(GENDER=="F")  # To  retrieve indices of the females.
indm <- which(GENDER=="M")  # To retrieve indices of the males.
twosample.cor.test(height[indf],weight[indf],
                   height[indm],weight[indm])
detach(BMI.CHILD)
}

