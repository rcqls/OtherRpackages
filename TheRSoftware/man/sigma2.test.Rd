\name{sigma2.test}
\alias{sigma2.test}
\title{Test of a variance}
\description{Comparing the theoretical variance with a reference value}
\usage{sigma2.test(x, alternative = "two.sided", var0 = 1, conf.level = 0.95)}
\arguments{
  \item{x}{numeric vector}
  \item{alternative}{Alternative hypothesis for the test. Either two
    sided ("two.sided"), one sided to the left  
  ("less") or one sided to the right ("greater"). Default is "two.sided".}
  \item{var0}{value of reference for the variance}
  \item{conf.level}{confidence level}
}
\value{
Returns a list:
\item{statistic}{Value of the test statistic}
\item{parameter}{degrees of freedom}
\item{p.value}{p-value of the test}
\item{conf.int}{confidence interval}
\item{estimate}{sample variance}
\item{null.value}{value of reference for the variance}
\item{alternative}{Alternative hypothesis for the test}
\item{method}{"One-sample Chi-squared test for given variance"}
\item{data.name}{name of the data set}
} 
\references{
Chapter 13 (Confidence Intervals and Hypothesis Testing) from the book:
The R Software, Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(NUTRIELDERLY)
sigma2.test(NUTRIELDERLY$weight,conf.level=0.9)$conf
}
