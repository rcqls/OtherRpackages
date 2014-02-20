\name{flashy.plot}
\alias{flashy.plot}
\title{A flashy scatter plot}
\description{This function tries to make a nicer plot than the one given by the
  \code{plot()} function for two quantitative variables}
\usage{flashy.plot(x,y,my.factor, family = "Courier",xlab="",ylab="")}
\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{my.factor}{factor}
  \item{family}{font family for the title. Default is "Courier". Another
    choice can be, e.g., "HersheyScript"}
  \item{xlab}{character. x label}
  \item{ylab}{character. y label}
}

\value{A flashy scatter plot}
\references{
Chapter 11 (Descriptive Statistics) from the book: The R Software,
Fundamentals of Programming and Statistical Analysis}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\seealso{\code{\link{plot}}}
\examples{
data(NUTRIELDERLY)
attach(NUTRIELDERLY)
gender <- as.factor(gender)
levels(gender) <- c("Male","Female")
flashy.plot(weight,height,gender,xlab="Height",ylab="Weight")
detach(NUTRIELDERLY)
}


