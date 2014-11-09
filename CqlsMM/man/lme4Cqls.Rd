\name{lmer.cqls}
\alias{lmer.cqls}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ lmer interface }
\description{
   An attempt to provide a wrapper for lmer function based on the case-sensitive principle  describing fixed and random factors.
}
\usage{
lmer.cqls(formula,data,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula object}
  \item{data}{a required data.frame object}
  \item{\dots}{further arguments to add to the original lmer function (from lme4 package).}
}
\details{
     
}
\value{
      
}
\author{ Cqls Team }

\seealso{ \code{\link{lmer}}}
\examples{
 ## linear mixed models - reference values from older code
(fm1 <- lmer.cqls(Reaction ~ days*(1+Subject), sleepstudy))
summary(fm1)# (with its own print method)

str(terms(fm1))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
