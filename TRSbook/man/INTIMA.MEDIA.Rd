\name{INTIMA.MEDIA}
\alias{INTIMA.MEDIA}
\docType{data}
\title{Intima-Media Thickness}
\description{
   Atherosclerosis is the main cause of death for men above 35 and women above
45 in most developed countries. It is a thickening and hardening of internal artery
walls. One of its consequences is myocardial infarction. An artery wall is made of
three layers; innermost to outermost, they are called intima, media and adventitia.
Intima-media thickness is a marker of atherosclerosis. It was measured by ultra-
sonography on a sample of 110 subjects in 1999 in Bordeaux hospitals. Information
on the main risk factors was also collected.
}
\usage{data(INTIMA.MEDIA)}
\format{
  A data frame with 110 observations measured on the 9 following variables:
  \describe{
    \item{\code{GENDER}}{1=male, 2=female. Gender.}
    \item{\code{AGE}}{Age (in years) at date of consultation.}
    \item{\code{height}}{Hieght in cm.}
    \item{\code{weight}}{Weight in kg.}
    \item{\code{tobacco}}{0=non smoker, 1=former smoker,
      2=smoker. Smoking status.}
    \item{\code{packyear}}{Number of packs per year. Estimation of
      tobacco consumption for smokers and former smokers.}
    \item{\code{SPORT}}{0=no, 1=yes. Physicial activity.}
    \item{\code{measure}}{Intima-media thickness in cm}
    \item{\code{alcohol}}{0=non-drinker, 1=occasional drinker, 2=regular
    drinker. Alcohol consumption.}
  }
}

\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
data(INTIMA.MEDIA)
str(INTIMA.MEDIA) 
}
\keyword{datasets}
