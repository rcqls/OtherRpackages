\name{BIRTH.WEIGHT}
\alias{BIRTH.WEIGHT}
\docType{data}
\title{Weight at Birth}
\description{
This study focused on risks associated with low weight at birth; the data were
collected at the Baystate Medical Centre, Massachusetts, in 1986. Physicians have
been interested in low weight at birth for several years, because
underweight babies
have high rates of infant mortality and infant anomalies. The behaviour of the
mother-to-be during pregnancy (diet, smoking habits) can have a significant impact
on the chances of having a full-term pregnancy, and thus of giving birth to a child
of normal weight. The data file includes information on 189 women (identification
number: ID) who came to the centre for consultation. Weight at birth is categorized
as low if the child weighs less than 2,500 g.
}
\usage{data(BIRTH.WEIGHT)}
\format{
  A data frame with 189 observations measured on the following 11 variables.
  \describe{
    \item{\code{ID}}{Numeric. Identification.}
    \item{\code{AGE}}{Numeric. Age of mother.}
    \item{\code{LWT}}{Numeric. Weight of mother at last menstrual period.}
    \item{\code{RACE}}{1=white, 2=black, 3=other. Race of mother.}
    \item{\code{SMOKE}}{Yes=1, No=0. Smoking during pregnancy.}
    \item{\code{PTL}}{0=none, 1=one, 2=two, etc. Number of premature births in medical history.}
    \item{\code{HT}}{Yes=1, No=0. Medical history of hypertension.}
    \item{\code{UI}}{Yes=1, No=0. Uterine irritability.}
    \item{\code{FVT}}{0=none, 1=one, etc. Number of medical
      consultations during first trimester}
    \item{\code{BWT}}{Numeric. Grams.}
    \item{\code{LOW}}{Yes=1, No=0. Weight at birth less than 2,500g}
  }
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
data(BIRTH.WEIGHT)
str(BIRTH.WEIGHT) 
}
\keyword{datasets}
