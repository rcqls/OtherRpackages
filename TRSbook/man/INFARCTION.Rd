\name{INFARCTION}
\alias{INFARCTION}
\docType{data}
\title{Study Case of Myocardial Infarction}
\description{
The study for which the following data were collected aimed at examining
whether women who use or have used oral contraceptives are at a higher risk of myocardial infarction. The sample includes 149 women who had myocardial infarction
(cases) and 300 women who did not (controls). The main exposure factor is usage of
oral contraceptives; the data also include age, weight, height, tobacco consumption,
hypertension and family history of cardiovascular diseases.
}
\usage{data(INFARCTION)}
\format{
  A data frame with 449 observations measured on the following 10 variables:
  \describe{
    \item{\code{NUMBER}}{Identification.}
    \item{\code{infarct}}{0 = controls; 1 = cases. Myocardial infarction.}
    \item{\code{co}}{0 = never; 1 = yes. Usage of oral contraceptives.}
    \item{\code{tobacco}}{0 = no; 1 = smoker; 2 = fromer smoker. Tobacco usage.}
    \item{\code{age}}{Age in years.}
    \item{\code{weight}}{Weight in kg.}
    \item{\code{height}}{Height in cm.}
    \item{\code{atcd}}{0 = no; 1 = yes. Family history of cardiovascular diseases.}
    \item{\code{hta}}{0 = no; 1 = yes. Hypertension.}
  }
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <b.liquet@uq.edu.au>
}
\examples{
data(INFARCTION)
str(INFARCTION) 
}
\keyword{datasets}
