\name{NUTRIELDERLY}
\alias{NUTRIELDERLY}
\docType{data}
\title{Diet of Elderly People}
\description{
A sample of 226 elderly people living in Bordeaux (Gironde, South-West France)
were interviewed in 2000 for a nutritional study.
}
\usage{data(NUTRIELDERLY)}
\format{
  A data frame with 226 observations measured on the 13 following variables:
  \describe{
    \item{\code{gender}}{2 = female; 1 = male}
    \item{\code{situation}}{1 = single; 2 = living with spouse; 3 =
      living with family; 4 = living with someone else; Family status.
}
    \item{\code{tea}}{Number of cups. Daily consumption of tea.}
    \item{\code{coffee}}{Number of cups. Daily consumption of coffee}
    \item{\code{height}}{Height in cm.}
    \item{\code{weight}}{Weight in cm.}
    \item{\code{age}}{Age in years at date of interview.}
    \item{\code{meat}}{0 = never; 1 = less than once a week; 2 = Once a
      week; 3 = 2/3 times a week; 4 = 4/6 times a week; 5 = every day. Consumption of meat.
}
    \item{\code{fish}}{Idem. Consumption of fish.}
    \item{\code{raw_fruits}}{Idem. Consumption of raw fruits.}
    \item{\code{cooked_fruits_veg}}{Idem. Consumption of cooked fruits and vegetables.}
    \item{\code{chocol}}{Idem. Consumption of chocolate.}
    \item{\code{fat}}{1 = butter; 2 = margarine; 3 = peanut oil; 4 =
      sunflower oil; 5 = olive oil; 6 = mix of vegetable oils (e.g.,
      Isio4); 7 = colza oil; 8 = duck or goose fat. Type of fat used for cooking.
}
  }
}
\source{\url{http://www.biostatisticien.eu/springeR/}}
\author{
Lafaye de Micheaux Pierre <lafaye@dms.umontreal.ca>, Remy Drouilhet <Remy.Drouilhet@upmf-grenoble.fr>, Liquet Benoit <benoit.liquet@isped.u-bordeaux2.fr>
}
\examples{
data(NUTRIELDERLY)
str(NUTRIELDERLY) 
}
\keyword{datasets}
