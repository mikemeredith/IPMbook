\name{hoopoe}
\alias{hoopoe}
\encoding{UTF-8}
\docType{data}

\title{
Data for hoopoe
}
\description{
Data for hoopoe \emph{Upupa epops} from the Valais valley in Switzerland from 2002 to 2017.
}

\usage{data("hoopoe")}

\format{
\code{hoopoe} is a list with 5 components:

\describe{
  \item{ch }{matrix of capture histories for 3,844 female birds x 16 years.}
  \item{age }{the age of each female at first capture, 1 = nestling, 2 otherwise.}
  \item{count }{the annual population index (number of breeding pairs recorded).}
  \item{reproAgg }{a data frame with annual totals of mothers for which fledglings were recorded (B1 = 1-year-old, B2 = older females) and the corresponding number of fledglings (J1 and J2 respectively).}
  \item{reproInd }{a data frame with details of reproduction of individual females: id = mother's ID, f = total annual number of fledglings raised by each mother, including any replacement and second broods, year = year of the record.}
  }
}

\source{Arlettaz et al. (2010), Schaub et al. (2012)}

\references{
Arlettaz, R., M. Schaub, J. Fournier, T. S. Reichlin, A. Sierro, J. E. M. Watson, and V. Braunisch (2010) From Publications to Public Actions: When Conservation Biologists Bridge the Gap between Research and Implementation. \emph{BioScience} 60:835–842.

Schaub, M., T. S. Reichlin, F. Abadi, M. Kéry, L. Jenni, and R. Arlettaz (2012) The demographic drivers of local population dynamics in two rare migratory birds. \emph{Oecologia} 168:97–108.

Schaub, M. & Kéry, M. (2021), \emph{Integrated Population Modeling}, chapter 14.
}

\examples{
data(hoopoe)
str(hoopoe)
}
\keyword{datasets}
