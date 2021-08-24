\name{hoopoe}
\alias{hoopoe}
\encoding{UTF-8}
\docType{data}

\title{
Data for hoopoe from Switzerland, 2002-2017
}
\description{
The data for the hoopoe (\emph{Upupa epops}) were collected from 2002 to 2017 in the Swiss Alps (Valais). The nestboxes were checked regularly to record the number of broods and to determine breeding success. Nestlings were ringed and breeding adults captured.
}

\usage{data(hoopoe)}

\format{
\code{hoopoe} is a list with 5 components:

\describe{
  \item{ch }{matrix of capture histories for 3,844 female birds x 16 years.}
  \item{age }{the age of each female at first capture, 1 = nestling, 2 = at least 1-year old.}
  \item{count }{the annual population index (number of breeding pairs recorded).}
  \item{reproAgg }{a data frame with annual totals of mothers for which fledglings were recorded (B1 = 1-year-old, B2 = older females) and the corresponding number of fledglings (J1 and J2, respectively).}
  \item{reproInd }{a data frame with details of reproduction of individual females: id = mother's ID, f = total annual number of fledglings raised by each mother, including any replacement and second broods, year = year of the record.}
  }
}

\source{Swiss Ornithological Institute and Institute of Ecology and Evolution (Division of Conservation Biology), University of Bern.}

\references{
Arlettaz, R., M. Schaub, J. Fournier, T. S. Reichlin, A. Sierro, J. E. M. Watson, V. Braunisch (2010) From Publications to Public Actions: When Conservation Biologists Bridge the Gap between Research and Implementation. \emph{BioScience} 60, 835-842.

Plard, F., R. Arlettaz, A. Jacot, M. Schaub (2020) Disentangling the spatial and temporal causes of decline in a bird population. \emph{Ecology and Evolution} 10, 6909-6918.

Schaub, M., T. S. Reichlin, F. Abadi, M. Kéry, L. Jenni, R. Arlettaz (2012) The demographic drivers of local population dynamics in two rare migratory birds. \emph{Oecologia} 168, 97-108.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 14.
}

\examples{
data(hoopoe)
str(hoopoe)
}
\keyword{datasets}
