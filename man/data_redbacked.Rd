\name{redbacked}
\alias{redbacked}
\encoding{UTF-8}
\docType{data}

\title{
Data for red-backed shrike from Germany, 1971-2006
}
\description{
Data for the red-backed shrike (\emph{Lanius collurio}) were collected from 1971 to 2006 from a study area in southern Germany. They comprise the annual number of breeding pairs, information about productivity from regular checks of nests, and capture-recapture data   of juvenile and adult shrikes.
}

\usage{data(redbacked)}

\format{
\code{redbacked} is a list with 5 components:

\describe{
  \item{marr.a }{capture histories for females marked as adults in m-array format.}
  \item{marr.j }{capture histories for females marked as fledglings in m-array format.}
  \item{B }{the number of broods monitored each year.}
  \item{J }{the number of fledglings produced each year by the monitored broods.}
  \item{count }{the number of active breeding pairs on 15 June each year.}
  }
}

\source{
Wolfgang Staube, Hans Jakober
}
\references{
Abadi, F., Gimenez, O., Jakober, H., Stauber, W., Arlettaz, R., Schaub, M. (2012) Estimating the strength of density dependence in the presence of observation errors using integrated population models. \emph{Ecological Modelling} 242, 1-9.

Schaub, M., Jakober, H., Stauber, W. (2013) Strong contribution of immigration to local population regulation: evidence from a migratory passerine. \emph{Ecology} 94, 1828-1838.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 8.2.
}
\examples{
data(redbacked)
str(redbacked)
}
\keyword{datasets}
