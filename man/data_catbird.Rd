\name{catbird}
\alias{catbird}
\encoding{UTF-8}
\docType{data}

\title{
Data for gray catbird from New England/Mid-Atlantic region, USA, 1992-2008
}
\description{
The data set includes sighting data from the North American Breeding Bird Survey (BBS) and mark-recapture data from the Monitoring Avian Productivity and Survivorship program (MAPS) for gray catbirds (\emph{Dumetella carolinensis}) in the New England/Mid-Atlantic region, USA, for 1992-2008.
}

\usage{data(catbird)}

\format{
\code{catbird} is a list with 9 components:

\describe{
  \item{y }{matrix of capture-recapture data from MAPS data, 4276 birds x 17 years.}
  \item{r }{residency for each bird, 1 if the bird was captured more than once at least 11 days apart in the same year; 0 otherwise.}
  \item{station }{banding station identifier.}
  \item{count }{number of individuals detected on each route in the BBS data.}
  \item{stratum }{identifier of the stratum of the route.}
  \item{year }{year of the count.}
  \item{observer }{identifier of the observer making the count.}
  \item{firstyr }{1 if the observer counted on the route for the first time, 0 otherwise.}
  \item{area }{the area of each stratum.}
}
}

\source{North American Breeding Bird Survey (BBS) and Monitoring Avian Productivity and Survivorship program (MAPS)}

\references{
Ahrestani, F. S., Saracco, J. F., Sauer, J. R., Pardieck, K. L., Royle, J. A. (2017) An integrated population model for bird monitoring in North America. \emph{Ecological Applications} 27, 916-924.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 19.
}

\examples{
data(catbird)
str(catbird)
}
\keyword{datasets}
