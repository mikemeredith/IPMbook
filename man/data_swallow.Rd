\name{swallow}
\alias{swallow}
\encoding{UTF-8}
\docType{data}

\title{
Data for barn swallows in Switzerland, 1997-2003
}
\description{
Demographic data of barn swallows \emph{Hirundo rustica} were collected from nine sites that were between 19 and 224 km apart from 1997 to 2003, though not all sites were surveyed every year, resulting in NAs in the data. Pairs can raise more than one brood in a year, and first and second broods were monitored.

See Schaub et al (2015) for details.
}

\usage{data("swallow")}

\format{
\code{swallow} is a list with 5 components:

\describe{
  \item{marr.j }{an array with mark-recapture data for juvenile females in m-array format, sites x release year x recapture year.}
  \item{marr.a }{an array with mark-recapture data for adult females in m-array format, sites x release year x recapture year.}
  \item{counts }{a matrix with number of breeding pairs observed, sites x years.}
  \item{productivity }{an array with productivity variables for monitored broods, brood type (first or second) x sites x years x variable; the variables are total number of eggs and fledglings in monitored broods and the number of broods monitored.}
  \item{second }{an array with the number of first broods and second broods observed, sites x years x brood type.}
  }
}

\source{Swiss Ornithological Institute}

\references{
Schaub, M., von Hirschheydt, J., & Grüebler, M.U. (2015) Differential contribution of demographic rate synchrony to population synchrony in barn swallows. \emph{Journal of Animal Ecology}, 84, 1530-1541.}

\examples{
data(swallow)
str(swallow)
}
\keyword{datasets}
