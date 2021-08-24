\name{wryneck}
\alias{wryneck}
\encoding{UTF-8}
\docType{data}

\title{
Data from wrynecks in Switzerland, 2002-2006
}
\description{
The data contain information about the survival of 181 broods of wrynecks (\emph{Jynx torquilla})  collected in the Valais (Swiss Alps) from 2002-2006 in nestboxes. Dates are recorded such that day 1 is May 16.
}

\usage{data(wryneck)}

\format{
\code{wryneck} is a data frame with 5 columns:

\describe{
  \item{f }{day of first detection of nestlings.}
  \item{j }{day of the latest check when nestlings were present.}
  \item{k }{day of the last check; this is the same as 'j' if nestlings fledged successfully, or the day when the nest was found to have failed.}
  \item{x }{1 if nestlings fledged successfully, otherwise 0.}
  \item{age }{the age of the nestlings on first detection in number of days.}
  }
}

\source{Swiss Ornithological Institute}

\references{
Geiser, S., R. Arlettaz, M. Schaub (2008) Impact of weather variation on feeding behaviour, nestling growth and brood survival in Wrynecks \emph{Jynx torquilla}. \emph{Journal of Ornithology} 149, 597-606.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 4.4.6.
}

\examples{
data(wryneck)
str(wryneck)
}
\keyword{datasets}
