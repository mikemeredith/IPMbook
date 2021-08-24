\name{woodchat64}
\alias{woodchat64}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for woodchat shrike for use in section 6.4
}
\description{
Demographic parameters based on the woodchat shrike are introduced in Schaub & Kéry (2021) section 3.2 and details of the simulation are given in section 5.5. This data set has simulated data for use in section 6.4.
}

\usage{data(woodchat64)}

\format{
\code{woodchat64} is a list with 6 components:

\describe{
  \item{ch }{capture histories for 947 females x 10 years.}
  \item{age }{the age of each individual at first capture: 1 = juvenile, 2 = at least 1 year old (adult).}
  \item{count }{number of breeding pairs recorded in each year.}
  \item{J }{annual number of juveniles (fledglings) per year.}
  \item{B }{annual number of broods surveyed from which the fledglings counts are obtained.}
  \item{trueN }{true number of breeding pairs (known because it is a simulated data set).}
  }
}

\source{
Simulated data
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 6.4.
}
\examples{
data(woodchat64)
str(woodchat64)
}
\keyword{datasets}
