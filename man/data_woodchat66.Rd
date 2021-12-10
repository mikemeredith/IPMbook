\name{woodchat66}
\alias{woodchat66}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for woodchat shrike for use in section 6.6
}
\description{
Demographic parameters based on the woodchat shrike are introduced in Schaub & Kéry (2021) section 3.2 and details of the simulation are given in section 5.5. This data set has simulated data for use in section 6.6.
}

\usage{data(woodchat66)}

\format{
\code{woodchat66} is a list with 5 components:

\describe{
  \item{ch }{capture histories for 947 females x 10 years.}
  \item{age }{the age of each individual at first capture: 1 = juvenile, 2 = at least 1 year old (adult).}
  \item{count }{number of breeding pairs recorded in each year.}
  \item{J }{annual number of juveniles (fledglings) per year.}
  \item{B }{annual number of broods surveyed from which the fledglings counts are obtained.}
  }
}

\source{
Simulated data
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 6.6.2.
}
\examples{
data(woodchat66)
str(woodchat66)
}
\keyword{datasets}
