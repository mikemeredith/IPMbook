\name{woodchat6}
\alias{woodchat6}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for Woodchat Shrike for use in section 6.3
}
\description{
Demographic parameters based on the Woodchat Shrike are introduced in Schaub & Kéry (2021) section 3.2 and details of the simulation are given in section 5.5. This data set has simulated data for use in section 6.3.
}

\usage{data("woodchat6")}

\format{
\code{woodchat6} is a list with 6 components:

\describe{
  \item{ch }{capture histories for 947 females x 10 years.}
  \item{age }{the age of each individual at first capture.}
  \item{count }{number of breeding pairs recorded in each year.}
  \item{J }{annual number of juveniles (fledglings) per year.}
  \item{B }{annual number of broods surveyed from which the fledglings counts are obtained.}
  \item{f }{number of fledglings per nest.}
  }
}

\source{
Simulated data.
}
\references{
Schaub, M. & Kéry, M. (2021) \emph{Integrated Population Modeling}, section 6.3.
}

\examples{
data(woodchat6)
str(woodchat6)
}
\keyword{datasets}
