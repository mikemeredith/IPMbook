\name{woodchat10}
\alias{woodchat10}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for woodchat shrike for use in chapter 10
}
\description{
Demographic parameters based on the woodchat shrike are introduced in Schaub & Kéry (2021) section 3.2 and details of the simulation are given in section 5.5. This data set has simulated data for use in chapter 10.
}

\usage{data(woodchat10)}

\format{
\code{woodchat10} is a list with 4 components:

\describe{
  \item{marr.a }{capture histories for females marked as adults in m-array format.}
  \item{marr.j }{capture histories for females marked as fledglings in m-array format.}
  \item{B }{ number of broods monitored each year.}
  \item{J }{ number of fledglings produced by the monitored broods.}
  \item{count }{ number of breeding pairs recorded in each year.}
  }
}

\source{
Simulated data
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 10.5.1.
}

\examples{
data(woodchat10)
str(woodchat10)
}
\keyword{datasets}
