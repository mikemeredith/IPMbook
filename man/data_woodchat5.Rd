\name{woodchat5}
\alias{woodchat5}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for Woodchat Shrike for use in Chapter 5
}
\description{
Demographic parameters based on the Woodchat Shrike are introduced in Schaub & Kéry (2021) section 3.2 and details of the simulation are given in section 5.5. This data set has simulated data for use in chapter 5.
}

\usage{data("woodchat5")}

\format{
\code{woodchat5} is a list with 4 components:

\describe{
  \item{ch }{capture histories for 1902 individuals ?? females x 20 years.}
  \item{age }{the age of each individual at first capture: 1 = juvenile, 2 = adult.}
  \item{repro }{reproduction information for 929 monitored nests: \code{Reproduction} = total number of fledglings (male + female), Year = year monitored, Age of mother = ??.}
  \item{count }{total number of breeding pairs ?? observed in each year.}
  }
}

\source{
Simulated data.
}
\references{
Schaub, M. & Kéry, M. (2021) \emph{Integrated Population Modeling}, chapter 5.
}

\examples{
data(woodchat5)
str(woodchat5)
}
\keyword{datasets}
