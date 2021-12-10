\name{woodchat7}
\alias{woodchat7}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for woodchat shrike for use in chapter 7
}
\description{
Simulation of data with demographic parameters based on the woodchat shrike are introduced in section 3.2. This data set has additional simulated data for use in chapter 7. It is the same as \code{woodchat5} but with approximately 5 times the number of observations.
}

\usage{data(woodchat7)}

\format{
\code{woodchat7} is a list with 4 components:

\describe{
  \item{ch }{capture histories for 9438 females x 20 years.}
  \item{age }{the age of each individual at first capture: 1 = juvenile, 2 = at least 1 year old (adult).}
  \item{repro }{reproduction information for 6207 monitored nests: Reproduction = total number of fledglings (male + female), Year = year monitored, Age of mother: 1 = 1 year old, 2 = at least 2 years old.}
  \item{count }{total number of breeding pairs observed in each year.}
  }
}

\source{
Simulated data
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press,  section 7.2.1.
}
\examples{
data(woodchat7)
str(woodchat7)
}
\keyword{datasets}
