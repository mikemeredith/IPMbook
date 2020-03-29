\name{simCapHist}
\alias{simCapHist}
\alias{simCapHist2}
\encoding{UTF-8}

\title{
Simulate capture histories from a population
}
\description{
Simulate capture histories from a simulated population created by \code{simPop/simPop2}. Capture probabilities may vary among the age classes and with time, and can be different for initial capture vs recapture.
}
\usage{
simCapHist(ind, c, p, maxAge = 2)
simCapHist2(state, cap = c(0.35, 0.4), recap = 0.6, maxAge = 2)
}
\arguments{
  \item{ind}{
array with the population, usually the output from \code{\link{simPop}}
}
  \item{state}{
an animals x years matrix with the state of each animal in the population, usually the output from \code{\link{simPop2}}
}
  \item{c, cap}{
matrix with age- and time-specific probabilities for initial capture. For \code{simPop2}, this may be a vector if probabilities do not vary with time, or scalar if all probabilities are equal.
}
  \item{p, recap}{
matrix with age- and time-specific probabilities probabilities of REcapture. For \code{simPop2}, this may be a vector if probabilities do not vary with time, or scalar if all probabilities are equal.
}
  \item{maxAge}{
maximal number of age classes that can be identified when the individuals are captured for the first time. Currently only maxAge = 2 is supported.
}
}
\value{
A list with the arguments used and following 2 components:

 \item{ch }{matrix with the capture histories}
 \item{age }{vector with the age class at first capture for each individual}
}
\author{
M. Schaub
}

\examples{
# Generate a simulated population:
pop <- simPop2()
ch <- simCapHist2(pop$state)
str(ch)
ch$cap
ch$age
head(ch$ch)
}
