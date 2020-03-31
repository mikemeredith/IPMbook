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
simCapHist2(state, cap = c(0.35, 0.4), recap = NULL, maxAge = 2)
}
\arguments{
  \item{ind}{
array with the population, usually the output from \code{\link{simPop}}
}
  \item{state}{
an animals x years matrix with the age of each animal in the population (animals born in the current year coded as age = 0), usually the output from \code{\link{simPop2}}
}
  \item{c, cap}{
matrix with age- and time-specific probabilities for initial capture; the first row refers to newborns/nestlings. For \code{simPop2}, this may be a vector if probabilities do not vary with time, or scalar if all probabilities are equal.
}
  \item{p, recap}{
matrix with age- and time-specific probabilities probabilities of REcapture: this has 1 less column than \code{cap} (no recaptures in the first year) and 1 less row (animals marked in the youngest class will be older when recaptured). For \code{simPop2}, this may be a vector if probabilities do not vary with time, or scalar if all probabilities are equal. If \code{recap = NULL}, recapture probabilities will be the same as initial capture probabilities.
}
  \item{maxAge}{
maximal number of age classes that can be identified when the individuals are captured for the first time.
}
}
\value{
A list with the arguments used and the following 2 components:

 \item{ch }{matrix with the capture histories}
 \item{age }{vector with the age class at first capture for each individual; note that animals marked as infants/nestlings have age = 1, adult ages from 2 to \code{maxAge}}
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
ch$recap
ch$age
head(ch$ch)

# Time-varying recapture probabily, same for all classes:
#  use 1-row matrix
ch <- simCapHist2(pop$state, recap=matrix(1:5/10, nrow=1))
ch$recap
}
