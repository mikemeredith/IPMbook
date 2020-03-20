\name{simCapHist}
\alias{simCapHist}
\encoding{UTF-8}

\title{
Simulate capture histories from a population
}
\description{
Simulate capture histories from a population

}
\usage{
simCapHist(ind, c, p, maxAge = 2)
}
\arguments{
  \item{ind}{
array with the population, usually the output from \code{\link{simPop}}
}
  \item{c}{
matrix with age- and time-specific capture probabilities (probability of first capture).
}
  \item{p}{
matrix with age- and time-specific REcapture probabilities.
}
  \item{maxAge}{
maximal number of age classes that can be identified when the individuals are captured for the first time.
}
}
\value{
A list with the following 2 components:

 \item{ch }{matrix with the capture histories}
 \item{age }{vector with the age class at first capture for each individual}
}
\author{
M. Schaub
}

\examples{
# TODO
}
