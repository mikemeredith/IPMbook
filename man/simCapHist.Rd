\name{simCapHist}
\alias{simCapHist}
\encoding{UTF-8}

\title{
Simulates capture histories from a population
}
\description{
Simulates capture histories from a simulated population created by \code{simPop}. Capture probabilities may vary among the age classes and with time, and can be different for initial capture vs recapture.
}

\usage{
simCapHist(state, cap = c(0.35, 0.4), recap = NULL, maxAge = NULL, verbose = TRUE)
}

\arguments{
  \item{state}{
an animals x years matrix with the age of each animal in the population (animals born in the current year coded as age = 0), usually the output from \code{\link{simPop}.}
}
  \item{cap}{
matrix with age- and time-specific probabilities for initial capture; the first row refers to newborns/nestlings.  Input can be \link{abbreviated}.
}
  \item{recap}{
matrix with age- and time-specific probabilities probabilities of REcapture: this has 1 less column than \code{cap} (no recaptures in the first year) and 1 less row (animals marked in the youngest class will be older when recaptured). Input can be \link{abbreviated}. If \code{recap = NULL}, recapture probabilities will be the same as initial capture probabilities.
}
  \item{maxAge}{
maximum number of age classes that can be identified when the individuals are captured for the first time; if NULL, no upper limit is imposed.
}
  \item{verbose}{
if TRUE, information is displayed in the console.
}
}
\value{
A list with the arguments used and the following 2 components:

 \item{ch }{matrix with the capture histories.}
 \item{age }{vector with the age class at first capture for each individual; note that animals marked as newborns/nestlings have age = 1, older individuals have ages from 2 to \code{maxAge}.}
}
\author{
Michael Schaub
}
\references{
Schaub, M.,  Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 5.5.1.
}

\examples{
# Generate a simulated population:
pop <- simPop()
# Simulate capture histories
ch <- simCapHist(pop$state)
str(ch)
ch$cap
ch$recap
ch$age
head(ch$ch)

# Time-varying recapture probabilities, same for all classes:
#  use 1-row matrix
ch <- simCapHist(pop$state, recap=matrix(1:5/10, nrow=1))
ch$recap
}
