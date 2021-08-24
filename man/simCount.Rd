\name{simCount}
\alias{simCount}
\alias{simCountBin}
\alias{simCountNorm}
\alias{simCountPois}
\encoding{UTF-8}

\title{
Simulates count survey data
}
\description{
Simulates count survey data assuming a binomial, normal, or Poisson sampling process. For the last two, it is assumed that individuals may be double counted and missed at the same rate.

}
\usage{
simCountBin(N, pDetect)
simCountNorm(N, sigma)
simCountPois(N)
}
\arguments{
  \item{N}{
annual number of individuals at risk of detection, which may be all adults or just those breeding. The length of the vector determines the number of sampling occasions.
}
  \item{pDetect}{
vector with the annual detection probabilities of the individuals at risk of counting; all individuals have the same probability to be counted; a scalar can be used if probability does not change with time.
}
  \item{sigma}{
vector with the annual observation error (SD); a scalar can be used if error does not change with time.
}
}
\value{
A list with the parameters entered and \code{count} with the number of individuals counted in each year.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 5.5.1.
}

\examples{
# Generate a simulated population:
pop <- simPop()
pop$breeders
# Simulate count data of breeders or of adults
simCountBin(pop$totBreeders, p=0.6)
simCountNorm(pop$totBreeders, sigma=10)
simCountPois(pop$totAdults)
}
