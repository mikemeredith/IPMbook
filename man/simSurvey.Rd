\name{simSurvey}
\alias{simSurvey}
\alias{simSurveyBin}
\alias{simSurveyNorm}
\alias{simSurveyPois}
\encoding{UTF-8}

\title{
Simulate population survey data
}
\description{
Simulate population survey data assuming a binomial, normal, or Poisson sampling process. For the last two, it is assumed that individuals may be double counted and missed at the same rate.

}
\usage{
simSurveyBin(N, pDetect)
simSurveyNorm(N, sigma)
simSurveyPois(N)
}
\arguments{
  \item{N}{
Annual number of individuals at risk of detection, which may be all adults or just those breeding. The length of the vector determines the number of sampling occasions.
}
  \item{pDetect}{
vector with the annual detection probabilities of the individuals at risk of counting; all individuals have the same probability to be counted; a scalar can be used if probability does not change with time.
}
  \item{sigma}{
vector with the annual observation error (SD); a scalar can be used if probability does not change with time.
}
}
\value{
A vector with the number of individuals counted in each year.
}
\author{
M. Schaub
}

\examples{
pop <- simPop()
pop$breeders
( N <- pop$breeders["Total", ] )
simSurveyBin(N, p=0.6)
simSurveyNorm(N, sigma=10)
simSurveyPois(N)


}
