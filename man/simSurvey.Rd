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
simSurveyBin(N, psur)
simSurveyNorm(N, sigma)
simSurveyPois(N)
}
\arguments{
  \item{N}{
Annual number of individuals at risk of detection (usually population size).
}
  \item{psur}{
vector with the annual detection probabilities of the individuals at risk of counting; all individuals have the same probability to be counted.
}
  \item{sigma}{
vector with the annual observation error (SD).
}
}
\value{
A vector with the number of individuals counted in each year.
}
\author{
M. Schaub
}

\examples{
# TODO
}
