\name{summaryPop}
\alias{summaryPop}
\encoding{UTF-8}

\title{
Create summary statistics from a subset of a population
}
\description{
Create summary statistics from a subset of a population; only needed if the sampling(counts) is conducted on a subset of the complete population.
}
\usage{
summaryPop(ind, incl)

}
\arguments{
  \item{ind}{
array with the population, usually the output from \code{\link{simPop}}.
}
  \item{incl}{
vector with the indices of the individuals that are to be included for the summary statistics.
}
}
\value{
A matrix with the numbers in each age class for each year.
}
\author{
M. Schaub
}

\examples{
# TODO
}
