\name{summaryPop}
\alias{summaryPop}
\encoding{UTF-8}

\title{
Calculates summary statistics from a subset of a population
}
\description{
Calculates summary statistics from a subset of a population; used if a count survey is to be simulated with a subset of the complete population.
}
\usage{
summaryPop(state, subset)

}
\arguments{
  \item{state}{
individuals x years matrix for the population with ages of adult individuals plus newborns (coded 0) and died (-1), usually the output from \code{\link{simPop}}.
}
  \item{subset}{
vector with the indices of the individuals that are to be included for the summary statistics; if missing, all individuals are included.
}
}
\value{
A matrix with the numbers in each adult age class for each year, total adults, and number  of female juveniles.
}
\author{
Michael Schaub
}

\examples{
pop <- simPop()
summaryPop(pop$state) # default is to include all, compare with
pop$N

# define a subset of 80 drawn at random
sset <- sample(nrow(pop$state), 80)
summaryPop(pop$state, subset=sset)
}
