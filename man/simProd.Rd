\name{simProd}
\alias{simProd}
\encoding{UTF-8}

\title{
Simulate productivity data
}
\description{
Simulate a survey of breeding females, where a proportion of the females are included each year and the number of offspring (male and female) recorded.
}
\usage{
simProd(reprod, pInclude, females.only = FALSE)
}
\arguments{
  \item{reprod}{
array with the reproductive performance of the population, usually the output from \code{\link{simPop}}
}
  \item{pInclude}{
vector with the annual detection probabilities of broods; for \code{simProd2}, it can be scalar, used for all years.
}
  \item{females.only}{
if TRUE only the number of females that are produced is recorded. Default: total reproduction is recorded.
}
}
\value{
A list with the following 2 components:

 \item{prod.ind }{matrix with the individual reproductive output. The three columns give the output, the year of the brood and the age of the mother.}
 \item{prod.agg }{matrix with the same data, but aggregated. The two columns give the year-specific total number of newborn and the year-specific number of surveyed broods.}
}
\author{
M. Schaub
}

\examples{
# Generate a population
pop <- simPop()
prod <- simProd(pop$reprod)
str(prod)
}
