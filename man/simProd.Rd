\name{simProd}
\alias{simProd}
\encoding{UTF-8}

\title{
Simulate productivity data
}
\description{
Simulate ...

}
\usage{
simProd(ind, prep, females.only = 'no')
}
\arguments{
  \item{ind}{
array with the population, usually the output from \code{\link{simPop}}
}
  \item{prep}{
vector with the annual detection probabilities of broods.
}
  \item{females.only}{
if 'yes' only the number of females that are produced is recorded. Default: total reproduction is recorded.
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
# TODO
}
