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
simProd(reprod, pInclude = 0.3, females.only = FALSE, verbose = TRUE)
}
\arguments{
  \item{reprod}{
array with the reproductive performance of the population, usually the output from \code{\link{simPop}}
}
  \item{pInclude}{
vector with the annual detection probabilities of broods; it can be scalar, used for all years.
}
  \item{females.only}{
if TRUE only the number of females that are produced is recorded. Default: total reproduction is recorded.
}
  \item{verbose}{
if TRUE, information is displayed in the console.
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
\references{
Schaub & Kéry (2021) Integrated Population Modeling, section 5.5.1.
}

\examples{
# Generate a population
pop <- simPop()
prod <- simProd(pop$reprod)
str(prod)
}
