\name{simMHB}
\alias{simMHB}
\encoding{UTF-8}

\title{
Simulates detection and count data with spatial and temporal replication
}
\description{
Function simulates MHB (Monitoring häufige Brutvögel) lookalike data. MHB is the Swiss breeding bird survey that is the source of many classic data sets (e.g. Kéry & Royle, 2016, 2021). This survey was launched in 1999 and for a total of 267 1km2 quadrats laid out in an approximate grid over Switzerland. 2 or 3 surveys are conducted in each breeding season (mid-April to early July) on a quadrat-specific, constant route averaging 4-6 km and all birds detected are mapped, thus yielding replicated counts of unmarked individuals.

The data are simulated under the assumptions of a binomial N-mixture model where for \code{lambda} we can specify a log-linear trend over the years and we can account for site-level random effects in both the intercept and the slopes of the log-linear model.

For detection probability we have currently a constant average or a logit-linear trend over the years, with no further heterogeneity.
}
\usage{
simMHB(nsites = 267, nsurveys = 3, nyears = 25,
    mean.lam = 1, mean.beta = 0.03, sd.lam = c(0.5, 0.05),
    mean.p = 0.6, beta.p = 0.1,
    show.plot = TRUE)
}
\arguments{
  \item{nsites}{
number of sites included in the survey.
}
  \item{nsurveys}{
number of replicate surveys at each site in each year.
}
  \item{nyears}{
number of years.
}
  \item{mean.lam}{
intercept of expected abundance.
}
  \item{mean.beta}{
average slope of log(lambda) on year.
}
  \item{sd.lam}{
a length 2 vector, the SDs of the Normal distribution from which random site effects for the intercept and the slope in the log-linear model for lambda are drawn randomly.
}
  \item{mean.p}{
value of constant detection probability per survey (or intercept of the logit-linear model for p).
}
  \item{beta.p}{
slope of the logit(p) in year.
}
  \item{show.plot}{
if TRUE, the output will be displayed graphically.
}
}
\value{
A list with the arguments used and the following components:

 \item{alpha }{vector with intercept used for the log-linear model for lambda for each site.}
 \item{beta }{vector with slope used for the log-linear model for lambda for each site.}
 \item{lam }{sites x years matrix with the expected number of animals at each site.}
 \item{N }{sites x years matrix  with the realized number of animals at each site.}
 \item{totalN }{vector with the total number of animals in each year.}
 \item{p }{vector with the probability of detection in each year.}
 \item{C }{sites x surveys x years array with the counts of animals detected.}
}
\author{
Marc Kéry
}
\references{
Kéry, M., Royle, A. (2016) \emph{Applied Hierarchical Modeling in Ecology} Vol 1, Academic Press.

Kéry, M., Royle, A. (2021) \emph{Applied Hierarchical Modeling in Ecology} Vol 2, Academic Press.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 4.3.3.
}

\examples{
# Explicit default values for all function arguments
str(dat <- simMHB(nsites=267, nsurveys=3, nyears=25, mean.lam=1,
    mean.beta=0.03, sd.lam=c(0.5, 0.05), mean.p=0.6, beta.p=0.1,
    show.plot=TRUE))
str(dat <- simMHB())                # Same, implicit
str(dat <- simMHB(nsites=1000))     # More sites
str(dat <- simMHB(nsurveys=10))     # More surveys
str(dat <- simMHB(nyears=50))       # More years
str(dat <- simMHB(mean.lam=5))      # Higher mean abundance
str(dat <- simMHB(mean.beta=-0.03)) # Population declines
str(dat <- simMHB(sd.lam=c(0, 0)))  # No site variability in lambda
str(dat <- simMHB(mean.p=1))        # Perfect detection
str(dat <- simMHB(mean.p=0.6, beta.p=0)) # Constant p = 0.6
str(dat <- simMHB(mean.p=0.6, beta.p=-0.2)) # Declining p
str(dat <- simMHB(show.plot=FALSE)) # No plots (when used in simulations)
}
