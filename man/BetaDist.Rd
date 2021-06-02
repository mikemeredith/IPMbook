\name{BetaDist}
\alias{BetaDist}
\alias{dbeta2}
\alias{pbeta2}
\alias{qbeta2}
\alias{rbeta2}
\alias{getBeta2Par}
\alias{dbeta3}
\alias{pbeta3}
\alias{qbeta3}
\alias{rbeta3}
\alias{getBeta3Par}
\encoding{UTF-8}

\title{
The beta distribution
}
\description{
Density, distribution function, quantile function and random number generation for the beta distribution with parameters \code{mean} and \code{sd} OR \code{mode} and \code{concentration}. These are wrappers for \code{stats::dbeta}, etc. \code{getBeta*Par} returns the shape parameters.
}
\usage{
dbeta2(x, mean, sd)
pbeta2(q, mean, sd, lower.tail=TRUE, log.p=FALSE)
qbeta2(p, mean, sd, lower.tail=TRUE, log.p=FALSE)
rbeta2(n, mean, sd)
getBeta2Par(mean, sd)

dbeta3(x, mode, concentration)
pbeta3(q, mode, concentration, lower.tail=TRUE, log.p=FALSE)
qbeta3(p, mode, concentration, lower.tail=TRUE, log.p=FALSE)
rbeta3(n, mode, concentration)
getBeta3Par(mode, concentration)
}
\arguments{
  \item{x}{
vector of parameter values.
}
  \item{q}{
vector of quantiles.
}
  \item{p}{
vector of probabilities.
}
  \item{n}{
number of random draws required.
}
  \item{mean}{
mean of the beta distribution; cannot be 0 or 1.
}
  \item{sd}{
standard deviation of the beta distribution; this must be less than \code{sqrt(mean * (1-mean))}, larger values will return NA, with a warning.
}
  \item{mode}{
mode of the beta distribution; may be 0 or 1.
}
  \item{concentration}{
concentration of the beta distribution; concentration = 2 is uniform, and the distribution becomes narrower as concentration increases. It is sometimes referred to as 'sample size', but best thought of as sample size + 2.
}
  \item{lower.tail}{
logical; if TRUE (default), cumulative probabilities up to x, otherwise, above x.
}
  \item{log.p}{
logical; if TRUE, probabilities p are given as log(p).
}
}
\value{
\code{dbeta*} gives the density, \code{pbeta*} gives the distribution function, \code{qbeta*} gives the quantile function, and \code{rbeta*} generates random deviates.

\code{getBeta*Par} returns a 2-column matrix with the shape parameters corresponding to \code{mean} and \code{sd} OR \code{mode} and \code{concentration}.
}
\author{
Mike Meredith
}

\seealso{
See the \pkg{stats} functions \code{\link{dbeta}}, \code{\link{pbeta}}, \code{\link{qbeta}}, \code{\link{rbeta}}.
}
\examples{
# Plot some curves with dbeta2
xx <- seq(0, 1, length.out=101)
plot(xx, dbeta2(xx, 0.4, 0.12), xlab="x", ylab="Probability density",
  main="Beta curves with mean = 0.4", type='l', lwd=2)
lines(xx, dbeta2(xx, 0.4, 0.24), col='darkgreen', lwd=2)
lines(xx, dbeta2(xx, 0.4, 0.28), col='red', lwd=2)
lines(xx, dbeta2(xx, 0.4, 0.36), col='blue', lwd=2)
abline(v=0.4, lty=3, lwd=2)
legend('topright', paste("sd =", c(0.12,0.24, 0.28, 0.36)), lwd=2,
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# Get shape and rate parameters for mean = 0.4 and sd = c(0.12,0.24, 0.28, 0.36, 0.49)
# The last value for sd is too big and will produce NAs and a warning
getBeta2Par(mean = 0.4, sd = c(0.12,0.24, 0.28, 0.36, 0.49))

# The parameterisation with mean and sd doesn't seem intuitive,
#   let's try mode and concentration.
# This does not allow 'bathtub' curves, which are bimodal.
plot(xx, dbeta3(xx, 0.4, 16), xlab="x", ylab="Probability density",
  main="Beta curves with mode = 0.4", type='l', lwd=2)
lines(xx, dbeta3(xx, 0.4, 8), col='darkgreen', lwd=2)
lines(xx, dbeta3(xx, 0.4, 4), col='red', lwd=2)
lines(xx, dbeta3(xx, 0.4, 2), col='blue', lwd=2)
abline(v=0.4, lty=3, lwd=2)
legend('topright', , lwd=2, paste("concentration =", c(16, 8, 4, 2)),
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# The mode can be at 0 or 1:
plot(xx, dbeta3(xx, 1, 16), xlab="x", ylab="Probability density",
  main="Beta curves with mode = 1", type='l', lwd=2)
lines(xx, dbeta3(xx, 1, 8), col='darkgreen', lwd=2)
lines(xx, dbeta3(xx, 1, 4), col='red', lwd=2)
lines(xx, dbeta3(xx, 1, 2), col='blue', lwd=2)
legend('topleft', paste("concentration =", c(16, 8, 4, 2)), lwd=2,
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# Cumulative plots with pbeta3
plot(xx, pbeta3(xx, 0.4, 16), xlab="x", ylab="Cumulative probability",
  main="Beta curves with mode = 0.4", type='l', lwd=2)
lines(xx, pbeta3(xx, 0.4, 8), col='darkgreen', lwd=2)
lines(xx, pbeta3(xx, 0.4, 4), col='red', lwd=2)
lines(xx, pbeta3(xx, 0.4, 2), col='blue', lwd=2)
abline(v=0.4, lty=3, lwd=2)
legend('topleft', paste("concentration =", c(16, 8, 4, 2)), lwd=2,
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# Generate random draws and plot a histogram
rnd <- rbeta3(1e5, 0.4, 8)
hist(rnd, freq=FALSE)
# Add the curve:
lines(xx, dbeta3(xx, 0.4, 8), col='darkgreen', lwd=2)

# Get shape and rate parameters for mode = 0.4 and concentration = c(2, 4, 8, 16)
getBeta3Par(mode = 0.4, concentration = c(2, 4, 8, 16))

}
\keyword{"distribution"}