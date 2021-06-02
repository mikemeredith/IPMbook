\name{GammaDist}
\alias{GammaDist}
\alias{dgamma2}
\alias{pgamma2}
\alias{qgamma2}
\alias{rgamma2}
\alias{getGammaPar}
\encoding{UTF-8}

\title{
The gamma distribution
}
\description{
Density, distribution function, quantile function and random number generation for the gamma distribution with parameters \code{mean} and \code{sd}. These are wrappers for \code{stats::dgamma}, etc. \code{getGammaPar} returns the shape and rate parameters.
}
\usage{
dgamma2(x, mean, sd)
pgamma2(q, mean, sd, lower.tail=TRUE, log.p=FALSE)
qgamma2(p, mean, sd, lower.tail=TRUE, log.p=FALSE)
rgamma2(n, mean, sd)
getGammaPar(mean, sd)
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
mean of the gamma distribution.
}
  \item{sd}{
standard deviation of the gamma distribution.
}
  \item{lower.tail}{
logical; if TRUE (default), cumulative probabilities up to x, otherwise, above x.
}
  \item{log.p}{
logical; if TRUE, probabilities p are given as log(p).
}
}
\value{
\code{dgamma2} gives the density, \code{pgamma2} gives the distribution function, \code{qgamma2} gives the quantile function, and \code{rgamma2} generates random deviates.

\code{getGammaPar} returns a 2-column matrix with the shape and rate parameters corresponding to \code{mean} and \code{sd}. }
\author{
Mike Meredith
}

\seealso{
See the \pkg{stats} functions \code{\link{dgamma}}, \code{\link{pgamma}}, \code{\link{qgamma}}, \code{\link{rgamma}}.
}
\examples{
# Plot some curves with dgamma2
xx <- seq(0, 20, length.out=101)
plot(xx, dgamma2(xx, 5, 1), xlab="x", ylab="Probability density",
  main="Gamma curves with mean = 5", type='l', lwd=2)
lines(xx, dgamma2(xx, 5, 2), col='darkgreen', lwd=2)
lines(xx, dgamma2(xx, 5, 4), col='red', lwd=2)
lines(xx, dgamma2(xx, 5, 8), col='blue', lwd=2)
abline(v=5, lty=3, lwd=2)
legend('topright', paste("sd =", c(1,2,4,8)), lwd=2,
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# Cumulative plots with pgamma2
plot(xx, pgamma2(xx, 5, 1), xlab="x", ylab="Cumulative probability",
  main="Gamma curves with mean = 5", type='l', lwd=2)
lines(xx, pgamma2(xx, 5, 2), col='darkgreen', lwd=2)
lines(xx, pgamma2(xx, 5, 4), col='red', lwd=2)
lines(xx, pgamma2(xx, 5, 8), col='blue', lwd=2)
abline(v=5, lty=3, lwd=2)
legend('bottomright', paste("sd =", c(1,2,4,8)), lwd=2,
  col=c('black', 'darkgreen', 'red', 'blue'), bty='n')

# Generate random draws and plot a histogram
rnd <- rgamma2(1e5, 5, 2)
hist(rnd, freq=FALSE)
# Add the curve:
lines(xx, dgamma2(xx, 5, 2), col='darkgreen', lwd=2)

# Get shape and rate parameters for mean = 5 and sd = c(1,2,4,8)
getGammaPar(mean = 5, sd = c(1,2,4,8))

}
\keyword{"distribution"}