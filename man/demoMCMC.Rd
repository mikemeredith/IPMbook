\name{demoMCMC}
\alias{demoMCMC}
\encoding{UTF-8}

\title{
Demonstration of a random-walk sampler for MCMC
}
\description{
Implements a random-walk sampler for a single parameter: the probability of success on the logit scale, theta = logit(p), for binomial data with y successes in N trials. The prior for theta is a Normal distribution with mean mu.theta and SD s.theta. Proposal values are drawn from a Normal distribution with mean = current value and SD = prop.s.
}
\usage{
demoMCMC(y = 20, N = 50, niter = 25000, mu.theta = 0, s.theta = 100, prop.s = 1, init = 0)
}
\arguments{
  \item{y}{
number of successes.
}
  \item{N}{
number of trials.
}
  \item{niter}{
the number of MCMC draws required.
}
  \item{mu.theta}{
the mean of the Normal prior distribution.
}
  \item{s.theta}{
the SD of the Normal prior distribution.
}
  \item{prop.s}{
the SD of the Normal distribution for the proposals.
}
  \item{init}{
initial value for theta.
}
}
\value{
A list with the values of the input arguments and the following additional elements:
 \item{theta }{a vector with the MCMC draws}
 \item{acc.prob }{the proportion of proposed values accepted}
}
\author{
Adapted from code in Ntzoufras (2009) \emph{Bayesian Modeling Using WinBUGS} p. 48.
}

\examples{
require(graphics)
# Default settings
str(tmp <- demoMCMC(y = 20, N = 50, niter = 25000,
  mu.theta = 0, s.theta = 100, prop.s = 1, init = 0))

# Show convergence
str(tmp <- demoMCMC(prop.s = 0.1, init = 10))

# Show convergence: no convergence
str(tmp <- demoMCMC(prop.s = 0.1, init = 100))

# Very, very small step size: very inefficient
str(tmp <- demoMCMC(prop.s = 0.01))

# Very small step size: inefficient
str(tmp <- demoMCMC(prop.s = 0.1))

# Default step size: pretty good for this case
str(tmp <- demoMCMC(prop.s = 1))

# Larger than default step size: efficiency goes down again
str(tmp <- demoMCMC(prop.s = 10))

# Muuuuch larger ..... brrrrr !
str(tmp <- demoMCMC(prop.s = 100))
}
