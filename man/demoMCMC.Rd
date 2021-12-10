\name{demoMCMC}
\alias{demoMCMC}
\encoding{UTF-8}

\title{
Demonstration of a random-walk sampler for a single parameter
}
\description{
Generates an MCMC chain for the probability of success on the logit scale, \code{ltheta = logit(theta)}, for binomial data with y successes in N trials. The prior for \code{ltheta} is a Normal distribution with mean \code{mu.ltheta} and SD \code{sd.ltheta}. Proposal values are drawn from a Normal distribution with mean = current value and SD = prop.sd.
}
\usage{
demoMCMC(y = 20, N = 50, niter = 25000, mu.ltheta = 0, sd.ltheta = 100,
    prop.sd = 1, init = 0, quiet = FALSE, show.plots = TRUE)
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
  \item{mu.ltheta}{
the mean of the Normal prior distribution for logit(theta).
}
  \item{sd.ltheta}{
the SD of the Normal prior distribution for logit(theta).
}
  \item{prop.sd}{
the SD of the Normal distribution for the proposals.
}
  \item{init}{
initial value for logit(theta).
}
  \item{quiet}{
if TRUE, console output is suppressed.
}
  \item{show.plots}{
if FALSE, plotting is suppressed.
}
}
\value{
A list with the values of the input arguments and the following additional elements:
 \item{ltheta }{a vector with the MCMC draws.}
 \item{acc.prob }{the proportion of proposed values accepted.}
}
\author{
Adapted from code in Ntzoufras (2009), p. 48.
}
\references{
Ntzoufras, I. (2009) \emph{Bayesian Modeling Using WinBUGS}, Wiley, Hoboken NJ.

Schaub, M., Kéry, M. (2022), \emph{Integrated Population Models}, Academic Press, section 2.5.
}
\examples{
require(graphics)
# Default settings
str(tmp <- demoMCMC(y = 20, N = 50, niter = 25000,
  mu.ltheta = 0, sd.ltheta = 100, prop.sd = 1, init = 0))

# Show convergence
str(tmp <- demoMCMC(prop.sd = 0.1, init = 10))

# Show convergence: no convergence
str(tmp <- demoMCMC(prop.sd = 0.1, init = 100))

# Very, very small step size: very inefficient
str(tmp <- demoMCMC(prop.sd = 0.01))

# Very small step size: inefficient
str(tmp <- demoMCMC(prop.sd = 0.1))

# Default step size: pretty good for this case
str(tmp <- demoMCMC(prop.sd = 1))

# Larger than default step size: efficiency goes down again
str(tmp <- demoMCMC(prop.sd = 10))

# Muuuuch larger ..... brrrrr !
str(tmp <- demoMCMC(prop.sd = 100))
}
