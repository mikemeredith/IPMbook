\name{Discrete uniform}
\alias{dUnif}
\encoding{UTF-8}

\title{
Discrete uniform prior distribution
}
\description{
\code{dUnif} creates a vector or matrix to be used with the categorical distribution, \code{dcat}, in JAGS to generate a discrete uniform prior.
}
\usage{
dUnif(lower, upper)
}
\arguments{
  \item{lower, upper}{
Range of the discrete uniform prior. Either scalars, or vectors of the same length. Values are rounded to integers.
}

}
\value{
If the arguments are scalar, a vector of length equal to \code{upper}, with the first \code{lower - 1} elements filled with zeros and remainder holding a constant value such that the vector sums to 1.

Otherwise a matrix with each row constructed as above: the columns from \code{lower} to \code{upper} have a constant value, other columns hold zeros. See the examples.
}
\author{
Michael Schaub, Marc Kéry, Mike Meredith
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 5.4.4.
}
\examples{
# Both arguments scalar, vector output
(p <-  dUnif(lower = 4, upper = 8))
#  0.0 0.0 0.0 0.2 0.2 0.2 0.2 0.2
sum(p)
# Non-integer arguments are rounded
(p <- dUnif(lower = 3.8, upper = 8.2))  # same as above

# Vector arguments, matrix output
(p <- dUnif(lower = c(4, 2, 5), upper = c(8,11,6)))
# [1,]    0  0.0  0.0  0.2  0.2  0.2  0.2  0.2  0.0   0.0   0.0
# [2,]    0  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1   0.1   0.1
# [3,]    0  0.0  0.0  0.0  0.5  0.5  0.0  0.0  0.0   0.0   0.0
rowSums(p) # all 1
}
