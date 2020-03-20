\name{Helper functions}
\alias{cleanCH}
\alias{rmFirst}
\alias{getFirst}
\alias{dUnif}
\encoding{UTF-8}

\title{
Various helper functions.
}
\description{
\code{cleanCH} removes histories without any capture (ie, all-zero rows) from a capture-recapture matrix.

\code{rmFirst} removes the first capture in each row of a capture history matrix, replacing it with zero.

\code{getFirst} calculates the occasion of first capture in a row of a capture history matrix.

\code{dUnif} creates a vector to be used with the categorical distribution in BUGS to generate a discrete uniform prior.
}
\usage{
cleanCH(ch)
rmFirst(ch)
getFirst(x)
dUnif(A, B)
}
\arguments{
  \item{ch}{
An individuals x time matrix with capture histories.
}
  \item{x}{
A vector of capture/noncapture data, typically 1 row of a capture history.
}
  \item{A, B}{
Range of the discrete uniform prior.
}

}
\value{
\code{cleanCH} and \code{rmFirst} return the modified capture history matrix.

\code{getFirst} returns the occasion of first capture (scalar).

\code{dUnif} returns a vector... ??
}
\author{
M. Schaub.
}

\examples{
# TODO
}
