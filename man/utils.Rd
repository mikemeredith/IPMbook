\name{Helper functions}
\alias{cleanCH}
\alias{zKnown}
\alias{zInit}
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

\code{zKnown} takes a capture history and returns a matching matrix with 1 where the individual is know to be alive (ie, between the first and last capture) and NA elsewhere.

\code{zInit} takes a capture history and returns a matching matrix with 1 \emph{after} the occasion of first capture and NA elsewhere.

\code{getFirst} calculates the occasion of first capture in a capture history matrix.

\code{dUnif} creates a vector to be used with the categorical distribution in BUGS to generate a discrete uniform prior.
}
\usage{
cleanCH(ch)
zKnown(ch)
zInit(ch)
rmFirst(ch)
getFirst(x)
dUnif(A, B)
}
\arguments{
  \item{ch}{
An individuals x time matrix with capture histories.
}
  \item{x}{
An individuals x time matrix with capture histories, or a vector of capture/noncapture data, typically 1 row of a capture history.
}
  \item{A, B}{
Range of the discrete uniform prior.
}

}
\value{
\code{cleanCH}, \code{zKnown}, \code{zInit} and \code{rmFirst} return the modified capture history matrix.

\code{getFirst} returns the occasion of first capture.

\code{dUnif} returns a vector... ??
}
\author{
M. Schaub, Marc Kéry, Mike Meredith
}

\examples{
# TODO
}
