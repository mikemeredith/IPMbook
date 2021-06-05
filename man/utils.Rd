\name{Helper functions}
\alias{cleanCH}
\alias{zKnown}
\alias{zInit}
\alias{zInitDR}
\alias{rmFirst}
\alias{getFirst}
\encoding{UTF-8}

\title{
Various helper functions
}
\description{
\code{cleanCH} removes histories without any capture (ie, all-zero rows) from a capture-recapture matrix.

\code{rmFirst} removes the first capture in each row of a capture history matrix, replacing it with zero.

\code{zKnown} takes a capture history and returns a matching matrix with 1 where the individual is know to be alive (ie, between the first and last capture) and NA elsewhere.

\code{zInit} takes a capture history and returns a matching matrix with 1 \emph{after} the occasion of first capture and NA elsewhere.

\code{zInitDR} takes a dead-recovery capture history and returns a matching matrix with 1 \emph{after} the occasion of first capture until the recovery occasion and 0 subsequently; NA elsewhere.

\code{getFirst} calculates the occasion of first capture in a capture history matrix.

}
\usage{
cleanCH(ch)
zKnown(ch)
zInit(ch)
zInitDR(chDR)
rmFirst(x)
getFirst(x)
}
\arguments{
  \item{ch}{
an individuals x time matrix with capture histories.
}
  \item{chDR}{
an individuals x time matrix with dead recovery capture histories: 1 indicates initial capture or dead recovery, 0 otherwise.
}
  \item{x}{
an individuals x time matrix with capture histories, or a vector of capture/noncapture data, typically 1 row of a capture history.
}

}
\value{
\code{cleanCH}, \code{zKnown}, \code{zInit}, \code{zInitDR} and \code{rmFirst} return the modified capture history matrix.

\code{getFirst} returns the occasion of first capture, NA if no capture recorded for an individual.

}
\author{
Michael Schaub, Marc Kéry, Mike Meredith
}
