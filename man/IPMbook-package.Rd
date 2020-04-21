\name{IPMbook-package}
\alias{IPMbook-package}
\alias{IPMbook}
\docType{package}
\encoding{UTF-8}

\title{
Functions and data for the Book 'Integrated Population Modeling'
}
\description{
Provides functions and data sets needed to run the code given in the book \emph{Integrated Population Modeling} by Michael Schaub and Marc Kéry, Academic Press (in preparation).
}
\details{


The functions are listed by chapter below.
}

\section{Chapter X}{
  \describe{
    \item{\code{\link{demoMCMC}}}{ Demonstration of a random-walk sampler for MCMC (section x.x)}
  }
}

\section{Chapter 5}{
  \describe{
    \item{\code{\link{simPop}}}{ Simulate a population based on demographic parameters (section x.x)}
    \item{\code{\link{simCapHist}}}{ Simulate capture histories from a population (section x.x)}
    \item{\code{\link{simProd}}}{ Simulate productivity data (section x.x)}
    \item{\code{\link{simCount}}}{ Simulate population survey data (section x.x)}
    \item{\code{\link{marray}}}{ Converts capture-histories to an m-array for one age class (section x.x)}
    \item{\code{\link{marrayAge}}}{ Create age-dependent m-arrays (section x.x)}
  }
}


\section{DATA SETS}{
  \describe{
  \item{\code{\link{hoopoe}}}{ Data for Hoopoe from Valais valley, 2001-2017.}
  \item{\code{\link{woodchat5}}}{ Simulated data for Woodchat Shrike for use in Chapter 5.}
  \item{\code{\link{woodchat10}}}{ Simulated data for Woodchat Shrike for use in Chapter 10.}
  \item{\code{\link{woodchat11}}}{ Real data for Woodchat Shrike for use in Chapter 11.}

  }
}

\section{UTILITY FUNCTIONS}{
  \describe{
  \item{\code{\link{getFirst}}}{ Calculates the occasion of first capture.}
  \item{\code{\link{rmFirst}}}{ Removes the records of first capture from a capture history matrix.}
  \item{\code{\link{cleanCH}}}{ Removes all-zero rows from a capture history matrix.}
  \item{\code{\link{dbeta2}}}{ Beta distribution with mean and sd.}
  \item{\code{\link{dbeta3}}}{ Beta distribution with mode and concentration.}
  \item{\code{\link{dgamma2}}}{ Gamma distribution with mean and sd.}
  }
}

\author{
Michael Schaub, Marc Kéry, Mike Meredith
}

\keyword{package}
