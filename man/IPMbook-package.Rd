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

\section{Chapter 2}{
  \describe{
    \item{\code{\link{demoMCMC}}}{ Demonstration of a random-walk sampler for MCMC (section 2.5)}
  }
}

\section{Chapter 4}{
  \describe{
    \item{\code{\link{simMHB}}}{ Simulate detection and count data with spatial and temporal replication (section 4.3.3)}
    \item{\code{\link{createAge}}}{ Create an age matrix (section 4.5.1.1)}
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

\section{Chapter 12}{
  \describe{
    \item{\code{\link{marrayDead}}}{ Create m-array from dead-recovery data (section 12.x)}
  }
}


\section{DATA SETS}{
  \describe{
  \item{\code{\link{bats}}}{ Data for greater horseshoe bats, 1989 to 2017, chapter 13.}
  \item{\code{\link{bear}}}{ Data for black bears, 2007-2012, chapter 21.}
  \item{\code{\link{catbird}}}{ Data for catbirds from USA, 1992-2008, chapter 19.}
  \item{\code{\link{cormorant}}}{ Data for cormorants from Denmark, 1991-2004, chapter 18.}
  \item{\code{\link{elk}}}{ Data for elk cows from Northern Idaho, 1988 to 1993, chapter 17.}
  \item{\code{\link{grouse}}}{ Data for black grouse from Northern Italy, 1997-2016, chapter 15.}
  \item{\code{\link{hoopoe}}}{ Data for Hoopoe from Valais valley, 2001-2017, chapter 14.}
  \item{\code{\link{kestrel}}}{ Data for Kestrels in NW Switzerland, chapter 20.}
  \item{\code{\link{peregrine}}}{ Data for Peregrine Falcons, section 12.4.}
  \item{\code{\link{redbacked}}}{ Data for Redbacked Shrike, section 8.2.}
  \item{\code{\link{stork}}}{ Data for white storks, section 4.5.4.}
  \item{\code{\link{swallow}}}{ Data for barn swallows, Chapter 16.}
  \item{\code{\link{woodchat5}}}{ Simulated data for Woodchat Shrike for use in Chapter 5.}
  \item{\code{\link{woodchat6}}}{ Simulated data for Woodchat Shrike for use in Chapter 6.}
  \item{\code{\link{woodchat64}}}{ Simulated data for Woodchat Shrike for use in section 6.4.}
  \item{\code{\link{woodchat66}}}{ Simulated data for Woodchat Shrike for use in section 6.6.}
  \item{\code{\link{woodchat7}}}{ Simulated data for Woodchat Shrike for use in chapter 7.}
  \item{\code{\link{woodchat10}}}{ Simulated data for Woodchat Shrike for use in Chapter 10.}
  \item{\code{\link{woodchat11}}}{ Real data for Woodchat Shrike for use in Chapter 11.}
  \item{\code{\link{wryneck}}}{ Nest survival data for wryneck, section 4.4.6.}
  }
}

\section{UTILITY FUNCTIONS}{
  \describe{
  \item{\code{\link{getFirst}}}{ Calculates the occasion of first capture.}
  \item{\code{\link{rmFirst}}}{ Removes the records of first capture from a capture history matrix.}
  \item{\code{\link{zKnown}}}{ Converts a capture history matrix to one with 1 where the animal is know to be alive, NA elsewhere.}
  \item{\code{\link{zInit}}}{ Converts a capture history matrix to one with 1 after the occasion of first capture, NA elsewhere.}
  \item{\code{\link{zInitDR}}}{ Converts a dead-recovery capture history matrix to one with 1 after the occasion of first capture until the occasion of dead recovery, then 0; NA elsewhere.}
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
