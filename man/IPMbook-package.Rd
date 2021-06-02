\name{IPMbook-package}
\alias{IPMbook-package}
\alias{IPMbook}
\docType{package}
\encoding{UTF-8}

\title{
Functions and data for the Book 'Integrated Population Models'
}
\description{
Provides functions and data sets to accompany the  book \emph{Integrated Population Models} by Michael Schaub and Marc Kéry (ISBN: 9780128205648).
}
\details{


The functions are listed by chapter below.
}

\section{Chapter 2}{
  \describe{
    \item{\code{\link{demoMCMC}}}{ Demonstration of a random-walk sampler for MCMC (Section 2.5)}
  }
}

\section{Chapter 4}{
  \describe{
    \item{\code{\link{simMHB}}}{ Simulates detection and count data with spatial and temporal replication (Section 4.3.3)}
    \item{\code{\link{createAge}}}{ Creates an age matrix (Section 4.5.1.1)}
    \item{\code{\link{marray}}}{ Converts single- or multistate capture histories to an m-array for one age class (Section 4.5.1.2)}
    \item{\code{\link{marrayAge}}}{ Converts single-state to age-dependent m-arrays (Section 4.5.1.2)}
  }
}

\section{Chapter 5}{
  \describe{
    \item{\code{\link{simPop}}}{ Simulates a population based on demographic parameters (Section 5.5.1)}
    \item{\code{\link{simCapHist}}}{ Simulates capture histories from a population (Section 5.5.1)}
    \item{\code{\link{simProd}}}{ Simulates productivity data for a population(Section 5.5.1)}
    \item{\code{\link{simCount}}}{ Simulates population survey data for a population(Section 5.5.1)}
  }
}

\section{Chapter 12}{
  \describe{
    \item{\code{\link{marrayDead}}}{ Converts dead-recovery capture histories to an m-array (Section 12.4.2)}
  }
}


\section{DATA SETS}{
  \describe{
  \item{\code{\link{bats}}}{ Data for greater horseshoe bats from Switzerland, 1989-2017, Chapter 13.}
  \item{\code{\link{bear}}}{ Data for black bears from USA, 2007-2012, Chapter 21.}
  \item{\code{\link{catbird}}}{ Data for catbirds from USA, 1992-2008, Chapter 19.}
  \item{\code{\link{cormorant}}}{ Data for cormorants from Denmark, 1991-2004, Chapter 18.}
  \item{\code{\link{elk}}}{ Data for elk cows from USA, 1988-1993, Chapter 17.}
  \item{\code{\link{grouse}}}{ Data for black grouse from Italy, 1997-2016, Chapter 15.}
  \item{\code{\link{hoopoe}}}{ Data for hoopoe from Switzerland, 2001-2017, Chapter 14.}
  \item{\code{\link{kestrel}}}{ Data for kestrels from Switzerland, 2002-2016, Chapter 20.}
  \item{\code{\link{peregrine}}}{ Data for peregrine falcons from France and Switzerland, 1965-2007, Chapter 12.}
  \item{\code{\link{redbacked}}}{ Data for redbacked shrike from Germany, 1971-2006, Section 8.2.}
  \item{\code{\link{stork}}}{ Data for white storks from Germany, 1986-2001, Section 4.5.4.}
  \item{\code{\link{swallow}}}{ Data for barn swallow from Switzerland, 1997-2003, Chapter 16.}
  \item{\code{\link{woodchat5}}}{ Simulated data for woodchat shrike for use in Chapter 5.}
  \item{\code{\link{woodchat6}}}{ Simulated data for woodchat shrike for use in Chapter 6.}
  \item{\code{\link{woodchat64}}}{ Simulated data for woodchat shrike for use in Section 6.4.}
  \item{\code{\link{woodchat66}}}{ Simulated data for woodchat shrike for use in Section 6.6.}
  \item{\code{\link{woodchat7}}}{ Simulated data for woodchat shrike for use in Chapter 7.}
  \item{\code{\link{woodchat10}}}{ Simulated data for woodchat shrike for use in Chapter 10.}
  \item{\code{\link{woodchat11}}}{ Real data for woodchat shrike, Chapter 11.}
  \item{\code{\link{wryneck}}}{ Nest survival data for wryneck from Switzerland, 2002-2006, Section 4.4.6.}
  }
}

\section{UTILITY FUNCTIONS}{
  \describe{
  \item{\code{\link{getFirst}}}{ Calculates the occasion of first capture from a capture history matrix.}
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
