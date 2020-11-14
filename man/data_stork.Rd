\name{stork}
\alias{stork}
\encoding{UTF-8}
\docType{data}

\title{
Data on marked white storks in Eastern Germany
}
\description{
Data for white stork (\emph{Ciconia ciconia}) during 16 years (1986-2001) in Eastern Germany. Chicks were ringed and adults resighted when they raised a brood in the study area. Nineteen ringed storks were recovered dead. The data set only includes birds resighted at least once as > 2 year olds.
}

\usage{data("stork")}

\format{
\code{stork} is a matrix with rows for 691 marked birds and columns for the years 1986 to 2001. 1 indicates resighting, 2 a dead recovery, otherwise 0.
}
\source{Vogelwarte Hiddensee, Germany}

\references{
Schaub, M., W. Kania, and U. Köppen. 2005. Variation of primary production during winter induces synchrony in survival rates in migratory white storks \emph{Ciconia ciconia}. \emph{Journal of Animal Ecology} 74:656–666.
}

\examples{
data(stork)
str(stork)
}
\keyword{datasets}
