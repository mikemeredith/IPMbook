\name{stork}
\alias{stork}
\encoding{UTF-8}
\docType{data}

\title{
Data on marked white storks in Germany, 1986-2001
}
\description{
Data for white stork (\emph{Ciconia ciconia}) during 16 years (1986-2001) in eastern Germany. Chicks were ringed and adults resighted when they raised a brood in the study area. Nineteen ringed storks were recovered dead. The data set only includes birds resighted at least once as > 2 year olds.
}

\usage{data(stork)}

\format{
\code{stork} is a matrix with rows for 691 marked birds and columns for the years 1986 to 2001. 1 indicates ringing or resighting, 2 a dead recovery, otherwise 0.
}
\source{Vogelwarte Hiddensee, Germany}

\references{
Schaub, M., Kania, W., Köppen, U. (2005) Variation of primary production during winter induces synchrony in survival rates in migratory white storks \emph{Ciconia ciconia}. \emph{Journal of Animal Ecology} 74, 656-666.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 4.5.4.
}

\examples{
data(stork)
str(stork)
}
\keyword{datasets}
