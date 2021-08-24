\name{marrayDead}
\alias{marrayDead}
\encoding{UTF-8}

\title{
Converts dead-recovery capture histories to an m-array
}
\description{
Converts dead-recovery capture histories to an m-array.
}
\usage{
marrayDead(MR)
}
\arguments{
  \item{MR}{
an individuals x time matrix with 1 denoting either the time of marking or the time of recovery; otherwise 0.
}
}
\value{
An m-array, a (years-1) x years matrix, where element [i, j] contains the number of individuals marked in year i and recovered in year j+1. The last column contains the number of individuals marked in year i and never recovered.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 12.4.2.
}

\examples{
data(peregrine)
str(peregrine$recoveries)  # 1810 animals x 43 years
dr <- cleanCH(peregrine$recoveries[, 29:43])  # Use last 15 years of data
marrayDead(dr)
}
