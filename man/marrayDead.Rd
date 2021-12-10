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
marrayDead(MR, freq = 1, groups = NULL)
}
\arguments{
  \item{MR}{
an individuals x time matrix with 1 denoting either the time of marking or the time of recovery; otherwise 0.
}
  \item{freq}{
a vector with the number of animals with each capture history, or a matrix with a column for each group. If a single value is supplied, it will be used for all rows in the capture history; the default is to assume each row corresponds to a single animal.
}
  \item{groups}{
a factor (or a vector that can be coerced to a factor) which identifies the group that each row of \code{ch} refers to. Ignored if \code{freq} is a matrix with > 1 column.
}
}

\details{
The argument \code{MR} can be a matrix of unique capture histories accompanied by a vector or matrix, \code{freq}, specifying the number of animals with each capture history. For other formats, see \code{\link{ch2matrix}}.
}

\value{
An m-array, a (years-1) x years x groups array, where element [i, j, g] contains the number of individuals in group g marked in year i and recovered in year j+1. The last column contains the number of individuals marked in year i and never recovered. If no groups are specified, this will be a (years-1) x years matrix.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 12.4.3.
}

\examples{
data(peregrine)
str(peregrine$recoveries)  # 1810 animals x 43 years
dr <- cleanCH(peregrine$recoveries[, 29:43])  # Use last 15 years of data
marrayDead(dr)
}
