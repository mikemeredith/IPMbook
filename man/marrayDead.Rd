\name{marrayDead}
\alias{marrayDead}
\encoding{UTF-8}

\title{
Converts mark-recovery histories for dead recoveries to an m-array
}
\description{
Create an m-array for mark-recovery data for use in a Cormack-Jolly-Seber (CJS) model.
}
\usage{
marrayDead(MR)
}
\arguments{
  \item{MR}{
An individuals x time matrix with 1 denoting either the time of marking or the time of recovery; otherwise 0.
}
}
\value{
An m-array, a (years-1) x years matrix, where element [i, j] contains the number of individuals marked in year i and recovered in year j+1. The last column contains the number of individuals marked in year i and never recovered.
}
\author{
M. Schaub.
}

\examples{
# TODO
}
