\name{marray}
\alias{marray}
\encoding{UTF-8}

\title{
Converts capture-histories to an m-array for one age class
}
\description{
Create an m-array for a single- or multistate capture-recapture data with one age class for use in a Cormack-Jolly-Seber (CJS) model.
}
\usage{
marray(ch, unobs = 0)
}
\arguments{
  \item{ch}{
An individuals x time matrix with single- or multistate capture histories (0: not captured; 1..X: captured in the 1..X states).
}
  \item{unobs}{
Number of unobserved states (default is 0, needs to be given only in specific cases).
}

}
\value{
An m-array, a (years-1) x years matrix, where element [i, j] contains the number of individuals released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). The last column contains the number of individuals released in year i and never recaptured.
}
\author{
M. Schaub.
}

\examples{
# TODO
}
