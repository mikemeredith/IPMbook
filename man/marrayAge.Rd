\name{marrayAge}
\alias{marrayAge}
\encoding{UTF-8}

\title{
Create age-dependent m-arrays
}
\description{
Convert single-state capture-recapture data to age-dependent m-arrays.
}
\usage{
marrayAge(ch, age, mAge = 1)
}
\arguments{
  \item{ch}{
An individuals x time matrix with capture histories (0: not captured; 1: captured).
}
  \item{age}{
Vector with the age class at first capture for each individual.
}
  \item{mAge}{
maximal number of age classes for which m-arrays are constructed. Input is optional and only required if the age vector has fewer age classes than we want to separate (e.g. CH contains only individuals marked as juveniles, and we want 2 age classes).
}

}
\value{
A 3-d array, (years-1) x years x age classes, where element [i, j, k] contains the number of individuals of age class k released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). The last column contains the number of individuals released in year i and never recaptured.
}
\author{
M. Schaub.
}

\examples{
# TODO
}
