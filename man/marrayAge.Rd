\name{marrayAge}
\alias{marrayAge}
\encoding{UTF-8}

\title{
Creates age-dependent m-arrays
}
\description{
Converts single-state capture-recapture data to age-dependent m-arrays.
}
\usage{
marrayAge(ch, age, mAge = 1, freq = 1)
}
\arguments{
  \item{ch}{
an individuals x time matrix with capture histories (0: not captured; 1: captured). Alternativel, this can be a matrix of unique capture histories accompanied by a vector, \code{freq}, specifying the number of animals with each combination of capture history and age. Trap losses can be indicated either by negative values for \code{freq}, or by filling the row with NA after the last capture.
}
  \item{age}{
vector with the age class at first capture for each individual.
}
  \item{mAge}{
maximum number of age classes for which m-arrays are constructed; ignored if \code{max(age) > mAge}. Only required if the \code{age} vector has fewer age classes than we want to separate (e.g. capture histories \code{ch} contains only individuals marked as juveniles, and we want 2 age classes).
}
  \item{freq}{
a vector with the number of animals with each capture history. If a single value is supplied, it will be used for all rows in the capture history. The default is to assume each row corresponds to a single animal.
}

}
\value{
A 3-d array, (years-1) x years x age classes, where element [i, j, k] contains the number of individuals of age class k released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). The last column contains the number of individuals released in year i and never recaptured.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 4.5.1.2.
}

\examples{
data(woodchat5)
dim(woodchat5$ch)  # 1902 animals x 20 years
marrayAge(ch=woodchat5$ch, age=woodchat5$age, mAge=2)
}
