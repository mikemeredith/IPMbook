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
marrayAge(ch, age = 1, mAge = 1, freq = 1, groups = NULL)
}
\arguments{
  \item{ch}{
an individuals x time matrix with capture histories (0: not captured; 1: captured). See Details.
}
  \item{age}{
vector with the age class at first capture for each individual, or a scalar that will be used for all individuals.
}
  \item{mAge}{
maximum number of age classes for which m-arrays are constructed; ignored if \code{max(age) > mAge}. Only required if the \code{age} vector has fewer age classes than we want to separate (e.g. capture histories \code{ch} contains only individuals marked as juveniles, and we want 2 age classes).
}
  \item{freq}{
a vector with the number of animals with each capture history, or a matrix with a column for each group. If a single value is supplied, it will be used for all rows in the capture history; the default is to assume each row corresponds to a single animal.
}
  \item{groups}{
a factor (or a vector that can be coerced to a factor) which identifies the group that each row of \code{ch} refers to. Ignored if \code{freq} is a matrix with > 1 column.
}
}

\details{
The argument \code{ch} can be a matrix of unique capture histories accompanied by a vector or matrix, \code{freq}, specifying the number of animals with each capture history. Trap losses can be indicated either by negative values for \code{freq}, or by filling the row with NA after the last capture. For other formats, see \code{\link{ch2matrix}}.
}

\value{
A 4-d array, (years-1) x years x age classes x groups, where element [i, j, k, g] contains the number of individuals in group g of age class k released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). If no groups are specified, this will be a 3-d array, (years-1) x years x age classes. The last column contains the number of individuals released in year i and never recaptured.
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
