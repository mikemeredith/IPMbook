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
marrayAge(ch, age, mAge = 1)
}
\arguments{
  \item{ch}{
an individuals x time matrix with capture histories (0: not captured; 1: captured).
}
  \item{age}{
vector with the age class at first capture for each individual.
}
  \item{mAge}{
maximum number of age classes for which m-arrays are constructed; ignored if \code{max(age) > mAge}. Only required if the \code{age} vector has fewer age classes than we want to separate (e.g. capture histories \code{ch} contains only individuals marked as juveniles, and we want 2 age classes).
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
