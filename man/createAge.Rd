\name{createAge}
\alias{createAge}
\encoding{UTF-8}

\title{
Create an age matrix
}
\description{
Creates an animals x years matrix which indicates the age class in each year for each individual, based on the occasion of first capture and the age at first capture.
}
\usage{
createAge(f, age, nyears, mAge=2)
}
\arguments{
  \item{f}{
A vector of first capture occasions.
}
  \item{age}{
A corresponding vector with the age at first capture of each animal.
}
  \item{nyears}{
The number of years to be covered by the matrix, ie, the number of columns.
}
  \item{mAge}{
The maximum age class; age is incremented by 1 each year up to \code{mAge}.
}

}
\value{
An individuals x years matrix with the age of each individual in each year. Cells prior to the first capture are filled with NAs.
}
\author{
M. Schaub.
}

\examples{
# TODO
}
