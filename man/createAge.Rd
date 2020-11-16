\name{createAge}
\alias{createAge}
\encoding{UTF-8}

\title{
Create an age matrix
}
\description{
Creates an animals x intervals matrix which gives the age class for each individual at the beginning of each interval, based on the occasion of first capture and the age at first capture.
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
The number of years of the study; the number of intervals is \code{nyears - 1}.
}
  \item{mAge}{
The maximum age class; age is incremented by 1 each year up to \code{mAge}.
}

}
\value{
An individuals x intervals matrix with the age of each individual at the beginning of each interval. Cells prior to the first capture are filled with NAs.
}
\author{
M. Schaub.
}

\examples{
# TODO
}