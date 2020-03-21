\name{simPop}
\alias{simPop}
\encoding{UTF-8}

\title{
Simulate a population based on demographic parameters
}
\description{
Simulates a female-based population. Reproduction is the total number of young, and the young sex ratio is used to track only females in the population later. It includes demographic stochasticity.

The function allows for a flexible number of age classes.
}
\usage{
simPop(Ni = c(10, 10), phi = c(0.3, 0.55), f = 3.2, 
  sex.ratio = 0.5, Im = 0, nyears = 6)
}
\arguments{
  \item{Ni}{
vector with age(stage)-specific population size in the first year [length of vector: number of age(stage) classes, but without immigrants]. Note that the first age(stage) class refers to the 1-year old individuals, since the model is a pre-breeding model. The number of age(stage) classes must correspond to the number of age(stage) classes in the matrix of the survival probabilities.
}
  \item{phi}{
survival probabilities: can be (1) scalar, (2) a vector of length equal to Ni, or (3) a matrix with dimensions number of age(stage) classes x (number of years-1).
}
  \item{f}{
fecundity rates: can be (1) scalar, (2) a vector of length equal to Ni, or (3) a matrix with dimensions number of age(stage) classes x number of years.
}
  \item{sex.ratio}{
sex ratio (probability that a young is a female) in each year: either scalar or a vector of length nyears.
}
  \item{Im}{
number of immigrants in each year: either scalar or a vector of length = number of years].
}
  \item{nyears}{
number of years of data; ignored if either \code{phi} or \code{f} is a matrix.
}
}
\value{
A list with the following 2 components:

 \item{ind }{array with the life history of all individuals in the population
   - the first index of the array refers to the stage
   - the second index of the array refers to the time
   - the third index of the array refers to the individiual
   - Rep F are the number of fledged females, Rep M the number of fledged males}
 \item{N }{summary statistics: Juv F = total number of fledged females in a year, Juv T = total number of fledglings in a year}
}
\author{
M. Schaub
}

\examples{
# TODO
}
