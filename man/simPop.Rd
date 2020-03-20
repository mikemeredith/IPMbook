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
simPop(Ni = c(10, 10),
  phi = matrix(c(rep(0.3, 5), rep(0.55, 5)), ncol = 5, byrow = TRUE),
  f = matrix(c(rep(3.2, 6), rep(3.2, 6)), ncol = 6, byrow = TRUE),
  sex.ratio = rep(0.5, 6), Im = rep(0, 6))
}
\arguments{
  \item{Ni}{
vector with age(stage)-specific population size in the first year [length of vector: number of age(stage) classes, but without immigrants]. Note that the first age(stage) class refers to the 1-year old individuals, since the model is a pre-breeding model. The number of age(stage) classes must correspond to the number of age(stage) classes in the matrix of the survival probabilities.
}
  \item{phi}{
matrix with age(stage)- and time-specific survival probabilities [dimension of matrix: number of age(stage) classes x number of years-1]. The rows correspond to the age(stage) classes (first row are the newborns), the columns to the years. Note that the number of age(stage) classes specified here will correspond to the number of age(stage) classes of the simulated population. Moreover, the number of colums defines the number of years.
}
  \item{f}{
matrix with the age(stage)- and time-specific fecundity rates [dimension of matrix: number of age(stage) classes x number of years]. The first row corresponds to the fecundity of individuals in the first age(stage) class.
}
  \item{sex.ratio}{
vector with the sex ratio (probability that a young is a female) in each year.
}
  \item{Im}{
vector with the number of immigrants in each year [length of vector: number of years].
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
