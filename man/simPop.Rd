\name{simPop}
\alias{simPop}
\encoding{UTF-8}

\title{
Simulates a population based on demographic parameters
}
\description{
The simulation tracks individual females from birth through a series of adult age classes to death, as well as numbers of female and male offspring. The function allows for 1 or more age classes. Once an animal reaches the oldest age class, age does not increase further until death.

The user specifies the number of females in each age class at the beginning of the simulation, before the first breeding season. Adult females can enter the population as immigrants; the age of immigrants is controlled by \code{ageOfImm}.

Fecundity (the total number of offspring per adult female per year) can vary by age class and year in an age x year matrix. The sex ratio determines the proportion of females among the offspring.

In some age classes, not all animals may attempt to breed, and the probability of breeding can be specified.

Probability of survival can also vary by age class and year. The first row of the survival matrix specifies the survival of the animals born in each year and the remaining rows control survival for the respective older age classes.
}
\usage{
simPop(Ni = c(10, 10), phi = c(0.3, 0.55), f = 3.2, nYears = 6, pBreed = 1,
  sex.ratio = 0.5, Im = 0, ageOfIm = 1)
}
\arguments{
  \item{Ni}{
vector with number of females of each adult age class in the population prior to breeding in the first year. The length of this vector determines the number of age classes. The first age class refers to the individuals born the previous year.
}
  \item{phi}{
survival probabilities for new-borns and each age class for each interval between surveys: a (age classes + 1) x (years - 1) matrix. Input can be \link{abbreviated}.
}
  \item{f}{
fecundity rates, the number of offspring (male + female) per female in the population, for each adult age class and each year: an age classes x years matrix. Input can be \link{abbreviated}.
}
  \item{nYears}{
number of years the population is simulated.
}
  \item{pBreed}{
the probability that a female will attempt to breed for each adult age class and each year: an age classes x years matrix. Input can be \link{abbreviated}.
}
  \item{sex.ratio}{
sex ratio (probability that a new-born is a female) in each year: either scalar or a vector of length \code{nYears}.
}
  \item{Im}{
number of female immigrants in each year: either scalar or a vector of length \code{nYears}.
}
  \item{ageOfIm}{
the age class of immigrants in each year: either scalar or a vector of length \code{nYears}.
}
}
\value{

A list with the simulation settings and the following components:
 \item{state }{an individuals x years matrix, with the state of each individual in each year. States are coded: -1 = died, 0 = newborn, 1,2,... = 1-Year, 2-Year,...}
 \item{imYear }{a vector with the year of immigration for each individual, or NA if not an immigrant.}
 \item{reprod }{an individuals x years x 3 array with the number of female and male offspring and the age of the mother for each individual; NAs indicate individuals not alive or alive but not attempting to breeding.}
 \item{N }{a matrix with a column for each year and rows for numbers surviving in each age class, total adults, number of females born and total births, number of immigrants. (Immigrants are included in the age classes at the top of the table.)}
 \item{breeders }{a matrix with a column for each year and rows for numbers breeding in each age class and total breeders. Note that all animals attempting to breed are classified as breeders, even though no offspring may be produced.}
 \item{totAdults }{a vector with total adults in each year.}
 \item{totBreeders }{a vector with total breeders in each year.}

 The last two are the same as the relevant rows in \code{N} and \code{breeders}, repeated for ease of extraction with \code{$}.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 5.5.1.
}

\examples{
# Simulate a population with the default arguments:
str(pop <- simPop())
pop$N
# Try View, but not available on all platforms
# View(pop$state)
pop$phi
pop$f
pop$reprod[1:5,,]

# Just one (adult) age group:
str(pop <- simPop(Ni=20))
pop$N
pop$phi
pop$f

# Using matrices for time-dependent survival, fecundity
phi <- matrix(c(
  0.3, 0.5, 0.5, 0.2, 0.2,  # newborns
  0.5, 0.7, 0.7, 0.4, 0.4), # adults
  nrow=2, byrow=TRUE)
f <- matrix(c(3, 4, 5, 2, 2, 3), nrow=1)
str(pop <- simPop(phi=phi, f=f))
pop$phi
pop$f    # Matrices expanded to all adult age classes
pop$N

# With immigration
str(pop <- simPop(Im=5))
pop$N
# with ageOfIm = 1 (default) immigrants are included in 1-Year figures.

# Not all 1-Years attempt to breed, and fecundity is lower
str(pop <- simPop(f = c(2, 3.2), pBreed = c(0.7, 1)))
pop$f
pop$pBreed
# Note the difference between numbers of breeders and adults among 1-Years
pop$breeders
pop$N

# Extinction
set.seed(1)
str(pop <- simPop(f = 0.1, phi = c(0.1, 0.2)))
pop$N

# Sink population, rescued by immigration
set.seed(1)
str(pop <- simPop(f = 0.1, phi = c(0.1, 0.2), Im=5))
pop$N
}
