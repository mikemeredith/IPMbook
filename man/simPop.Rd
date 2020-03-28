\name{simPop}
\alias{simPop}
\alias{simPop2}  %%% temporary entry
\encoding{UTF-8}

\title{
Simulate a population based on demographic parameters
}
\description{
The simulation tracks individual females from birth through a series of adult age classes to death, as well as numbers of female and male offspring.
The function allows for 1 or more age classes.
Once an animal reaches the oldest age class, they do not progress further until death.
The user specifies the number of females in each age class at the beginning of the simulation, before the first breeding season. Adult females can enter the population as immigrants, and are treated as being in the oldest age class.

Fecundity (the total number of offspring per adult female per year) can vary by age class and year in an age x year matrix. The sex ratio determines the proportion of females among the offspring.

Probability of survival can also vary by age class and interval between years. The first row of this matrix specifies the survival of the animals born at the beginning of each interval and the remaining rows control survival for the respective adult age classes.

User specification of the fecundity and survival matrices is simplified when values do not change over time or are the same for all age classes.
}
\usage{
simPop(Ni = c(10, 10), phi = c(0.3, 0.55), f = 3.2,
  sex.ratio = 0.5, Im = 0, nYears = 6)
simPop2(Ni = c(10, 10), phi = c(0.3, 0.55), f = 3.2,
  sex.ratio = 0.5, Im = 0, nYears = 6)
}
\arguments{
  \item{Ni}{
vector with number of females of each adult age class in the population prior to breeding in the first year. The length of this vector (minimum 2) determines the number of age classes. The first age class refers to the individuals born the previous year.
}
  \item{phi}{
survival probabilities for new-borns and each age class for each interval between surveys: The full (age classes + 1) x (years - 1) matrix can be entered, or (1) a matrix with just 2 rows if all adults have the same survival probability, or (2) a vector (length between 2 and number of age classes +1) if survival does not change with time, or (3) scalar, if all survival probabilities are equal.
}
  \item{f}{
fecundity rates, the number of offspring (male + female) per female in the population, for each adult age class and each year: The full (age classes x years) matrix can be entered, or (1) a vector of length equal to number of age classes if fecundity does not change with time, or (2) scalar, if all fecundity rates are equal. %%% no option for fewer rows? %%%
}
  \item{sex.ratio}{
sex ratio (probability that a new-born is a female) in each year: either scalar or a vector of length nYears.
}
  \item{Im}{
number of immigrants in each year: either scalar or a vector of length = number of years.
}
  \item{nYears}{
number of years of data; ignored if either \code{phi} or \code{f} is a matrix.
}
}
\value{
This may change in future versions!

For \code{simPop2}, a list with the simulation settings and the following components:
 \item{state }{an individuals x years matrix, with the state of each individual in each year. States are coded: 0 = dead, 1 = newborn, 2 = immigrant, 3 = 1-Year, etc}
 \item{s.reprod }{an individuals x years x sex array with the number of female and male offspring for each individual; NAs indicate individuals not alive.}
 \item{N }{a matrix with a column for each year and rows for number of females born, number of surviving immigrants, numbers in each age class, total adults and total births.}

}
\author{
M. Schaub
}

\examples{
# Simulate a population with the default arguments:
str(tmp <- simPop2())
tmp$N
tmp$phi
tmp$f
}
