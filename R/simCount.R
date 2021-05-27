
# simCountBin
# simCountNorm
# simCountPois

#########################################
#
# Function to simulate population survey data assuming a binomial sampling process
#
# It is assumed that all individuals have the same probability to be counted
#
# Input variables
#    N: Annual number of individuals at risk of detection (usually population size)
#    psur: vector with the annual detection probabilities of the individuals at risk of counting
#
#
# Last-up date: 9.6.2016, M.Schaub
#
################################################

simCountBin <- function(N, pDetect){
  N <- round(N)
  stopifNegative(N, allowNA=FALSE, allowZero=TRUE)
  nYears <- length(N)
  stopifnotProbability(pDetect, allowNA=FALSE)
  pDetect <- fixAvector(pDetect, nYears)
  count <- rbinom(nYears, N, pDetect)
  return(list(pDetect = pDetect, count = count))
}

#########################################
#
# Function to simulate population survey data assuming a Normal sampling process
#
# It is assumed that individuals may be double counted and missed at the same rate
#
# Input variables
#    N: Annual number of individuals at risk of detection (usually population size)
#    sigma: vector with the annual observation error (SD)
#
#
# Last-up date: 9.6.2016, 26.11.2019, M.Schaub
#
################################################

simCountNorm <- function(N, sigma){
  N <- round(N)
  stopifNegative(N, allowNA=FALSE, allowZero=TRUE)
  nYears <- length(N)
  stopifNegative(sigma, allowNA=FALSE, allowZero=TRUE)
  sigma <- fixAvector(sigma, nYears)
  SUR <- round(rnorm(nYears, N, sigma))
  SUR <- pmax(0, SUR)
  return(list(sigma = sigma, count = SUR))
}

#########################################
#
# Function to simulate population survey data assuming a Poisson sampling process
#
# It is assumed that individuals may be double counted and missed at the same rate
#
# Input variables
#    N: Annual number of individuals at risk of detection (usually population size)
#
#
# Last-up date: 6.12.2019, M.Schaub
#
################################################

simCountPois <- function(N){
  N <- round(N)
  stopifNegative(N, allowNA=FALSE, allowZero=TRUE)
  count <- rpois(length(N), N)
  return(list(count = count))
}

