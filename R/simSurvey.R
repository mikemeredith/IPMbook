
# simSurveyBin
# simSurveyNorm
# simSurveyPois

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

simSurveyBin <- function(N, pDetect){

  nYears <- length(N)
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

simSurveyNorm <- function(N, sigma){

  nYears <- length(N)
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

simSurveyPois <- function(N){
  count <- rpois(length(N), N)
  return(list(count = count))
}

