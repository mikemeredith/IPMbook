
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

simSurveyBin <- function(N, psur){

  T <- length(N)
  SUR <- numeric()
  for (t in 1:T){
    SUR[t] <- rbinom(1, N[t], psur[t])
  } # t
  return(SUR)
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

  T <- length(N)
  SUR <- numeric(T)
  for (t in 1:T){
    SUR[t] <- rnorm(1, N[t], sigma[t])
  } # t
  return(SUR)
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

  T <- length(N)
  SUR <- numeric(T)
  for (t in 1:T){
    SUR[t] <- rpois(1, N[t])
  } # t
  return(SUR)
}

