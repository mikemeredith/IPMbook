
# cleanCH
# rmFirst
# getFirst
# dUnif


#####################################################################################################
#
# Function to remove histories without any capture from a capture-recapture matrix
#
# Input variables
#    ch: matrix with capture histories.
#
# Written: 14.3.2016, M.Schaub
#
# Last up-date:  18.3.2020
#
#####################################################################################################

cleanCH <- function(ch){
  incl <- which(rowSums(ch)>=1)
  ch <- ch[incl,]
  return(ch)
}


#####################################################################################################
#
# Function to the first capture in a capture-recapture matrix
#
# Input variables
#    ch: matrix with capture histories.
#
# Written: 14.3.2016, M.Schaub
#
# Depends on function 'getFirst'
#
# Last up-date: 18.3.2020
#
#####################################################################################################

rmFirst <- function(ch){
  first <- apply(ch, 1, getFirst)
  for (i in 1:nrow(ch)){
    ch[i,first[i]] <- 0
  }
  return(ch)
}


#####################################################################################################
#
# Function to calculate the occasion of first capture
#
# Written: 2011, BPA
#
# Last up-date: 18.3.2020
#
#####################################################################################################

getFirst <- function(x)
  min(which(x==1))


#####################################################################################################
#
# Function to create a vector to be used with the categorical distribution in BUGS to generate a discrete uniform prior
#
# Input variables
#    A, B: range of the discrete uniform prior
#
# Written: September 2014, Michael Schaub
#
# Last up-date: 6.12.2019
#
#####################################################################################################

dUnif <- function(A, B){
  pprob <- c(rep(0, A-1), rep(1/(B-A+1), (B-A+1)))
  return(pprob)
}
