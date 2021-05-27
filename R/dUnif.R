
# Function to create a vector to be used with the categorical distribution
# in BUGS to generate a discrete uniform prior
#
# Input variables
#    A, B: range of the discrete uniform prior
#
# Written: September 2014, Michael Schaub
#
# Last up-date: 6.12.2019
#
##############################################################################

# dUnif0 <- function(A, B){
  # pprob <- c(rep(0, A-1), rep(1/(B-A+1), (B-A+1)))
  # return(pprob)
# }

# Proposed changes (by Mike):
# 1. Change arg names A, B to lower, upper (can be abbreviated).
# 2. Round input values
# 3. Vectorise: lower and/or upper can be vectors, in which case the function
#    returns a matrix with a row corresponding to each element in the input.

dUnif <- function(lower, upper){
  stopifnotEqualLength(lower, upper)
  A <- round(lower) ; B <- round(upper)
  nrow <- length(lower)
  out <- matrix(0, nrow=nrow, ncol=max(B))
  n <- B - A + 1  # number of non-zero values
  for(i in 1:nrow)
    out[i, A[i]:B[i]] <- rep(1/n[i], n[i])
  return(drop(out))
}

