
# cleanCH
# zKnown
# zInit
# rmFirst
# getFirst
## dUnif - now has its own file


# Function to remove histories without any capture from a capture-recapture matrix
# Input variables
#    ch: matrix with capture histories.
# Written: 14.3.2016, M.Schaub

cleanCH <- function(ch){
  incl <- which(rowSums(ch)>=1)
  ch <- ch[incl,]
  return(ch)
}
# ......................................................

# From a capture history matrix, creates a matrix with 1 where the individual
#  is known to be alive, NA elsewhere.
# Input variables
#    ch: matrix with capture histories.

zKnown <- function(ch) {
  zknown1 <- function(x) {
    kn <- which(x > 0)
    x[min(kn):max(kn)] <- 1
    x[x == 0] <- NA
    return(x)
  }
  return(t(apply(ch, 1, zknown1)))
}
# .................................................................

# From a capture history matrix, creates a matrix with 1 after the occasion
#   of first capture, NA elsewhere.
# Input variables
#    ch: matrix with capture histories.

zInit  <- function(ch){
  f <- getFirst(ch) # occasion of first capture
  zInit <- array(NA, dim = dim(ch))
  for(i in 1:nrow(ch)){
    if(f[i] >= ncol(ch)) # first captured on last occasion (or never!)
      next
    zInit[i,(f[i]+1):ncol(ch)] <- 1
  }
  return(zInit)
}


# Function to remove the first capture in a capture-recapture matrix
# Input variables
#    ch: matrix with capture histories.
# Written: 14.3.2016, M.Schaub
# Modified to handle vectors, 2020-10-15
# Depends on function 'getFirst'

rmFirst <- function(x){
  if(is.array(x)) {
    index <- cbind(1:nrow(x), getFirst(x))
    x[index] <- 0
  } else {
    x[getFirst(x)] <- 0
  }
  return(x)
}
# .............................................................................

# Function to calculate the occasion of first capture
# Written: 2011, BPA
# Modified to deal with all-zero rows, 2020-10-15

getFirst <- function(x) {
  extract <- function(u) {
    tmp <- which(u > 0)
    ifelse(length(tmp) == 0, NA, min(tmp))
  }
  if(is.matrix(x)) {
    return(apply(x, 1, extract))
  }
  extract(x)
}
