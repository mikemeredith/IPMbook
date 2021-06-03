
#####################################################################################################
#
# Function to create an m-array for a single- or multistate capture-recapture data with one age class
#
# Input variables
#    ch: matrix with single- or multistate capture histories (0: not captured; 1..X: captured in the 1..X states)
#    unobs: number of unobserved states (default is 0, needs to be given only in specific cases)
#
# Output
#    out: single- or multistate m-array. The last column of each m-array is the number of released individuals that were never recaptured. Thus, the total number of released individuals per occasion and state is the row sum of the m-array.
#
#    Depends on functions 'getFirst'
#
# Written: 22.9.2016, M.Schaub
#
# Last up-date:
#
#####################################################################################################

marray <- function(ch, unobs = 0){
  ch <- round(ch)
  stopifNegative(ch, allowNA=FALSE, allowZero=TRUE)
  unobs <- round(unobs)
  stopifNegative(unobs, allowNA=FALSE, allowZero=TRUE)

  if(!is.matrix(ch))
    ch <- matrix(ch, nrow=1)
  ns <- length(table(ch)) - 1 + unobs # number of states, excluding 0
  no <- ncol(ch)                      # number of observations (replicates, years, surveys, ...)

  # Remove capture histories of individuals that are marked at last occasion
  first <- getFirst(ch)
  last <- which(first==no)
  if (length(last) > 0)
    ch <- ch[-last,]

  # Create empty m-array, add dimnames
  out <- matrix(0, ncol = ns*(no-1)+1, nrow = ns*(no-1))
  if(ns == 1) {
    dimnames(out) <- list(released = paste0("Y", 1:(no-1)),
        recaptured = c(paste0("Y", 2:no), "never"))
  } else {
    YStmp <- expand.grid(paste0("S", 1:ns), paste0("Y", 1:no))
    Y.S <- paste(YStmp[,2], YStmp[,1], sep=".")
    dimnames(out) <- list(released = Y.S[1:(ns*(no-1))],
        recaptured = c(Y.S[-(1:ns)], "never"))
  }

  # Insert values in m-array
  for (i in 1:nrow(ch)){
    cap.occ <- which(ch[i,]!=0)
    state <- ch[i,cap.occ]
    if (length(state) == 1) {
       out[state[1]+ns*(cap.occ[1]-1), ns*(no-1)+1] <- out[state[1]+ns*(cap.occ[1]-1), ns*(no-1)+1] + 1
    }
    if (length(state) > 1) {
      for (t in 2:length(cap.occ)){
        out[(cap.occ[t-1]-1)*ns+state[t-1], (cap.occ[t]-2)*ns+state[t]] <- out[(cap.occ[t-1]-1)*ns+state[t-1], (cap.occ[t]-2)*ns+state[t]] + 1
      } # t
      if (max(cap.occ) < no){
        out[(cap.occ[t]-1)*ns+state[t], ns*(no-1)+1] <- out[(cap.occ[t]-1)*ns+state[t], ns*(no-1)+1] + 1
      } # if
    } # if
  } # t  ?? i
  return(out)
}

