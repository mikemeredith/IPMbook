
#####################################################################################################
#
# Function to create an m-array for a single- or multistate capture-recapture data with one age class
#
# Input variables
#    ch: matrix with single- or multistate capture histories (0: not captured; 1..X: captured in the 1..X states)
#    New 2021-09-27:
#      ch can include NA for occasions after a trap loss.
#      ch can include only unique capture histories, with the number of animals with each history given in
#        the vector 'freq'.
#    unobs: number of unobserved states (default is 0, needs to be given only in specific cases)
#
# Output
#    out: single- or multistate m-array. The last column of each m-array is the number of released individuals that were never recaptured. Thus, the total number of released individuals per occasion and state is the row sum of the m-array.
#
#    Depends on functions 'getFirst'
#
# Written: 22.9.2016, M.Schaub, desecrated by Mike Meredith 2021-09-27
#
#####################################################################################################

marray <- function(ch, unobs = 0, freq = 1, groups = NULL){
  if(is.data.frame(ch))
    ch <- as.matrix(ch)
  if(!is.matrix(ch))
    ch <- matrix(ch, nrow=1)
  ch <- round(ch)
  stopifNegative(ch, allowNA=TRUE, allowZero=TRUE)
  unobs <- round(unobs)
  stopifNegative(unobs, allowNA=FALSE, allowZero=TRUE)
  if(is.data.frame(freq))
    freq <- as.matrix(freq)
  if(length(freq) == 1)
    freq <- rep(freq, nrow(ch))
  if(!is.matrix(freq))
    freq <- matrix(freq, ncol=1)
  freq <- round(freq)
  if(!is.null(groups) && ncol(freq) == 1)
    freq <- groupfreq(freq, groups)
  
  absfreq <- abs(freq)

  ns <- length(table(ch)) - 1 + unobs # number of states, excluding 0, can be 1
  no <- ncol(ch)                      # number of observations (replicates, years, surveys, ...)
  ng <- ncol(freq)                    # number of groups, can be 1

  # Check for trap losses
  traploss <- freq < 0
  if(all(traploss == FALSE))
    traploss <- matrix(is.na(rowSums(ch)), nrow=nrow(ch), ncol=ng)

  # Create empty m-array, add dimnames
  out <- array(0, dim = c(ns*(no-1), ns*(no-1)+1, ng))

  gNames <- colnames(freq)
  if(is.null(gNames))
    gNames <- paste0("G", 1:ng)
  if(ns == 1) {
    dimnames(out) <- list(released = paste0("Y", 1:(no-1)),
        recaptured = c(paste0("Y", 2:no), "never"), group=gNames)
  } else {
    YStmp <- expand.grid(paste0("S", 1:ns), paste0("Y", 1:no))
    Y.S <- paste(YStmp[,2], YStmp[,1], sep=".")
    dimnames(out) <- list(released = Y.S[1:(ns*(no-1))],
        recaptured = c(Y.S[-(1:ns)], "never"), group=gNames)
  }

  # Insert values in m-array
  first <- getFirst(ch)
  for (i in 1:nrow(ch)){
    if(first[i] == no)   # Skip cases where first capture is last occasion
      next
    cap.occ <- which(ch[i,]!=0)
    state <- ch[i,cap.occ]
    if(ns == 1) {
      capID <- paste0("Y", cap.occ)
    } else {
      capID <- paste0("Y", cap.occ, ".S", state)
    }

    for(g in 1:ng) {
      if(absfreq[i, g] == 0)
        next
      if (length(state) == 1 && !traploss[i, g]) {  # no recaptures
         out[capID[1], 'never', g] <- out[capID[1], 'never', g] + absfreq[i, g]
      }
      if (length(state) > 1) {  # recaptured at least once
        for (t in 2:length(cap.occ)){
          out[capID[t-1], capID[t], g] <- out[capID[t-1], capID[t], g] + absfreq[i, g]
        }
        if (max(cap.occ) < no && !traploss[i, g]){  # never recaptured after last release
          out[capID[t], 'never', g] <- out[capID[t], 'never', g] + absfreq[i, g]
        }
      }
    }
  }

  return(drop(out))
}

