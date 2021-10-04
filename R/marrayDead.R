
# Define function to create an m-array based for mark-recovery (MR) data
# Modified to deal with a single individual as a vector 2020-10-15
# Modified to deal with a matrix of unique capture histories plus a vector of frequencies 2021-09-28.

marrayDead <- function(MR, freq = 1, groups = NULL){
  if(is.data.frame(MR))
    MR <- as.matrix(MR)
  MR <- round(MR)
  stopifNegative(MR, allowNA=TRUE, allowZero=TRUE)
  if(!is.matrix(MR))
    MR <- matrix(MR, nrow=1)
  if(any(rowSums(MR) > 2))
    stop("The rows of MR may not have more that two 1's", call.=FALSE)
  if(is.data.frame(freq))
    freq <- as.matrix(freq)
  if(length(freq) == 1)
    freq <- rep(freq, nrow(MR))
  if(!is.matrix(freq))
    freq <- matrix(freq, ncol=1)
  freq <- round(freq)
  if(!is.null(groups) && ncol(freq) == 1)
    freq <- groupfreq(freq, groups)

  absfreq <- abs(freq)

  nind <- nrow(MR)
  n.occasions <- ncol(MR)
  ng <- ncol(freq)                    # number of groups, can be 1

  # Create empty m-array, add dimnames
  out <- array(0, dim=c(n.occasions-1, n.occasions, ng))
  
  gNames <- colnames(freq)
  if(is.null(gNames))
    gNames <- paste0("G", 1:ng)
  dimnames(out) <- list(released = paste0("Y", 1:(n.occasions-1)),
      recovered = c(paste0("Y", 2:n.occasions), "never"),
      gNames)

  # Create vector with occasion of marking
  f <- getFirst(MR)  # year of release
  f.fact <- factor(f, levels=1:n.occasions)
  for(g in 1:ng) {
    # Calculate the number of released individuals at each time period
    released <- tapply(freq[,g], f.fact, sum)
    released[is.na(released)] <- 0  # tapply returns NA if a value is missing from f.

    # Fill m-array with recovered individuals
    rec.ind <- which(apply(MR, 1, sum)==2)  # which were recovered dead
    rec <- getFirst(rmFirst(MR[rec.ind, ])) # year of recovery
    for (i in seq_along(rec.ind)){
      out[f[rec.ind[i]],rec[i]-1, g] <- out[f[rec.ind[i]],rec[i]-1, g] + abs(freq[rec.ind[i], g])
    }
    # Calculate the number of individuals that are never recovered
    out[ ,n.occasions, g] <- released[-n.occasions] - rowSums(out[,,g])
  }
  return(drop(out))
}

