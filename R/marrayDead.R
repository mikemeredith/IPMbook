
# Define function to create an m-array based for mark-recovery (MR) data
# Modified to deal with a single individual as a vector 2020-10-15

marrayDead <- function(MR){
  if(!is.matrix(MR))
    MR <- matrix(MR, nrow=1)
  nind <- nrow(MR)
  n.occasions <- ncol(MR)
  out <- matrix(0, ncol=n.occasions, nrow=n.occasions-1)

  # Create vector with occasion of marking
  f <- getFirst(MR)  # year of release
  # Calculate the number of released individuals at each time period
  released <- tabulate(f, nbins=n.occasions) # 'tabulate' keeps the zeros

  # Fill m-array with recovered individuals
  rec.ind <- which(apply(MR, 1, sum)==2)  # which were recovered dead
  rec <- getFirst(rmFirst(MR[rec.ind, ])) # year of recovery
  for (i in seq_along(rec.ind)){
    out[f[rec.ind[i]],rec[i]-1] <- out[f[rec.ind[i]],rec[i]-1] + 1
  }
  # Calculate the number of individuals that are never recovered
  out[ ,n.occasions] <- released[-n.occasions] - rowSums(out)

  return(out)
}

