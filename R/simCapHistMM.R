
#####################################################################################################
#
# Function to simulate capture histories from the population (ind)
#
# Input variables
#    ind: array with the population
#    c: matrix with age- and time-specific capture probabilities (probability of first capture)
#    p: matrix with age- and time-specific REcapture probabilities
#    maxAge: maximal number of age classes that can be identified when the individuals are captured for the first time
#
# Output
#    ch: matrix with the capture histories
#    age: vector with the age class at first capture for each individual
#
# Written: 11.3.2016, M.Schaub
#
# Last up-date: 19.5.2017, 26.11.2019
#
#####################################################################################################


simCapHist <- function(ind, c, p, maxAge = 2){

  T <- dim(ind)[2]
  nind <- dim(ind)[3]
  nstage <- dim(ind)[1]
  aclasses <- nstage-3
  age <- first <- last <- numeric(nind)
  
  if(!is.matrix(c))
    c <- matrix(c, aclasses, T)
  if(!is.matrix(p))
    p <- matrix(p, aclasses, T-1)

  for (i in 1:nind){
    g <- which(!is.na(ind[1:(aclasses+1),,i]), arr.ind = TRUE)
    age[i] <- g[1,1]   # age when entering popn, mostly 1 except initial adults
    first[i] <- g[1,2] # year of entry to popn
    h <- which(ind[1:(aclasses+1),,i]==1, arr.ind = TRUE)
    last[i] <- max(h[,2]) # last year of presence prior to death or end of study
  } # i

  ch.true <- ch <- in.cap <- matrix(0, ncol = T, nrow = nind)
  for (i in 1:nind){
    ch.true[i,first[i]:last[i]] <- 1  # matrix of presence at any age, incl newborn
  } # i
  # Recode age
  age[age > maxAge] <- maxAge

  # Sampling
  # Expand c and p (to higher age classes)
  C <- matrix(0, ncol = T, nrow = max(c(maxAge, nrow(c))) + T)
  C[1:nrow(c),] <- c
  u <- max(c(maxAge, nrow(c))) + T - nrow(c)
  if (u > 0){
    for (j in 1:u){
      C[nrow(c)+j,] <- c[nrow(c),]
    } # j
  } # if

  P <- matrix(0, ncol = T-1, nrow = max(c(maxAge, nrow(p))) + T)
  P[1:nrow(p),] <- p
  u <- max(c(maxAge, nrow(p))) + T - nrow(p)
  if (u > 0){
    for (j in 1:u){
      P[nrow(p)+j,] <- p[nrow(p),]
    } # j
  } # if

  for (i in 1:nind){
    # First capture
    # When captured for the first time?
    for (t in first[i]:last[i]){
      in.cap[i,t] <- rbinom(1, 1, C[age[i]+t-first[i],t])
    }
    ch[i,] <- in.cap[i,]  # put in ALL the captures, even though only 1st is "1st capture"??
    # up-date the age at first capture
    if (sum(in.cap[i,]) >= 1){
      k <- min(which(in.cap[i,] == 1))  # year of 1st capture
      age[i] <- age[i] + k - first[i]
      if (age[i] > maxAge)
        age[i] <- maxAge
    }  # if
    if (sum(in.cap[i,]) > 1){
      h <- which(in.cap[i,] == 1)
      ch[i,h[2:length(h)]] <- 0   # take out the ones which aren't first captures. Doh!
    }
    if (first[i]==last[i])
      next

    if (sum(in.cap[i,])!=0){
      # Recapture (conditional on first capture)
      cap.occ <- min(which(in.cap[i,] == 1))
      if (cap.occ==last[i])
        next
      for (t in (cap.occ+1):last[i]){
        ch[i,t] <- rbinom(1, 1, P[age[i]+t-first[i],t-1])
      } # t
    } # if
  } # i

  # Remove individuals that have never been captured/marked
  incl <- which(rowSums(ch)>=1)
  ch <- ch[incl,]
  age <- age[incl]
  return(list(ch = ch, age = age))
}

