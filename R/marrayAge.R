
#########################################
#
# Function to create age-dependent m-arrays from single-state capture-recapture data
#
# Input variables
#    ch: matrix with capture histories.
#        Note: the capture history file is a single file that includes the individuals of all age classes
#    age: vector with the age class at first capture for each individual
#    mAge: maximal number of age classes for which m-arrays are constructed. Input is optional and only required if the age matrix has fewer age classes as we want to separate (e.g. CH contains only individuals marked as juveniles, and we want 2 age classes)
#
# Output
#    marr: 3-d array with the m-array. The third dimension is the age class. The last column of each m-array is the number of released individuals that were never recaptured. Thus, the total number of released individuals per occasion is the row sum of the m-array.
#
#    Depends on functions 'marray', 'cleanCH' and 'getFirst'
#
#
# Written: 14.3.2016, M.Schaub
#
# Last up-date: 17.03.2020
#
################################################


marrayAge <- function(ch, age, mAge = 1){
  ch <- round(ch)
  stopifNegative(ch, allowNA=FALSE, allowZero=TRUE)
  age <- round(age)
  stopifNegative(age, allowNA=FALSE, allowZero=FALSE)
  mAge <- round(mAge[1])
  stopifNegative(mAge, allowNA=FALSE, allowZero=FALSE)

  if (!is.matrix(ch))
    ch <- matrix(ch, nrow = 1)
  maxAge <- max(age, mAge)
  nind <- nrow(ch)
  stopifnotLength(age, nind, allow1=FALSE)
  n.occasions <- ncol(ch)
  first <- getFirst(ch)
  age.matrix <- matrix(0, ncol = n.occasions, nrow = nind)
  for (i in 1:nind){
    age.matrix[i,first[i]:n.occasions] <- 1:(n.occasions-first[i]+1)+(age[i]-1)
  }
  age.matrix[age.matrix > maxAge] <- maxAge

  ch.rec <- ch
  for (i in 1:nind){
    h <- which(ch.rec[i,]==1)
    for (j in 1:length(h)){
      ch.rec[i,h[j]] <- j
    } # j
  } # i
  ch.rec[ch.rec > maxAge] <- maxAge

  ch.split <- array(0, dim = c(nrow(ch), ncol(ch), maxAge))
  for (a in 1:maxAge){
    for (i in 1:nind){
      j <- which(ch.rec[i,]==a | ch.rec[i,]==(a+1))
      if (length(j)==0)
        next
      ch.split[i,j[1:2],age.matrix[i,j[1]]] <- 1
      if (length(j)>1){
        ch.split[i,j[2:length(j)],age.matrix[i,j[2]]] <- 1
      }
    } # i
  } # a

  marr <- array(0, dim = c(n.occasions-1, n.occasions, maxAge))
  dimnames(marr) <- list(released = paste0("Y", 1:(n.occasions-1)),
        recaptured = c(paste0("Y", 2:n.occasions), "never"),
        age = 1:maxAge)
  for (a in 1:(maxAge-1)){
    for (i in 1:nind){
      u <- which(ch.split[i,,a]==1)
      if (length(u)==0)
        next
      if (u[1]==n.occasions)
        next
      if (length(u)==1)
        marr[u,n.occasions,a] <- marr[u,n.occasions,a] + 1
      if (length(u)==2)
        marr[u[1], u[2]-1, a] <- marr[u[1], u[2]-1, a] + 1
    } # i
  } # a

  a <- maxAge
  if(sum(ch.split[,,a])) # all zeros, leave the zeros in marr[,,a]
     marr[,,a] <- marray(cleanCH(ch.split[,,a]))

  return(marr)
}

