
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
# Written: 14.3.2016, M.Schaub, desecrated by Mike Meredith 2021-09-27
#
################################################


marrayAge <- function(ch, age = 1, mAge = 1, freq = 1, groups = NULL){
  if(is.data.frame(ch))
    ch <- as.matrix(ch)
  if (!is.matrix(ch))
    ch <- matrix(ch, nrow = 1)
  ch <- round(ch)
  stopifNegative(ch, allowNA=TRUE, allowZero=TRUE)
  age <- round(age)
  stopifNegative(age, allowNA=FALSE, allowZero=FALSE)
  if(length(age) == 1)
    age <- rep(age, nrow(ch))
  mAge <- round(mAge[1])
  stopifNegative(mAge, allowNA=FALSE, allowZero=FALSE)
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

  maxAge <- max(age, mAge)
  nind <- nrow(ch)
  stopifnotLength(age, nind, allow1=FALSE)
  # stopifnotLength(freq, nind, allow1=FALSE)  ## FIXME
  n.occasions <- ncol(ch)
  ng <- ncol(freq)   # number of groups, can be 1

  # Remove capture histories of individuals that are marked at last occasion
  first <- getFirst(ch)
  # last <- which(first == n.occasions)
  # if (length(last) > 0) {
    # ch <- ch[-last,]
    # age <- age[-last]
    # freq <- freq[-last]
    # first <- getFirst(ch)
    # nind <- nrow(ch)
  # }
  absfreq <- abs(freq)

  # Check for trap losses
  traploss <- freq < 0
  if(all(traploss == FALSE))
    traploss <- matrix(is.na(rowSums(ch)), nrow=nrow(ch), ncol=ng)

  age.matrix <- matrix(0, ncol = n.occasions, nrow = nind)
  for (i in 1:nind){
    age.matrix[i,first[i]:n.occasions] <- 1:(n.occasions-first[i]+1)+(age[i]-1)
  }
  age.matrix[age.matrix > maxAge] <- maxAge

  # Create empty m-array, add dimnames
  marr <- array(0, dim = c(n.occasions-1, n.occasions, maxAge, ng))

  gNames <- colnames(freq)
  if(is.null(gNames))
    gNames <- paste0("G", 1:ng)
  dimnames(marr) <- list(released = paste0("Y", 1:(n.occasions-1)),
        recaptured = c(paste0("Y", 2:n.occasions), "never"),
        age = 1:maxAge,
        group = gNames)

  # Insert values in m-array
  first <- getFirst(ch)
  for (i in 1:nind){
    if(first[i] == n.occasions)   # Skip cases where first capture is last occasion
      next
    cap.occ <- which(ch[i,]!=0)
    capID <- paste0("Y", cap.occ)
    cap.age <- age.matrix[i, cap.occ]

    for(g in 1:ng) {
      if (length(capID) == 1 && !traploss[i,g]) {  # no recaptures
         marr[capID[1], 'never', cap.age[1], g] <- marr[capID[1], 'never', cap.age[1],g] + absfreq[i,g]
      }
      if (length(capID) > 1) {  # recaptured at least once
        for (t in 2:length(cap.occ)){
          marr[capID[t-1], capID[t], cap.age[t-1], g] <- marr[capID[t-1], capID[t], cap.age[t-1], g] + absfreq[i, g]
        }
        if (max(cap.occ) < n.occasions && !traploss[i,g]){  # never recaptured after last release
          marr[capID[t], 'never', cap.age[t], g] <- marr[capID[t], 'never', cap.age[t], g]  + absfreq[i, g]
        }
      }
    }
  }

  return(drop(marr))
}

