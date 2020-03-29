
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

if(FALSE) {
c_juv <- 0.35           # Initial capture probability of juveniles
c_ad <- 0.4            # Initial capture probability of adults
p_rec <- 0.6           # Recapture probability

c = matrix(c(c_juv, c_ad), nrow = 2, ncol=6)
p = matrix(p_rec, nrow = 2, ncol=5)

c <- c(c_juv, c_ad)
p <- p_rec
}

simCapHist2 <- function(state, cap = c(0.35, 0.4), recap = 0.6, maxAge = 2){

  nYears <- ncol(state)
  nind <- nrow(state)
  nstage <- max(state, na.rm=TRUE)
  if(maxAge > 2) {
    warning("maxAge > 2 not yet implemented; using maxAge = 2", call.=FALSE)
    maxAge <- 2
  }

  if(!is.matrix(cap))
    cap <- matrix(cap, nrow=length(cap), ncol=nYears)
  if(!is.matrix(recap))
    recap <- matrix(recap, nrow=length(recap), ncol=nYears-1)

  # Revamp the capture prob matrices to facilitate indexing
  # row 1 = newborns; row 2 = immigrants; rows 3... = age classes 1...
  if(nrow(cap) == 1) {
    C <- matrix(cap, nrow=nstage, ncol=nYears, byrow=TRUE)
  } else {
    C <- rbind(cap[1, ], cap[nrow(cap)], cap[-1,])
    while(nrow(C) < nstage)
      C <- rbind(C, cap[nrow(cap)])
  }
  if(nrow(recap) == 1) {
    P <- matrix(recap, nrow=nstage, ncol=nYears-1, byrow=TRUE)
  } else {
    P <- rbind(recap[1, ], recap[nrow(recap)], recap[-1,])
    while(nrow(P) < nstage)
      P <- rbind(P, recap[nrow(recap)])
  }
  # do a 3-d array, age x year x 2, [,,1] = first capture, [,,2] second capture.
  # Add a first column of NAs to P then combine
  Px <- cbind(NA, P)
  CP <- abind::abind(C, Px, along=3)

  # Objects to hold output
  ch.age <- matrix(0, nind, nYears) # Matrix with age class if captured, or 0
  ctBefore <- rep(1, nind)          # 1 if not caught before, 2 if already caught.

  for(t in 1:nYears) {
    alive <- which(state[, t] > 0)
    index <- cbind(state[alive, t], t, ctBefore[alive])
    caped <- rbinom(length(alive), 1, CP[index]) == 1 # TRUE if captured
    caught <- alive[caped]
    ch.age[caught, t] <- state[caught, t]
    ctBefore[caught] <- 2
  }

  # Remove individuals that have never been captured/marked
  incl <- rowSums(ch.age) >= 1
  ch.age <- ch.age[incl,]
  # Extract age of first capture
  index <- cbind(1:nrow(ch.age), getFirst(ch.age))
  age <- ch.age[index]  
  age <- pmin(age, maxAge)  # Easy with maxAge=2, if >2, need to fix Im's.
  # Get usual 0/1 capture histories
  ch <- (ch.age > 0) * 1  # *1 coerces to numeric but keeps the matrix structure.
  
  return(list(cap = cap, recap = recap, ch = ch, age = age))
}

