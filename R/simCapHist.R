
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

simCapHist <- function(state, cap = c(0.35, 0.4), recap = NULL,
    maxAge = NULL, verbose = TRUE){

  inputName <- deparse(substitute(state))
  # ~~~~~ check and fix input ~~~~~~~~~~~~~
  stopifnotMatrix(state, allowNA=TRUE, numericOnly=TRUE)
  nYears <- ncol(state)
  nind <- nrow(state)
  nstage <- max(state, na.rm=TRUE) + 1
  stopifnotProbability(cap, allowNA=FALSE)
  cap <- fixAmatrix(cap, nrow=nstage, ncol=nYears)
  if(is.null(recap)) {
    recap <- cap[-1, -1, drop=FALSE]
  } else {
    stopifnotProbability(recap, allowNA=FALSE)
    recap <- fixAmatrix(recap, nrow=nstage-1, ncol=nYears-1)
  }
  if(!is.null(maxAge)) {
    maxAge <- round(maxAge[1])
    stopifnotBetween(maxAge, min=1, max=nstage, allowNA=FALSE)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print some information
  if(verbose) {
    cat(paste0("The input object ", sQuote(inputName), " has ", nind, " individuals x ", nYears,
      " Years. Ages range from ", max(0, min(state, na.rm=TRUE)), " to ", max(state, na.rm=TRUE), ".\n"))
    cat(paste0("The ", sQuote("cap"), " matrix should be ", nstage, " x ", nYears,
      " and the ", sQuote("recap"), " matrix ", nstage-1, " x ", nYears-1, ".\n\n" ))
  }

  # do a 3-d array, age x year x 2, [,,1] = first capture, [,,2] second capture.
  # Add a first column and first row of NAs to recap then combine
  Px <- rbind(NA, cbind(NA, recap))
  CP <- abind::abind(cap, Px, along=3)

  # Objects to hold output
  ch.age <- matrix(NA, nind, nYears) # Matrix with age class if captured, or NA
  ctBefore <- rep(1, nind)          # 1 if not caught before, 2 if already caught.

  for(t in 1:nYears) {
    alive <- which(state[, t] >= 0)
    index <- cbind(state[alive, t]+1, t, ctBefore[alive])
    caped <- rbinom(length(alive), 1, CP[index]) == 1 # TRUE if captured
    caught <- alive[caped]
    ch.age[caught, t] <- state[caught, t]
    ctBefore[caught] <- 2
  }

  # Remove individuals that have never been captured/marked (ie, all-NA rows)
  incl <- rowSums(!is.na(ch.age)) >= 1
  ch.age <- ch.age[incl,]
  # Extract age of first capture
  ch.age <- ch.age + 1 # 1 = newborns, other ages increased by 1
  index <- cbind(1:nrow(ch.age), getFirst(ch.age))
  age <- ch.age[index]
  if(!is.null(maxAge))
    age <- pmin(age, maxAge)
  # Get usual 0/1 capture histories
  ch <- (ch.age > 0) * 1  # *1 coerces to numeric but keeps the matrix structure.
  ch[is.na(ch)] <- 0

  return(list(cap = cap, recap = recap, maxAge = maxAge,
              ch = ch, age = age))
}

