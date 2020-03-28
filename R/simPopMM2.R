
#####################################################################################################
#
# Function to simulate a population based on demographic parameters
#
# M.Schaub, 9.3.2016
#
# - last up-date: 10.11.2017 (inclusion of sex ratio of fledglings), 26.11.2019
#
# Desecrated by Mike, 2020-03-28
#
# See help file for details.
#
# This version produces a state matrix instead of the 3d array, reproduction
#   numbers are in a separate array.
# No Leslie matrix, instead we 'grow' the arrays each year.
# mAge = 1 works.
# phi has mAge + 1 rows, first row for newborns.
# No special action if population becomes extinct, return matrix of corpses.
#
#####################################################################################################

# This version returns an animals x years matrix with status as the main output
    # Status codes:
     # 0 = dead
     # 1 = newborn
     # 2 = immigrant
     # 3,4,... = adult age classes 1,2,...
# It grows the matrix each year, no Leslie matrix guessing.

if(FALSE) {  # Use these if stepping through code
  Ni = c(10, 10)
  # Ni = 10
  # phi = 0.55
  # phi = c(0.1, 0.2)  # extinction
  phi = c(0.3, 0.5, 0.6)
  f = 3.2
  # f = c(2, 3.2)
  # f = 0.1  # extinction
  # f = 0  # extinction
  sex.ratio = 0.5
  Im = 0
  # Im = 2:7
  nYears = 6

}

simPop2 <- function(Ni = c(10, 10),
  phi = c(0.3, 0.55), f = 3.2, sex.ratio = 0.5, Im = 0, nYears = 6) {

  # ~~~~ Check and fix input ~~~~~~~~~~~~
  # TODO

  nIntervals <- ncol(phi)     # NULL if not a matrix
  if(is.null(nIntervals))
    nIntervals <- ncol(f) - 1 # numeric(0) if not a matrix
  if(length(nIntervals) == 0)
    nIntervals <- nYears - 1
  nYears <- nIntervals + 1  # override user input if phi or f are matrices
  mAge <- length(Ni)         # Number of adult age(stage) classes

  # Turn input into matrices/vectors
  if(!is.matrix(phi))
    if(length(phi) < mAge+1)
      phi[length(phi):(mAge+1)] <- phi[length(phi)]
    phi <- matrix(phi, length(phi), nIntervals)
  if(!is.matrix(f))
    if(length(f) < mAge)
      f[length(f):mAge] <- f[length(f)]
    f <- matrix(f, length(f), nYears)
  if(length(sex.ratio) == 1)
    sex.ratio <- rep(sex.ratio, nYears)
  if(length(Im) == 1)
    Im <- rep(Im, nYears)

  # 1. Expand the vital rate matrices
  # Create matrix PHI for indexing
  # row 1 = newborns; row 2 = immigrants; rows 3... = age classes 1...
  PHI <- phi[c(1, nrow(phi), 2:nrow(phi)), ]

  # Create F matrix for indexing: row 1 not used, 2 = immigrants, 3... age classes
  F <- f[c(NA, nrow(f), 1:nrow(f)), ]

  # 3. Define state matrix and reproduction array
  state <- matrix(NA, sum(Ni) + sum(Im), nYears)
  reprod <- array(NA, c(sum(Ni) + sum(Im), nYears, 2))
  
  # -------------------------------------
  # 4. Simulate the population for each year
  # 4.1: Initialize for Year 1, insert 1s into matrix as per Ni
  state[1:sum(Ni), 1] <- rep(1:mAge, Ni) + 2
  Nadult <- sum(Ni)

  for (t in 1:nYears) {
    # 4.2 : add immigrants from Im[t]
    if(Im[t] > 0) {
      state[(Nadult+1):(Nadult+Im[t]), t] <- 2
      Nadult <- Nadult + Im[t]
    }

    # 4.3 : simulate births for all surviving adults in the population
    alive <- which(state[, t] > 1)
    g <- state[alive, t]
    births <- rpois(length(g), F[g,t])  # total births per individual
    Fbirths <- rbinom(length(g), births, sex.ratio[t])  # female births
    reprod[alive, t, 1] <- Fbirths
    reprod[alive, t, 2] <- births - Fbirths
    Nborn <- sum(Fbirths)
    # 'grow' the arrays, add newborn females to the array but don't update Nadult yet
    if(Nborn > 0) {
      reprod <- abind::abind(reprod, array(NA, c(Nborn, nYears, 2)), along=1)
      state <- rbind(state, matrix(NA, Nborn, nYears))
      state[(Nadult+1):(Nadult + Nborn), t] <- 1
    }

    # STOP HERE if it's the last year
    if(t == nYears) {
      Ntotal <- Nadult + Nborn
      break
    }

    # 4.4 : Simulate survival, fill in ind[, t+1, ]
    # the dead stay dead, the rest will be updated
    state[, t+1] <- state[, t]
    # Newborns, this is first row in PHI
    if(Nborn > 0) {
      NBsurv <- rbinom(Nborn, 1, PHI[1, t])
      state[(Nadult+1):(Nadult + Nborn), t+1] <- NBsurv*3 # next year these are 1-Years or dead
    }
    # Adults, rows 2 to mAge+2 in PHI
    ADsurv <- rbinom(length(g), 1, PHI[g, t])
    # increment ages for all survivors less than mAge, except immigrants
    newAge <- g * ADsurv
    newAge[newAge > 2] <- newAge[newAge > 2] + 1
    newAge <- pmin(newAge, mAge+2)  # upper limit to age classes
    state[alive, t+1] <- newAge

    # 4.5 Update Nadult
    Nadult <- Nadult + Nborn
  }
  # --------------------------------------

  # Summary statistics: Number of living individuals in each class (incl immigrants) and year
  BF <- colSums(reprod[, , 1], na.rm=TRUE)
  BM <- colSums(reprod[, , 2], na.rm=TRUE)
  adults <- apply(state, 2, tabulate, nbins=mAge+2)[-1,]
  rownames(adults) <- c("Im", paste(1:mAge, "Year", sep="-"))
  Nu <- rbind(BornF = BF,
              adults,
              Total = colSums(adults),
              BornT = BF + BM)

  # 6. Output
  return(list(
    # ~~~~ simulation parameters ~~~~~
    Ni = Ni, phi = phi, f = f, sex.ratio = sex.ratio, Im = Im,
    # ~~~~ simulated output ~~~~~
    state = state, reprod = unname(reprod), N = Nu))
}

# Testing, testing
if(FALSE) {
set.seed(1)
tmp <- simPop2()
str(tmp)
tmp$N

set.seed(1)
tmp <- simPop2(Im=5)
str(tmp)
tmp$N

set.seed(1)
tmp <- simPop2(f = c(2.3, 4))
str(tmp)
tmp$N

set.seed(1)
tmp <- simPop2(f = 0.1, phi = c(0.1, 0.2))  # Extinction
str(tmp)
tmp$N

set.seed(1)
tmp <- simPop2(Ni = 10, phi=0.55, f=3.2)  # 1 age class
str(tmp)
tmp$N
tmp$phi
tmp$f

set.seed(1)
tmp <- simPop2(Ni = 10, phi=0.55, f=3.2, Im=5)
str(tmp)
tmp$N
}