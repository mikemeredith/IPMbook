
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
#   immigrants dealt with differently, no immigrant cohort.
    # Status codes:
     # -1 = died (recently)
     # 0 = newborn
     # 1,2,... = adult age classes 1,2,...
# It grows the matrix each year, no Leslie matrix guessing.

if(FALSE) {  # Use these if stepping through code
  Ni = c(10, 10, 10) # 3 age classes
  # Ni = 10
  # phi = 0.55
  # phi = c(0.1, 0.2)  # extinction
  phi = c(0.3, 0.6) # survival low for newborns, constant for adults
  # f = 3.2
  f = c(2, 3.2)  # (some of) first cohort breed, but fecundity lower
  # f = c(0, 3.2)  # first cohort do not breed
  # f = 0.1  # extinction
  # f = 0  # extinction
  # pBreed = 1
  pBreed = c(0.7, 1)  # only 70% of 1-Year's attempt to breed
  sex.ratio = 0.5
  # Im = 0
  Im = 2:7
  # ageOfIm = 1
  ageOfIm = 2
  nYears = 6

}

simPop2 <- function(Ni = c(10, 10),
  phi = c(0.3, 0.55), f = 3.2, pBreed = 1, sex.ratio = 0.5, Im = 0, ageOfIm = 1, nYears = 6) {

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
  if(!is.matrix(phi))  {
    phi <- matrix(phi, length(phi), nIntervals)
  }
  if(nrow(phi) < mAge+1) {
    phi0 <- matrix(phi[nrow(phi),], nrow=mAge+1-nrow(phi), nIntervals)
    phi <- rbind(phi, phi0)
  }
  if(!is.matrix(f)) {
    f <- matrix(f, length(f), nYears)
  }
  if(nrow(f) < mAge) {
    f0 <- matrix(f[nrow(f),], nrow=mAge-nrow(f), nYears)
    f <- rbind(f, f0)
  }
  if(!is.matrix(pBreed)) {
    pBreed <- matrix(pBreed, length(pBreed), nYears)
  }
  if(nrow(pBreed) < mAge) {
    pBreed0 <- matrix(pBreed[nrow(pBreed),], nrow=mAge-nrow(pBreed), nYears)
    pBreed <- rbind(pBreed, pBreed0)
  }
  if(length(sex.ratio) == 1)
    sex.ratio <- rep(sex.ratio, nYears)
  if(length(Im) == 1)
    Im <- rep(Im, nYears)
  if(length(ageOfIm) == 1)
    ageOfIm <- rep(ageOfIm, nYears)

  # 3. Define state matrix, reproduction array, and immigration vector
  state <- matrix(NA, sum(Ni) + sum(Im), nYears)
  colnames(state) <- paste0("Y", 1:nYears)
  reprod <- array(NA, c(sum(Ni) + sum(Im), nYears, 3))
  dimnames(reprod) <- list(NULL, paste0("Y", 1:nYears), c("F", "M", "Age"))
  imYear <- rep(NA, sum(Ni) + sum(Im)) # year the animal immigrated into the population

  # -------------------------------------
  # 4. Simulate the population for each year
  # 4.1: Initialize for Year 1, insert ages into matrix as per Ni
  state[1:sum(Ni), 1] <- rep(1:mAge, Ni)
  Nadult <- sum(Ni)

  for (t in 1:nYears) {
    # 4.2 : add immigrants from Im[t]
    if(Im[t] > 0) {
      state[(Nadult+1):(Nadult+Im[t]), t] <- ageOfIm[t]
      imYear[(Nadult+1):(Nadult+Im[t])] <- t
      Nadult <- Nadult + Im[t]
    }

    # 4.3 : simulate births for all surviving adults in the population
    alive <- which(state[, t] > 0)
    age <- state[alive, t]
    tmp <- rbinom(length(age), 1, pBreed[age, t])
    breeding <- alive[tmp == 1]
    ageB <- state[breeding, t]
    births <- rpois(length(ageB), f[ageB,t])  # total births per individual
    Fbirths <- rbinom(length(ageB), births, sex.ratio[t])  # female births
    reprod[breeding, t, "F"] <- Fbirths
    reprod[breeding, t, "M"] <- births - Fbirths
    reprod[breeding, t, "Age"] <- ageB
    Nborn <- sum(Fbirths)
    # 'grow' the arrays, add newborn females to state but don't update Nadult yet
    if(Nborn > 0) {
      reprod <- abind::abind(reprod, array(NA, c(Nborn, nYears, 3)), along=1)
      state <- rbind(state, matrix(NA, Nborn, nYears))
      state[(Nadult+1):(Nadult + Nborn), t] <- 0
      imYear <- c(imYear, rep(NA, Nborn))
    }

    # STOP HERE if it's the last year
    if(t == nYears) {
      Ntotal <- Nadult + Nborn
      break
    }

    # 4.4 : Simulate survival, fill in ind[, t+1, ]
    # Newborns, this is first row in phi
    if(Nborn > 0) {
      NBsurv <- rbinom(Nborn, 1, phi[1, t])
      NBsurv[NBsurv == 0] <- -1                         # -1 indicates Died
      state[(Nadult+1):(Nadult + Nborn), t+1] <- NBsurv # next year these are 1-Years or died
    }
    # Adults, rows 2 to mAge+1 in phi
    ADsurv <- rbinom(length(age), 1, phi[age+1, t])
    newAge <- age + 1              # increment ages for all survivors
    newAge[ADsurv == 0] <- -1    # -1 = died
    newAge <- pmin(newAge, mAge) # enforce upper limit to age classes
    state[alive, t+1] <- newAge

    # 4.5 Update Nadult
    Nadult <- Nadult + Nborn
  }
  # --------------------------------------

  # 5. Summary statistics: Number of living individuals in each class (incl immigrants) and year
  BF <- colSums(reprod[, , 1], na.rm=TRUE)
  BM <- colSums(reprod[, , 2], na.rm=TRUE)
  adults <- apply(state, 2, tabulate, nbins=mAge)  # not all adults are necessarily breeders
  if(mAge == 1)
    adults <- matrix(adults, nrow=1)
  rownames(adults) <- paste(1:mAge, "Year", sep="-")
  Nu <- rbind(adults,
              Total = colSums(adults),
              BornF = BF,
              BornT = BF + BM,
              Im = Im)

  # Summary of breeding females
  breeders <- apply(reprod[,,3], 2, tabulate, nbins=mAge)
  if(mAge == 1)
    breeders <- matrix(breeders, nrow=1)
  rownames(breeders) <- paste(1:mAge, "Year", sep="-")
  breeders <- rbind(breeders, "Total" = colSums(breeders))

  # 6. Output
  return(list(
    # ~~~~ simulation parameters ~~~~~
    Ni = Ni, phi = phi, f = f, pBreed = pBreed, sex.ratio = sex.ratio,
    Im = Im, ageOfIm = ageOfIm,
    # ~~~~ simulated output ~~~~~
    state = state, imYear = imYear, reprod = reprod, N = Nu, breeders = breeders))
}

