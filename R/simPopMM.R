
#####################################################################################################
#
# Function to simulate a population based on demographic parameters
#
# M.Schaub, 9.3.2016
#
# - last up-date: 10.11.2017 (inclusion of sex ratio of fledglings), 26.11.2019
#
# Features of the population:
# - female-based
# - but reproduction is the total number of young. The young sex ratio is used to track only females in the population later
# - includes demographic stochasticity
#
# Features of the simulation tool:
# - allows for a flexible number of age classes
#
# Input parameters:
# - Ni: vector with age(stage)-specific population size in the first year [length of vector: number of age(stage) classes, but without immigrants]. Note that the first age(stage) class refers to the 1-year old individuals, since the model is a pre-breeding model. The number of age(stage) classes must correspond to the number of age(stage) classes in the matrix of the survival probabilities.
# - phi: matrix with age(stage)- and time-specific survival probabilities [dimension of matrix: number of age(stage) classes x number of years-1]. The rows correspond to the age(stage) classes (first row are the new-borns), the columns to the years. Note that the number of age(stage) classes specified here will correspond to the number of age(stage) classes of the simulated population. Moreover, the number of colums defines the number of years
# - f: matrix with the age(stage)- and time-specific fecundity rates [dimension of matrix: number of age(stage) classes x number of years]. The first row corresponds to the fecundity of individuals in the first age(stage) class.
# - sex.ratio: vector with the sex ratio (probability that a young is a female) in each year
# - Im: vector with the number of immigrants in each year [length of vector: number of years]
#
# Output parameters:
# - ind: array with the life history of all individuals in the population
#   - the first index of the array refers to the stage
#   - the second index of the array refers to the time
#   - the third index of the array refers to the individiual
#   - Rep F are the number of fledged females, Rep M the number of fledged males
# - N: summary statistics
#   - Born F: total number of fledged females in a year, Born T: total number of fledglings in a year
#
#####################################################################################################

if(FALSE) {  # Use these if stepping through code
  # Ni = c(10, 10)
  Ni = 10
  phi = 0.55
  # phi = c(0.1, 0.2)  # extinction
  f = 3.2
  # f = 1  # extinction
  # f = 0  # extinction
  sex.ratio = 0.5
  Im = 0
  # Im = 2:7
  nYears = 6

}

simPopMM <- function(Ni = c(10, 10),
  phi = c(0.3, 0.55), f = 3.2, sex.ratio = 0.5, Im = 0, nYears = 6) {

  # ~~~~ Check and fix input ~~~~~~~~~~~~
  # TODO

  nIntervals <- ncol(phi)            # Number of years ### intervals
  if(is.null(nIntervals))
    nIntervals <- ncol(f) - 1
  if(length(nIntervals) == 0)
    nIntervals <- nYears - 1
  nYears <- nIntervals + 1  # override user input if phi or f are matrices
  mAge <- length(Ni)         # Maximal number of adult age(stage) classes

  # Turn input into matrices/vectors
  if(!is.matrix(phi))
    phi <- matrix(phi, mAge, nIntervals)
  if(!is.matrix(f))
    f <- matrix(f, mAge, nIntervals+1)
  if(length(sex.ratio) == 1)
    sex.ratio <- rep(sex.ratio, nIntervals+1)
  if(length(Im) == 1)
    Im <- rep(Im, nIntervals+1)

  # 1. Expand the vital rate matrices, such that the number of age classes corresponds to the number of years

  # Create matrix PHI with default values (= last row of phi)
  PHI <- matrix(phi[nrow(phi),], ncol = nIntervals, nrow = mAge + nIntervals, byrow=TRUE)
  PHI[1:nrow(phi),] <- phi  # Plug in user-specified values

  # Create F matrix with default values (= last row of f)
  F <- matrix(f[nrow(f),], ncol = nYears, nrow = mAge + nIntervals, byrow=TRUE)
  F[1:nrow(f),] <- f

  # 2. Create a Leslie matrix to determine approximately how many individuals will be ever alive in the population  ### not sure about this, 'growing' the array each year might be simpler.
  N <- matrix(data = NA, nrow = mAge, ncol = nYears)
  N[,1] <- Ni
  A <- array(0, dim = c(mAge, mAge, nIntervals))
  for (t in 1:nIntervals){
    for (j in 1:mAge){
      A[1,j,t] <- F[j,t] * sex.ratio[t] * PHI[1,t]    # First row in Leslie matrix
    } # j
    for (j in 2:mAge){
      A[j,j-1,t] <- PHI[j,t]           # Subdiagonal
    } # j
    A[mAge, mAge, t] <- PHI[mAge, t]
  } # t
  for (t in 1:nIntervals){
    N[,t+1] <- A[,,t]%*%N[,t] + matrix(c(rep(0, mAge-1), Im[t]), ncol = 1)
  } # t
  no.ani <- round(sum(N)*5)              # 5 times as many individuals that were alive

  # 3. Define array for each individual
  ind <- array(NA, dim = c(mAge + 5, nYears, no.ani))
  dimnames(ind)[[1]] <- c(paste(1:mAge, "Year", sep="-"), "Im", "BornF", "Dead", "RepF", "RepM")
      # NB, change of order of rows, "Im" before "BornF"! Simplifies pulling out adults.

  # -------------------------------------
  # set.seed(5) ############### for testing, take out later
  # 4. Simulate the population for each year
  # 4.1: Initialize for Year 1, insert 1s into array as per Ni
  index <- cbind(rep(1:mAge, Ni), 1, 1:sum(Ni))
  ind[index] <- 1
  # Nindex <- c(0, cumsum(Ni))
  # for (a in 1:mAge){
    # if (Ni[a]==0)
      # next
    # ind[a, 1, (Nindex[a]+1):Nindex[a+1]] <- 1
  # } # a
  Nadult <- sum(Ni)

  for (t in 1:nYears) {
    # 4.2 : insert immigrants from Im[t]
    if(Im[t] > 0) {
      ind["Im",t,(Nadult+1):(Nadult+Im[t])] <- 1
      Nadult <- Nadult + Im[t]
    }

    # 4.3 : simulate births for all (survivors) in the population
    g <- which(!is.na(ind[1:(mAge+1), t, ]), arr.ind=TRUE)
        # g is 2-column matrix with all living individuals
        # g[,1] is age class, g[,2] is individual id.
    births <- rpois(nrow(g), F[g[,1],t])  # total births per individual
    Fbirths <- rbinom(nrow(g), births, sex.ratio[t])  # female births
    ind["RepF", t, g[,2]] <- Fbirths
    ind["RepM", t, g[,2]] <- births - Fbirths
    # add newborn females to the array but don't update Nadult yet
    Nborn <- sum(Fbirths)
    if(Nborn > 0)
      ind["BornF", t, (Nadult+1):(Nadult + Nborn)] <- 1

    # STOP HERE if it's the last year
    if(t == nYears) {
      Ntotal <- Nadult + Nborn
      break
    }

    # 4.4 : Simulate survival, fill in ind[, t+1, ]
    # the dead stay dead
    ind["Dead", t+1, ] <- ind["Dead", t, ]
    # Newborns, this is first row in PHI
    NBsurv <- rbinom(Nborn, 1, PHI[1, t])
    ind[1, t+1, ((Nadult+1):(Nadult + Nborn))[NBsurv == 1]] <- 1       # next year these are 1-Years
    ind["Dead", t+1, ((Nadult+1):(Nadult + Nborn))[NBsurv == 0]] <- 1  # ...or dead
    # Adults, rows 2 to mAge+2 in PHI
    ADsurv <- rbinom(nrow(g), 1, PHI[g[,1]+1, t])
    # increment ages for all survivors less than mAge
    newAge <- g[ADsurv==1, 1]
    newAge[newAge < mAge] <- newAge[newAge < mAge] + 1
    index <- cbind(newAge, t+1, g[ADsurv==1, 2])  # locations in the array to be updated
    ind[index] <- 1                         # next year these are older
    ind["Dead", t+1, g[ADsurv==0, 2]] <- 1  # ...or dead

    # 4.5 Update Nadult
    Nadult <- Nadult + Nborn
  }
  # --------------------------------------

  # 5. Tidy up the 'ind' array
  # Remove empty cells
  ind <- ind[,,1:Ntotal]
  #  reorder the array such that it starts with the BornF ### is that a good idea?
  #  ... and ends with Dead ### ??
  neworder <- c(mAge+2, 1:(mAge+1), mAge+4:5, mAge+3)
  IND <- ind[neworder,,]

  # Summary statistics: Number of living individuals in each class (incl immigrants) and year
  # Nu <- matrix(NA, ncol = nYears, nrow = mAge + 4)
  BF <- rowSums(ind["RepF", , ], na.rm=TRUE)
  BM <- rowSums(ind["RepM", , ], na.rm=TRUE)
  adults <- apply(ind[1:(mAge+1), , ], 1:2, sum, na.rm=TRUE)
  Nu <- rbind(BornF = BF,
              adults,
              Total = colSums(adults),
              BornT = BF + BM)

  # Reshape IND if the population went extinct
  # Note that a warning message will show up when the population goes extinct, ### no ???
  #  but it has no relevance
  extinction <- which(Nu["Total",] == 0)
  if (length(extinction) >=1 ){
    extinction.year <- min(extinction)
    # Nu[,extinction.year:(nYears)] <- 0 ### they are all zero anyway
    sum(Nu[,extinction.year:(nYears)]) == 0
    IND[,extinction.year:(nYears),] <- NA  # these are all Dead
    sum(!is.na(IND[,extinction.year:(nYears),])) == 0
    rem <- numeric()
    for (i in 1:dim(IND)[3]){  ### dim(IND)[3] == Ntotal
      rem[i] <- sum(!is.na(IND[,,i]))
    } # i
    # IND <- IND[,,rem > 0]
    stopifnot(all(rem > 0))
  } # if extinct

  # 6. Output
  return(list(ind = IND, N = Nu))
}
