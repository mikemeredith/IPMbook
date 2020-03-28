
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
#   - Juv F: total number of fledged females in a year, Juv T: total number of fledglings in a year
#
#####################################################################################################


simPop <- function(Ni = c(10, 10),
  phi = c(0.3, 0.55), f = 3.2, sex.ratio = 0.5, Im = 0, nYears = 6) {

  T <- ncol(phi)            # Number of years ### intervals
  if(is.null(T))
    T <- ncol(f) - 1
  if(length(T) == 0)
    T <- nYears - 1
  # mAge <- nrow(phi)         # Maximal number of age(stage) classes
  mAge <- length(Ni)         # Maximal number of age(stage) classes

  # Turn input in matrices/vectors
  if(!is.matrix(phi))
    phi <- matrix(phi, mAge, T)
  if(!is.matrix(f))
    f <- matrix(f, mAge, T+1)
  if(length(sex.ratio) == 1)
    sex.ratio <- rep(sex.ratio, T+1)
  if(length(Im) == 1)
    Im <- rep(Im, T+1)

  # 1. Expand the vital rate matrices, such that the number of age classes corresponds to the number of years
  PHI <- matrix(0, ncol = T, nrow = mAge + T)
  PHI[1:nrow(phi),] <- phi
  u <- mAge + T - nrow(phi)  ### u == T
  if (u > 0){                ### u == T > 0 always
    for (j in 1:u){
       PHI[nrow(phi)+j,] <- phi[nrow(phi),]
    } # j
  } # if

  F <- matrix(0, ncol = T + 1, nrow = mAge + T)
  F[1:nrow(f),] <- f
  u <- mAge + T - nrow(f)
  if (u > 0){
    for (j in 1:u){
      F[nrow(f)+j,] <- f[nrow(f),]
    } # j
  } # if

  # Nindex <- c(0, cumsum(Ni))

  # 2. Create a Leslie matrix to determine approximately how many individuals will be ever alive in the population
  N <- matrix(data = NA, nrow = mAge, ncol = T + 1)
  N[,1] <- Ni
  A <- array(0, dim = c(mAge, mAge, T))
  for (t in 1:T){
    for (j in 1:mAge){
      A[1,j,t] <- F[j,t] * sex.ratio[t] * PHI[1,t]    # First row in Leslie matrix
    } # j
    for (j in 2:mAge){
      A[j,j-1,t] <- PHI[j,t]           # Subdiagonal
    } # j
    A[mAge, mAge, t] <- PHI[mAge, t]
  } # t
  for (t in 1:T){
    N[,t+1] <- A[,,t]%*%N[,t] + matrix(c(rep(0, mAge-1), Im[t]), ncol = 1)
  } # t
  no.ani <- round(sum(N)*5)              # 5 times as many individuals that were alive

  # 3. Define array for each individual
  ind <- array(NA, dim = c(mAge + 5, T + 1, no.ani))   # information about [1-ye, 2-ye, ..., mAge-ye, Juv, Im, Rep, Dead]  ### add dimnames here?

  # 4. Simulate the fates of individuals already present at t = 1 (in different age classes) and their reproduction
  # 4.a: Simulate survival of the individuals present at t=1
  # Initialize = insert 1s into array as per Ni
  Nindex <- c(0, cumsum(Ni))
  for (a in 1:mAge){
    if (Ni[a]==0)
      next
    for (i in (Nindex[a]+1):Nindex[a+1]){  ### use indexing
      ind[a,1,i] <- 1
    } # i
  } # if ### no, for(a...)

  # Simulate survival
  z <- numeric(T)        ### inserted T
  for (a in 1:mAge){
    for (i in (Nindex[a]+1):Nindex[a+1]){
      for (t in 1:T){                   ### use indexing
        z[t] <- rbinom(1,1,PHI[a+t,t]) ### a starts at 1, t starts at 1 so a+t starts at 2
      } # t
      Z <- max(sum(cumprod(z)))  ### no. of intervals survived, Z > 0 if z[1] == 1, sum returns scalar, why max?
      if (Z==0) {
        ind[mAge+5,2,i] <- 1  ### flag as Dead in yr 2
        next
      } # if
      for (u in 1:Z){
        if (a+u < mAge){  ### maybe can use min(mAge, a+u)
          ind[a+u,u+1,i] <- 1
        } else {
          ind[mAge,u+1,i] <- 1
        } # else
      } # u
      # Record year of death (if any)
      if (sum(z)==T) {  ### ie alive for whole study period
        next
      } else {
        D <- min(which(z==0))  ### first non-survival
        ind[mAge+5,D+1,i] <- 1  ### flag as dead at the end of the interval
      } # else
    } # i
  } # a

  # 3.b: Survival of immigrants (in all years, not just of immigrants present at t = 1)
  Nimindex <- c(0, cumsum(Im)) + max(Nindex)
  for (t in 1:(T+1)){
    if (Im[t]==0)
      next
    for (i in (Nimindex[t]+1):Nimindex[t+1]){
      ind[mAge+2,t,i] <- 1   ### flag as immigrants ### use ind['Im', t, i]
    } # i
  } # t
  for (t in 1:T){
    if (Im[t]==0) next
    for (i in (Nimindex[t]+1):Nimindex[t+1]){
      z <- numeric()
      for (d in t:T){
        z[d-t+1] <- rbinom(1,1,PHI[mAge+d,d])
      } # d
      Z <- max(sum(cumprod(z)))
      if (Z==0){
        ind[mAge+5,t+1,i] <- 1
        next
      } # if
      for (u in 1:Z){
        if (t+u <= (T+1)){
          ind[mAge,u+t,i] <- 1
        } else {
          next
        }
      }  # u
      # Record year of death (if any)
      if (sum(z)==T-t+1) {
        next
      } else {
        D <- min(which(z==0))
        ind[mAge+5,D+t,i] <- 1
      } # else
    } # i
  } # t

  # 3.c: Simulate reproduction of all already existing individuals
  for (i in 1:max(Nimindex)){   ### Nimdex includes immigrants
    for (t in 1:(T+1)){
      g <- which(!is.na(ind[c(1:mAge, mAge+2),t,i]))
      if (length(g)==0)
        next
      if (g != 8){  ######## where does 8 come from?? What if g = 42?
        juv.tot <- rpois(1,F[g,t])
        ind[mAge+3,t,i] <- rbinom(1,juv.tot,sex.ratio[t])  ### RepF
        ind[mAge+4,t,i] <- juv.tot - ind[mAge+3,t,i]       ### RepM
      } # if
      if (g == 8){
        juv.tot <- rpois(1,F[mAge,t])
        ind[mAge+3,t,i] <- rbinom(1,juv.tot,sex.ratio[t])
        ind[mAge+4,t,i] <- juv.tot - ind[mAge+3,t,i]
      } # if
    } # t
  } # i

  # 4. Simulate the fates of individuals born during the study
  # - determine the number of nestlings
  # - determine their fate over time
  # - determine their reproduction
  nestl <- numeric(T+1)   #### inserted T+1
  nestl[1] <- 0
  for (t in 1:(T+1)){
    # 4.a: Enumerate the number of female nestlings
    nestl[t+1] <- sum(ind[mAge+3,t,], na.rm = TRUE) ### total female nestlings in year t
    ind[mAge+1,t,
      (max(Nimindex)+max(cumsum(nestl[1:t]))+1):(max(Nimindex)+max(cumsum(nestl[1:(t+1)])))] <- 1
    if (t==(T+1))
      break

    # 4.b: Model survival of these individuals
    for (i in (max(Nimindex)+max(cumsum(nestl[1:t]))+1):(max(Nimindex)+max(cumsum(nestl[1:(t+1)])))){
      z <- numeric() ###
      for (d in t:T){
        z[d-t+1] <- rbinom(1,1,PHI[d-t+1,d])
      } # d
      Z <- max(sum(cumprod(z)))  ### sum returns scalar, why max??
      if (Z==0){
        ind[mAge+5,t+1,i] <- 1  ### flag as Dead at next survey
        next
      } # if
      for (u in 1:Z){
        if (u < mAge){       ### use min
          ind[u,u+t,i] <- 1
        } else {
          ind[mAge,u+t,i] <- 1
        } # else
      }  # u
      # Record year of death (if any)
      if (sum(z)==T-t+1) {
        next
      } else {
        D <- min(which(z==0))
        ind[mAge+5,D+t,i] <- 1
      } # else
    } # i

    # 4.c: Model reproduction of the surviving individuals
    for (i in (max(Nimindex)+max(cumsum(nestl[1:t]))+1):(max(Nimindex)+max(cumsum(nestl[1:(t+1)])))){
      for(d in t:T+1){
        g <- which(!is.na(ind[c(1:mAge),d,i]))
        if (length(g)==0)
          next
        if (g !=8){
          juv.tot <- rpois(1,F[g,d])
          ind[mAge+3,d,i] <- rbinom(1,juv.tot,sex.ratio[t])
          ind[mAge+4,d,i] <- juv.tot - ind[mAge+3,d,i]
        } # if
      } # d
    } # i
  } # t

  # 5. Enumerate the total number of animals
  Ntotal <- sum(Ni) + sum(ind[mAge+1,1:(T+1),], na.rm = TRUE) + sum(Im)
  # Remove empty cells and reorder the array such that it starts with the Juv
  IND <- ind[,,1:Ntotal]
  IND[1,,] <- ind[mAge+1,,1:Ntotal] #### move "Juv" to first row
  for (a in 1:mAge){
    IND[a+1,,] <- ind[a,,1:Ntotal]
  } # a
  rnames <- numeric()  #### numeric ??!!
  for (a in 1:mAge){
    rnames[a] <- paste(a,"-Year", sep="")
  } # a
  rnames <- c("Juv", rnames, "Im", "Rep F", "Rep M", "Dead")
  rownames(IND) <- rnames

  # Summary statistics: Number of individuals in each class and year, plus immigration rate
  Nu <- matrix(NA, ncol = T+1, nrow = mAge + 4)
  for (t in 1:(T+1)){
    for (a in 1:(mAge+1)){
      Nu[a,t] <- sum(IND[a,t,], na.rm = TRUE)
    } # a
    Nu[mAge+2,t] <- sum(IND[mAge+2,t,], na.rm = TRUE)
    Nu[mAge+3,t] <- sum(Nu[2:(mAge+1),t]) + sum(IND[mAge+2,t,], na.rm = TRUE)
    Nu[mAge+4,t] <- sum(IND[mAge+4,t,], na.rm = TRUE) + Nu[1,t]
  } # t
  rnames <- numeric()
  for (a in 1:mAge){
    rnames[a] <- paste(a,"-Year", sep="")
  } # a
  rnames <- c("Juv F", rnames, "Im", "Total", "Juv T")
  rownames(Nu) <- rnames

  # Reshape output if the population went extinct
  # Note that a warning message will show up when the population goes extinct, but it has no relevance
  extinction <- which(Nu["Total",] == 0)
  if (length(extinction)>=1){
    extinction.year <- min(extinction)
    Nu[,extinction.year:(T+1)] <- 0
    IND[,extinction.year:(T+1),] <- NA
    rem <- numeric()
    for (i in 1:dim(IND)[3]){
      rem[i] <- sum(!is.na(IND[,,i]))
    } # i
    IND <- IND[,,rem>0]
  } # if

  # 6. Output
  return(list(ind = IND, N = Nu))
}
