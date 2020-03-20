
#########################################
#
# Function to simulate productivity data
#
# Input variables
#    ind: array with population information as produced with function createPopulation
#    prep: vector with the annual detection probabilities of broods
#    females.only: if 'yes' only the number of females that are produced is recorded. Default: total reproduction is recorded
#
# Output variables
#    prod.ind: matrix with the individual reproductive output. The three columns give the output, the year of the brood and the age of the mother.
#    prod.agg: matrix with the same data, but aggregated. The two columns give the year-specific total number of newborn and the year-specific number of surveyed broods
#
# Written: 16.3.2016, M.Schaub
# Last-up date: 16.8.2018, 26.11.2019, M.Schaub
#
################################################

simProd <- function(ind, prep, females.only = 'no'){

  r <- dim(ind)[1] - 2
  maxAge <- dim(ind)[1] - 4
  T <- dim(ind)[2]
  rep <- year <- age <- numeric()
  le <- 0
  for (t in 1:T){
    z <- which(!is.na(ind[r,t,]))
    if (length(z) > 0){
      for (i in 1:length(z)){
        j <- le + i
        h <- rbinom(1, 1, prep[t])
        if (h==1){
          rep[j] <- ind[r,t,z[i]] + ind[r+1,t,z[i]]
          if (females.only == 'yes') {
            rep[j] <- ind[r,t,z[i]]
          }
          year[j] <- t
          age[j] <- which(!is.na(ind[2:(maxAge+1),t,z[i]]))
        } else {
          rep[j] <- NA
          year[j] <- NA
          age[j] <- NA
        } # else
      } # i
      le <- length(rep)
    } # if
  } # t
  age[age==(maxAge+1)] <- maxAge   # re-adjust age of immigrants to maximal age
  k <- which(!is.na(rep))
  prod.ind <- cbind(rep[k], year[k], age[k])
  colnames(prod.ind) <- c("Productivity", "Year", "Age of mother")
  prod.agg <- matrix(NA, nrow = T, ncol = 2)
  for (t in 1:T){
    prod.agg[t,1] <- sum(prod.ind[prod.ind[,2]==t,1])
    prod.agg[t,2] <- length(prod.ind[prod.ind[,2]==t,1])
  }
  colnames(prod.agg) <- c("Juveniles", "Surveyed broods")

  return(list(prod.ind = prod.ind, prod.agg = prod.agg))
}

