
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


simProd <- function(reprod, pInclude = 0.3, females.only = FALSE, verbose = TRUE){

  inputName <- deparse(substitute(reprod))

  # ~~~~~~ check and fix input ~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnotArray(reprod, dims=3, allowNA = TRUE, numericOnly=TRUE)
  nYears <- dim(reprod)[2]
  stopifnotProbability(pInclude, allowNA=FALSE)
  pInclude <- fixAvector(pInclude, nYears)
  females.only <- females.only[1]
  stopifnotLogical(females.only, allowNA=FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  nNest <- sum(!is.na(reprod[,,3]))
  if(verbose) {
    cat(paste0("The input object ", sQuote(inputName), " has ", dim(reprod)[1], " individuals x ",
       nYears, " Years, with ", nNest, "\nbreeding attempts."))
    cat(paste0(" Mothers' ages range from ", min(reprod[,,3], na.rm=TRUE), " to ",
       max(reprod[,,3], na.rm=TRUE), ".\n\n"))
  }
  prod.ind <- matrix(NA, nNest, 3)
  colnames(prod.ind) <- c("Productivity", "Year", "Age of mother")
  le <- 0
  for (t in 1:nYears){
    z <- which(!is.na(reprod[, t, "F"]))  # row ids of all breeders
    if (length(z) > 0){
      found <- rbinom(length(z), 1, pInclude[t]) == 1
      offspring <- reprod[z[found], t, "F"]
      if(!females.only)
        offspring <- offspring + reprod[z[found], t, "M"]
      prod.ind[(le+1):(le+sum(found)), 1] <- offspring
      prod.ind[(le+1):(le+sum(found)), 2] <- t
      prod.ind[(le+1):(le+sum(found)), 3] <- reprod[z[found], t, 3]
      le <- le+sum(found)
    }
  }

  prod.ind <- prod.ind[1:le, ]

  prod.agg <- cbind(
    "Juveniles" = tapply(prod.ind[, 1], prod.ind[, 2], sum),
    "Surveyed broods" = tapply(prod.ind[, 1], prod.ind[, 2], length))

  return(list(pInclude = pInclude, females.only = females.only,
    prod.ind = prod.ind, prod.agg = prod.agg))
}

