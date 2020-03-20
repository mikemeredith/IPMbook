
#####################################################################################################
#
# Function to create a summary statistics from a subset of a population
#
# This function is only needed, if the sampling(counts) is conducted on a subset of the complete population only (i.e. to have purely independent data)
#
# Input variables
#    ind: array with the detailed population data
#    incl: vector with the number of the individuals that are included for the summary statistics.
#
# Last-up date: 15.3.2016, 6.12.2019, M.Schaub
#
#####################################################################################################

summaryPop <- function(ind, incl){

  T <- dim(ind)[2]
  mAge <- dim(ind)[1]-4

  # Summary statistics: Number of individuals in each class and year, plus immigration rate
  Nu <- matrix(0, ncol = T, nrow = mAge + 4)
  ind.sel <- ind[,,incl]
  for (t in 1:T){
    for (a in 1:(mAge+1)){
      Nu[a,t] <- sum(ind.sel[a,t,], na.rm = TRUE)
    }
  }
  rnames <- numeric()
  for (a in 1:mAge){
    rnames[a] <- paste(a,"-Year", sep="")
  }
  rnames <- c("Juv", rnames, "Im", "Total", "Imm rate")
  rownames(Nu) <- rnames
  return(Nu)
}

