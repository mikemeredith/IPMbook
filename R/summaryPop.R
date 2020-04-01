
#####################################################################################################
#
# Function to create a summary statistics from a subset of a population
#
# This function is only needed, if the sampling(counts) is conducted on a subset of the complete population only (i.e. to have purely independent data)
#
# Input variables
#    ind: array with the detailed population data
#    incl: vector with the number of the individuals that are included for the summary statistics.
#'
# Last-up date: 15.3.2016, 6.12.2019, M.Schaub
#
#####################################################################################################

summaryPop <- function(state, subset){

  # ~~~~~ check and fix input ~~~~~~~~~~~~~
  stopifnotMatrix(state, allowNA=TRUE, numericOnly=TRUE)
  if(!missing(subset))
    stopifnotBetween(subset, min=1, max=nrow(state))

  mAge <- max(state, na.rm=TRUE)
  if(missing(subset)) {
    statex <- state
  } else {
    statex <- state[subset, ]
  }

  # Summary statistics: Number of individuals in each class and year, total adults and newborns
  Nu <- apply(statex+1, 2, tabulate, nbins=mAge+1) # +1 because tabulate ignores 0s.
  adults <- Nu[-1, ]
  rownames(adults) <- paste(1:mAge, "Years", sep="-")
  N <- rbind(adults,
            Total = colSums(adults),
            JuvF = Nu[1,])
  return(N)
}

