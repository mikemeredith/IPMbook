
# Function to combine a vector of frequencies and a vector of group IDs
#  into a matrix with frequencies for each group

# Input:
# freq is a vector of frequencies.
# groups is a factor (or coercible object) identifying the group of each row.

# Value: a numeric matrix with 1 column per group

groupfreq <- function(freq, groups) {
  groups <- factor(groups)
  ng <- nlevels(groups)
  nind <- length(groups)

  out <- matrix(0, nrow=nind, ncol=ng)
  colnames(out) <- levels(groups)
  out[cbind(1:nind, as.numeric(groups))] <- freq
  return(out)
}
