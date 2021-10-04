
# Function to combine a vector of frequncies and a vector of group IDs
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


# For testing:
if(FALSE) {
library(wiqid)
data(dippers)
CH <- dippers[, 1:7]
groups <- dippers$sex
CHf <- CH[groups=="F", ]
CHm <- CH[groups=="M", ]

freq <- rep(1, length(groups))

library(IPMbook)

mf <- marray(CHf)
mm <- marray(CHm)

f2 <- groupfreq(freq, groups)
mg <- marray(CH, freq=f2)
mg
all(mg[,,1] == mf)
all(mg[,,2] == mm)


}
