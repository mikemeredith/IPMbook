
# Section 4.5.1.1

# p 80 of MS dated 2020-11-13

# Create a matrix x[i,t] which indicates the age class at time t
# for each individual i, based on the occasion of first capture, f, and
# age at first capture.

# Function to create the age matrix
createAge <- function(f, age, nyears, mAge=2){
  a <- matrix(NA, nrow=length(f), ncol=nyears)
  k <- c(1:mAge, rep(max(mAge), nyears))
  for (i in 1:length(f)){
    a[i,f[i]:(nyears)] <- k[age[i]:(nyears-f[i]+age[i])]
  }
  return(a[, 1:(nyears - 1)])
}

