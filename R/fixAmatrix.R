
# Expand abbreviated input into a matrix of required size. Not exported.

fixAmatrix <- function(x, nrow, ncol) {
  name <- deparse(substitute(x))
  if(!is.matrix(x)) {
    if(length(x) > nrow)  # check if vector is too long
      stop("Length of vector '", name, "' cannot be greater than ", nrow, ".", call.=FALSE)
    x <- matrix(x, nrow=length(x), ncol=ncol) # make matrix, plug x into all the columns
  }
  if(ncol(x) != ncol) # Check number of columns (needed if original x is a matrix)
      stop("Matrix '", name, "' must have ", ncol, " columns.", call.=FALSE)
  if(nrow(x) > nrow)  # Check number of columns (needed if original x is a matrix)  
      stop("Matrix '", name, "' cannot have more than ", nrow, " rows.", call.=FALSE)
  if(nrow(x) < nrow) {
    x0 <- matrix(x[nrow(x),], nrow=nrow-nrow(x), ncol, byrow=TRUE) # Replicate last row
    x <- rbind(x, x0)   # ... and add to the matrix.
  }
  return(x)
}

fixAvector <- function(x, length) {
  name <- deparse(substitute(x))
  if(length(x) == length)  # Do nothing
    return(x)
  if(length(x) != 1)
    stop("Argument '", name, "' must have length 1 or ", length, ".", call.=FALSE)
  return(rep(x, length))
}