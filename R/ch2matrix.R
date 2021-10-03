
# Function to convert a capture history recorded as text to a matrix, exported.

# Input: 'ch' could be
# 1a. a proper CH matrix: numeric, > 1 column -- returned unchanged
# 1b. the same, as a data frame

# 2a. a character vector with "100110", "001100", etc
# 2b. the same, as a factor
# 2c. a numeric vector with 100110, 1100, etc (leading zeros dropped)

# 3a. a 1-column data frame with 2a, b or c.
# 3b. a 1-column matrix with 2a or 2c.

# Value: a numeric matrix with 1 column per capture occasion and 1 row per capture history

ch2matrix <- function(ch) {
  if(is.data.frame(ch))
    ch <- as.matrix(ch)
  if(is.matrix(ch) && is.numeric(ch) && ncol(ch) > 1)
    return(ch)
  if(is.factor(ch))
    ch <- as.character(ch)

  # Check for matrix with > 1 column -> error
  if(is.matrix(ch) && ncol(ch) > 1)
    stop("The format of 'ch' was not recognised.", call. = FALSE)

  ch0 <- as.numeric(ch)
  if(is.character(ch)) {
    len <- nchar(ch)
    if(sum(len - len[1]) !=0)
      stop("Capture histories must all have the same number of occasions.")
    ncol <- len[1]
  } else {
    ncol <- round(log10(max(ch0))) + 1
  }

  out <- matrix(NA, length(ch0), ncol)
  colnames(out) <- paste0("Y", 1:ncol)
  for(i in 1:ncol) {
    out[,i] <- ch0 %/% 10^(ncol-i)
    ch0 <- ch0 %% 10^(ncol-i)
  }

  return(out)
}
