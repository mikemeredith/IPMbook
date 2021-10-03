\name{ch2matrix}
\alias{ch2matrix}
\encoding{UTF-8}

\title{
Convert capture histories in text format to a matrix
}
\description{
The function takes capture histories in any of the following formats and converts it to a matrix of integers with one column for each capture occasion and one row for each capture history.

Possible formats for \code{ch} are:

1a. a proper CH matrix: numeric, > 1 column -- returned unchanged.\cr
1b. the same, as a data frame.

2a. a character vector with "100110", "001100", etc, consisting entirely of digits (no letters, spaces, or symbols).\cr
2b. the same, as a factor.\cr
2c. a numeric vector with 100110, 1100, etc (leading zeros dropped).

3a. a 1-column data frame with 2a, b or c.\cr
3b. a 1-column matrix with 2a or 2c.
}
\usage{
ch2matrix(ch)
}
\arguments{
  \item{ch}{
capture histories in a suitable format, see Description.
}
}
\value{
A matrix of integers with one column for each capture occasion and one row for each capture history.
}
\author{
Mike Meredith
}

\examples{
# Create some example data
raw <- c("1000", "1000", "1100", "1100", "1100", "1110",
  "0110", "0110", "0111", "0101", "0010", "0011", "0001")

ch1 <- ch2matrix(raw)                 # character vector
cbind(raw, ch1)
ch <- ch2matrix(factor(raw))          # factor
all(ch == ch1)
ch <- ch2matrix(as.numeric(raw))      # numeric
all(ch == ch1)
ch <- ch2matrix(matrix(raw, ncol=1))  # 1 column matrix
all(ch == ch1)
ch <- ch2matrix(data.frame(raw=raw))  # 1 column data frame
all(ch == ch1)

# Error messages:
try(ch2matrix(matrix(rep(raw, 2), ncol=2)))     # 2 column matrix -> error
try(ch2matrix(data.frame(raw=raw, dummy=raw)))  # 2 column data frame -> error

raw[2] <- "11000"          # wrong length
try(ch2matrix(raw))        # error
ch2matrix(as.numeric(raw)) # this works, leading zeros not expected.
}