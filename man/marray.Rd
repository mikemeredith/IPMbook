\name{marray}
\alias{marray}
\encoding{UTF-8}

\title{
Converts capture-histories to an m-array for one age class
}
\description{
Creates an m-array for a single- or multistate capture-recapture data with one age class and optionally more than one group.
}
\usage{
marray(ch, unobs = 0, freq = 1, groups = NULL)
}
\arguments{
  \item{ch}{
an individuals x time matrix with single- or multistate capture histories (0: not captured; 1...X: captured in the 1...X states). See Details.
}
  \item{unobs}{
number of unobserved states (default is 0, needs to be given only in specific cases).
}
  \item{freq}{
a vector with the number of animals with each capture history, or a matrix with a column for each group. If a single value is supplied, it will be used for all rows in the capture history; the default is to assume each row corresponds to a single animal.
}
  \item{groups}{
a factor (or a vector that can be coerced to a factor) which identifies the group that each row of \code{ch} refers to. Ignored if \code{freq} is a matrix with > 1 column.
}
}

\details{
The argument \code{ch} can be a matrix of unique capture histories accompanied by a vector or matrix, \code{freq}, specifying the number of animals with each capture history. Trap losses can be indicated either by negative values for \code{freq}, or by filling the row with NA after the last capture. For other formats, see \code{\link{ch2matrix}}.
}

\value{
For single-state capture recapture data, an m-array which is a (years-1) x years x groups array, where element [i, j, g] contains the number of individuals in group g released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). If no groups are specified, this will be a (years-1) x years matrix. The last column contains the number of individuals released in year i and never recaptured.

For multi-state capture-recapture data with s states (including potential unobservable states), an m-array which is a (years-1)*s x (years-1)*2 + 1 x groups array. An element [i, j, g] contains the number of individuals in group g released in year t and state n (i = (t-1)*s + n) and recaptured in year k+1 in state m (j = (k-1)*s + m). If no groups are specified, this will be a matrix. The last column contains the number of individuals released in year t and state s and never recaptured. The labeling of unobserved states starts with the number of observed states + 1.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 4.5.1.2.
}

\examples{
# Convert a single state capture history matrix into an m-array
data(woodchat5)
dim(woodchat5$ch)  # 1902 animals x 20 years
ch1 <- cleanCH(woodchat5$ch[, 11:20])  # Use last 10 years only
marray(ch1)

# Convert a multistate capture history into an m-array
data(cormorant)
dim(cormorant$ms.ch) # 12,659 animals x 14 years
ch2 <- cleanCH(cormorant$ms.ch[, 11:14])  # Use last 4 years only
table(ch2) # 6 states
marray(ch2)
}
