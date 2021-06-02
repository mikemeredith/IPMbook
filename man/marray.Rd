\name{marray}
\alias{marray}
\encoding{UTF-8}

\title{
Converts capture-histories to an m-array for one age class
}
\description{
Create an m-array for a single- or multistate capture-recapture data with one age class.
}
\usage{
marray(ch, unobs = 0)
}
\arguments{
  \item{ch}{
An individuals x time matrix with single- or multistate capture histories (0: not captured; 1...X: captured in the 1...X states).
}
  \item{unobs}{
Number of unobserved states (default is 0, needs to be given only in specific cases).
}

}
\value{
For single-state capture recapture data, an m-array which is a (years-1) x years matrix, where element [i, j] contains the number of individuals released in year i and recaptured in year j+1 (by definition no recaptures can occur in year 1). The last column contains the number of individuals released in year i and never recaptured.

For multi-state capture-recapture data with s states (including potential unobservable states), an m-array which is a (years-1)*s x (years-1)*2 + 1 matrix. An element [i, j] contains the number of individuals released in year t and state n (i = (t-1)*s + n) and recaptured in year k+1 in state m (j = (k-1)*s + m). The last column contains the number of individuals released in year t and state s and never recaptured. The labeling of unobserved states starts with the number of observed states + 1.
}
\author{
M. Schaub.
}
\references{
Schaub, M. & Kéry, M. (2021) \emph{Integrated Population Models}, Academic Press, section 4.5.1.2.
}

\examples{
# Convert a single state capture history matrix into an m-array
data(woodchat5)
dim(woodchat5$ch)  # 1902 animals x 20 years
( marr <- marray(woodchat5$ch) )
str(marr) # 19 years-of-release x 20 (= 19 years-of-recaptures + never-recaptured)

# Convert a multistate capture history into an m-array
data(cormorant)
dim(cormorant$ms.ch) # 12,659 animals x 14 years
table(cormorant$ms.ch) # 6 states
( marr <- marray(cormorant$ms.ch) )
dim(marr) # 78 (= 13 years-of-release * 6 states) x
          #   79 (= 13 years-of-recapture * 6 states + never-captured)
}
