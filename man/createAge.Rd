\name{createAge}
\alias{createAge}
\encoding{UTF-8}

\title{
Creates an age matrix
}
\description{
Creates an animals x intervals matrix which gives the age class for each individual at the beginning of each interval, based on the occasion of first capture and the age at first capture.
}
\usage{
createAge(f, age, nyears, mAge=2)
}
\arguments{
  \item{f}{
a vector of first capture occasions.
}
  \item{age}{
a corresponding vector with the age at first capture of each animal.
}
  \item{nyears}{
the number of years of the study; the number of intervals is \code{nyears - 1}.
}
  \item{mAge}{
the maximum age class; age is incremented by 1 each year up to \code{mAge}.
}

}
\value{
An individuals x intervals matrix with the age of each individual at the beginning of each interval. Cells prior to the first capture are filled with NAs.
}
\author{
Michael Schaub
}
\references{
Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Adademic Press, section 4.5.1.1.
}

\examples{
data(woodchat5)
f <- getFirst(woodchat5$ch)
# Remove animals first captured on the last occasion
last <- which(f==ncol(woodchat5$ch))
f <- f[-last]
age <- woodchat5$age[-last]

x <- createAge(f, age, 20, 2)
head(x)
x[115:120,]
tail(x)
}
