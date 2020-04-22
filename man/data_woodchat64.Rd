\name{woodchat64}
\alias{woodchat64}
\encoding{UTF-8}
\docType{data}

\title{
Simulated data for Woodchat Shrike for use in section 6.4
}
\description{
Simulation of data with demographic parameters based on the Woodchat Shrike are introduced in section 3.3. This data set has additional simulated data for use in chapter 6.4.
}

\usage{data("woodchat64")}

\format{
\code{woodchat64} is a list with 6 components:

\describe{
  \item{ch }{capture histories for 947 individuals ?? females x 10 years.}
  \item{age }{the age of each individual at first capture.}
  \item{count }{number of breeding pairs recorded in each year ??.}
  \item{J }{??.}
  \item{R }{??.}
  \item{trueN }{??.}
  }
}

\source{
Simulated data.
}
\examples{
data(woodchat64)
str(woodchat64)
}
\keyword{datasets}
