\name{woodchat11}
\alias{woodchat11}
\encoding{UTF-8}
\docType{data}

\title{
Real data for Woodchat Shrike for use in Chapter 11
}
\description{
Real data for Woodchat Shrike \emph{Lanius senator} in orchards in southern Germany near Stuttgart from 1964 to 1992.
}

\usage{data("woodchat11")}

\format{
\code{woodchat11} is a list with 6 components:

\describe{
  \item{ch }{capture histories for 1079 females x 29 years.}
  \item{age }{the age of each individual at first capture.}
  \item{C }{the annual number of breeding pairs found in the study area.}
  \item{f }{the number fledglings (male or female) produced by 365 broods monitored.}
  \item{d }{d = 0 if \code{f} is an accurate count, d = 1 if it is a lower bound.}
  \item{year }{the year the brood was monitored.}
  }
}

\source{
Ullrich (2017)
}
\references{
Ullrich, B., 2017. Entdeckung und Aussterben einer Population des Rotkopfwürgers \emph{Lanius senator} in den Streuobstwiesen des Mittleren Albvorlandes, Landkreise Göppingen und Esslingen am Neckar. \emph{Ökologie der Vögel} 35, 273–335
}
\examples{
data(woodchat11)
str(woodchat11)
}
\keyword{datasets}
