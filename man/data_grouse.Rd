\name{grouse}
\alias{grouse}
\encoding{UTF-8}
\docType{data}

\title{
Data for black grouse from Italy, 1997-2016
}
\description{
Data for black grouse (\emph{Lyrurus tetrix}) from Alpe Devero in the Piemont region of the Alps in Northern Italy from 1997 to 2016.

The number of displaying cocks was recorded each spring. In late summer, grouse were looked for with the help of pointer dogs and counted. Some grouse were equipped with a radio transmitter and the status of these birds (alive, dead) was regularly recorded.
}

\usage{data(grouse)}

\format{
\code{grouse} is a list with 10 components:

\describe{
  \item{ch }{matrix of capture histories for 96 radio-tagged birds x 128 months: 1 if found alive, 0 if found dead, otherwise NA.}
  \item{age }{the actual age of each bird: 1 = juvenile, 2 = adult.}
  \item{sex }{the sex of each bird: 1 = female, 2 = male.}
  \item{season }{the season of the survey: 1 = spring, 2 = summer, 3 = autumn, 4 = winter.}
  \item{count.sp }{annual count of displaying males at leks in spring.}
  \item{count.lsM }{annual count of flushed adult males in late summer.}
  \item{count.lsF }{annual count of flushed adult females in late summer.}
  \item{count.lsC }{annual count of flushed chicks in late summer.}
  \item{v }{annual total number of chicks in families where chicks could be sexed.}
  \item{u }{annual number of female chicks in families where chicks could be sexed.}
  }
}

\source{Luca Rotelli, Radames Bionda}

\references{
Rotelli, L., Zbinden, N., Bionda, R., Schaub, M. (in prep.) Chick survival and hunting are important drivers for the dynamics of two Alpine black grouse (\emph{Lylurus tetrix}) populations. \emph{Wildlife Biology}.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 15.

}

\examples{
data(grouse)
str(grouse)
}
\keyword{datasets}
