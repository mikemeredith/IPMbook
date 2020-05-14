\name{grouse}
\alias{grouse}
\encoding{UTF-8}
\docType{data}

\title{
Data for grouse
}
\description{
Data for black grouse \emph{Lyrurus tetrix} from Alpe Devero in the Piemont region of the Alps in Northern Italy from 1997 to 2016.
}

\usage{data("grouse")}

\format{
\code{grouse} is a list with 10 components:

\describe{
  \item{ch }{matrix of capture histories for 96 radio-tagged birds x 128 months: 1 if found alive, 0 if found dead, otherwise NA.}
  \item{age }{the age of each bird: 1 juvenile, 2 adult.}
  \item{sex }{the sex of each bird: 1 female, 2 male.}
  \item{season }{the season of the survey: 1 spring, 2 summer, 3 autumn, 4 winter.}
  \item{count.sp }{annual count of displaying males at leks in spring.}
  \item{count.lsM }{annual count of flushed adult males in late summer.}
  \item{count.lsF }{annual count of flushed adult females in late summer.}
  \item{count.lsC }{annual count of flushed chicks in late summer.}
  \item{v }{annual total number of chicks in families where chicks could be sexed.}
  \item{u }{annual number of female chicks in families where chicks could be sexed.}
  }
}

\source{Luca Rotelli and colleagues, Rotelli et al (in prep.)}

\references{
Rotelli, L., Zbinden, N., Bionda, R., Schaad, M. (in prep.) Dynamics of two black grouse populations are driven by the variation of chick survival. \emph{Wildlife Biology}.
}

\examples{
data(grouse)
str(grouse)
}
\keyword{datasets}
