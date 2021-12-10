\name{woodchat11}
\alias{woodchat11}
\encoding{UTF-8}
\docType{data}

\title{
Data for woodchat shrike from Germany, 1964-1992
}
\description{
Real data for the woodchat shrike (\emph{Lanius senator}) were collected from 1964 to 1992 from a study area in southern Germany.

They comprise the annual number of breeding pairs, information about productivity from regular checks of nests, and capture, ringing and resighting of juvenile and adult shrikes. In some years, no resighting data were collected.
}

\usage{data(woodchat11)}

\format{
\code{woodchat11} is a list with 6 components:

\describe{
  \item{ch }{capture histories for 1079 individuals (males and females) x 29 years.}
  \item{age }{the age of each individual at first capture, 1 = nestling (juvenile), 2 = at least 1 year old (adult).}
  \item{count }{the annual number of breeding pairs found in the study area.}
  \item{f }{the number fledglings (male or female) produced by 365 broods monitored.}
  \item{d }{d = 0 if \code{f} is an accurate count, d = 1 if it is a lower bound.}
  \item{year }{the year the brood was monitored.}
  }
}

\source{
Bruno Ullrich
}
\references{
Ullrich, B. (2017) Entdeckung und Aussterben einer Population des Rotkopfwürgers \emph{Lanius senator} in den Streuobstwiesen des Mittleren Albvorlandes, Landkreise Göppingen und Esslingen am Neckar. \emph{Ökologie der Vögel} 35, 273-335.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, section 11.4.

Schaub, M., B. Ullrich (2021) A drop in immigration results in the extinction of a local woodchat shrike population. \emph{Animal Conservation} 24, accepted <https://doi.org/10.1111/acv.12639>
}
\examples{
data(woodchat11)
str(woodchat11)
}
\keyword{datasets}
