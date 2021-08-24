\name{bats}
\alias{bats}
\encoding{UTF-8}
\docType{data}

\title{
Data for greater horseshoe bats from Switzerland, 1989-2017
}
\description{
Data for greater horseshoe bats (\emph{Rhinolophus ferrumequinum}) were collected from 1989 to 2017 from a nursery colony in a church in the Swiss Alps (Valais).

Bats were captured and marked resulting in capture-recapture data, but in some years no capture was performed. Every year the number of adults was counted when they leave the colony at dusk and the number of newborn was recorded in the colony.
}

\usage{data(bats)}

\format{
\code{bats} is a list with 7 components:

\describe{
  \item{ch }{matrix of capture histories for 574 bats x 29 years.}
  \item{age }{the age of each bat at first capture, 1 = juvenile, 2 = 1-year old, 3 = at least 2 years old.}
  \item{sex }{sex of bats captured, 1 = female, 2 = male.}
  \item{J.count }{annual counts of newborn bats in the roost; in most years these were complete counts.}
  \item{Jm }{number of newborn bats recorded as male (not all newborns captured were sexed).}
  \item{Jf }{number of newborn bats recorded as female.}
  \item{A.count }{annual number of adults counted when the bats leave the colony at dusk; exit counts were not done in 1991 and 2001.}
  }
}

\source{Raphaël Arlettaz, François Biollaz, Antoine Sierro}

\references{
Schaub, M., Gimenez, O., Sierro, A., Arlettaz, R. (2007) Use of integrated modeling to enhance estimates of population dynamics obtained from limited data. \emph{Conservation Biology} 21, 945-955.

Sierro, A., Lugon, A., Arlettaz, R. (2009) La colonie de grands rhinolophes \emph{Rhinolophus ferrumequinum} de l'église St-Sylvie à Vex (Valais, Suisse): évolution sur deux décennies (1968-2006). \emph{Le Rhinolophe} 18, 75-82.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 13.
}

\examples{
data(bats)
str(bats)
}
\keyword{datasets}
