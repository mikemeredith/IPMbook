\name{hoopoe}
\alias{hoopoe}
\encoding{UTF-8}
\docType{data}

\title{
Data for hoopoe
}
\description{
Data for hoopoe \emph{Upupa epops} from the Valais valley in Switzerland from 2002 to 2017.
}

\usage{data("hoopoe")}

\format{
\code{hoopoe} is a list with 10 components:

\describe{
  \item{ch }{capture histories for 3,844 individuals x 16 years.}
  \item{age }{the age of each individual at first capture, 1 = nestling, 2 otherwise.}
  \item{count }{the annual population index.}
  \item{B1, B2 }{annual number of mothers for which fledglings were recorded: 1-year-old (B1) and older females (B2).}
  \item{J1, J2 }{annual number of fledglings produced by 1-year-old (J1) and older mothers (J2).}
  \item{id }{id of mothers of 1092 broods monitored.}
  \item{fl }{number of fledglings in each brood.}
  \item{year }{year of the brood.}
  }
}

\source{Arlettaz et al. 2010, Schaub et al. 2012}

\references{
Arlettaz, R., M. Schaub, J. Fournier, T. S. Reichlin, A. Sierro, J. E. M. Watson, and V. Braunisch. 2010. From Publications to Public Actions: When Conservation Biologists Bridge the Gap between Research and Implementation. \emph{BioScience} 60:835–842.

Schaub, M., T. S. Reichlin, F. Abadi, M. Kéry, L. Jenni, and R. Arlettaz. 2012. The demographic drivers of local population dynamics in two rare migratory birds. \emph{Oecologia} 168:97–108.
}

\examples{
data(hoopoe)
str(hoopoe)
}
\keyword{datasets}
