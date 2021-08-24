\name{bear}
\alias{bear}
\encoding{UTF-8}
\docType{data}

\title{
Data for black bears in Louisiana, USA, 2007-2012
}
\description{
The data on the Louisiana black bear (\emph{Ursus americanus luteolus}) were collected during six years (2007-2012) in the Upper Atchafalaya River Basin in South-central Louisiana, USA, using hair snares. Hair samples were collected during eight consecutive weeks in each year and a selection of samples were genotyped resulting in spatial capture-recapture (SCR) data. This data set includes SCR data for 2007, 2009 and 2011, plus simple occupancy data for 2008, 2010 and 2012. See Chandler and Clark (2014) for details.

Note that not all hair samples were successfully genotyped, and traps were not deployed at all 128 locations in every year, leading to NAs in the data.
}

\usage{data(bear)}

\format{
\code{bear} is a list with 3 components:

\describe{
  \item{scr }{an array with SCR data, individuals x traps x occasions x years.}
  \item{occ }{an array with detection/nondetection data, locations x occasions x years.}
  \item{trap.coord }{a 2-column matrix with the coordinates of the trap locations.}
  }
}

\source{Louisiana Department of Wildlife and Fisheries}

%%"We also wish to acknowledge C. Lowe and K. O'Connell for graciously allowing the use of data that they and many technicians collected in the field."
%%The contact would be Maria Davidson, Large Carnivore Program Manager, Louisiana Department of Wildlife and Fisheries, 646 Cajundome Blvd. Suite 127, Lafayette, LA 70506

\references{
Chandler, R. B., Clark, J. D. (2014) Spatially explicit integrated population models. \emph{Methods in Ecology and Evolution} 5, 1351-1360.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 21.
}

\examples{
data(bear)
str(bear)
}
\keyword{datasets}
