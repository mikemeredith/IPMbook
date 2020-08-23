\name{cormorant}
\alias{cormorant}
\encoding{UTF-8}
\docType{data}

\title{
Data from three Danish cormorant breeding colonies
}
\description{
The data were collected during 14 years from 1991 to 2004 at the three Danish cormorant breeding colonies located in the southwestern Kattegat in the inner Danish waters: Vorsø (labelled V), Mågeøerne (M) and Stavns Fjord (S).

Counts:  The number of active nests in each colony was counted each year in early May.

Resightings: In all colonies, a large number of nestlings were marked with a standard metal ring on one leg and an alphanumeric colored plastic ring on the other leg; in total 12,659 individuals were marked. Resightings of marked individuals at the breeding colonies by trained field workers from towers, hides or from the ground using a telescope were recorded. Resightings were restricted to breeding individuals. Marking of nestlings already started before 1991, hence some individuals in the data set look as if they had been ringed as breeders.
}

\usage{data("cormorant")}

\format{
\code{cormorant} is a list with 2 components:

\describe{
  \item{count }{matrix of counts of active nests in 3 breeding colonies x 14 years.}
  \item{ms.ch }{a multistate capture history matrix for 12,659 birds x 14 years. The state codes 1-3 index nestlings in colonies V, M and S, respectively, and the state codes 4-6 do the same for the breeders.}
  }
}

\source{National Environmental Research Institute, Denmark ??}

\references{
Borysiewicz, R. S., Morgan, B. J. T., Hénaux, V., Bregnballe, T., Lebreton, J. D., Gimenez, O., 2009.An integrated analysis of multisite recruitment, mark-recapture-recovery and multisite census data.Pages 579–591 in D. Thomson, E. Cooch and M. Conroy, editors. \emph{Modeling demographic processes in marked populations}.Springer, New York

McCrea, R. S., Morgan, B. J. T., Gimenez, O., Besbeas, P., Bregnballe, T., Hénaux, V., Lebreton, J. D., 2010. Multi-site integrated population modelling. \emph{JABES} 15, 539–561.

Schaub, M. & Kery, M., in prep., \emph{Integrated Population Modeling}, chapter 20.
}

\examples{
data(cormorant)
str(cormorant)
}
\keyword{datasets}
