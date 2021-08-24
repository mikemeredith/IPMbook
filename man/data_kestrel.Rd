\name{kestrel}
\alias{kestrel}
\encoding{UTF-8}
\docType{data}

\title{
Data for kestrels from Switzerland, 2002-2016
}
\description{
The data sets for kestrels (\emph{Falco tinnunculus}) were collected between 2002 and 2016 in the north-western quadrant of Switzerland.

They comprise breeding bird counts from a monitoring program and from the breeding bird atlas, and dead-recoveries of ringed individuals.
}

\usage{data(kestrel)}

\format{
\code{kestrel} is a list with 4 components:

\describe{
  \item{\bold{landData }}{landscape data, a data frame with rows for 15,734 quadrats (1 x 1km) in the study area and the following columns:
    \itemize{
      \item x and y : the coordinates of the center of the quadrat.
      \item elevation : mean elevation of the quadrat (m).
      \item lakes : proportion of the quadrat covered by water.
    }}}

\describe{
  \item{\bold{mhbData }}{data from Swiss Breeding Bird Monitoring (MHB, Schmid et al., 2004) for 120 quadrats in the study area, a list with the following components:
    \itemize{
      \item x and y : the coordinates of the center of the quadrat.
      \item tcount : a quadrats x years matrix, the annual total count of kestrels.
    }}}

\describe{
  \item{\bold{atlasData }}{data from the Swiss atlas survey (Knaus et al., 2018) for 574 atlas quadrats in the study area, a list with the following components:
    \itemize{
      \item x and y : the coordinates of the center of the quadrat.
      \item year : the year of the survey, 2013-2015.
      \item count : a quadrats x surveys matrix, the count of kestrels in the quadrat for up to 3 surveys; quadrats with only 2 surveys have NA in the third column.
    }}}

\describe{
  \item{\bold{drData }}{dead-recovery data from a total of 24,561 kestrels ringed at a nest-box in one of 7 ringing areas in the study area (Fay et al., 2019), a list with the following components:
    \itemize{
      \item site : a matrix with 24,561 rows and columns for site ID and mean x and y coordinates of the nest-boxes at the site.
      \item deadrecovery : an individual x year (2002-2016) matrix with 1 for the occasions of initial capture and dead recovery, 0 otherwise.
      \item age : a length 24,561 vector with the age ("juv" or "ad") of ringing.
    }}}
}

\source{Swiss Ornithological Institute}

\references{
Fay, R., Michler, S., Laesser, J., Schaub, M. (2019) Integrated population model reveals that kestrels breeding in nest boxes operate as a source population. \emph{Ecography} 42, 2122-2131.

Knaus, P., Antoniazza, S., Wechsler, S., Guelat, J., Kéry, M., Strebel, N., Sattler, T. (2018) \emph{Schweizer Brutvogelatlas 2013-2016. Verbreitung und Bestandsentwicklung der Vögel in der Schweiz und im Fürstentum Liechtenstein}, Sempach.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press. chapter 20.

Schmid, H., Zbinden, N., Keller, V. (2004) \emph{Überwachung der Bestandsentwicklung häufiger Brutvögel in der Schweiz}. Schweizerische Vogelwarte, Sempach.
}

\examples{
data(kestrel)
str(kestrel)
}
\keyword{datasets}
