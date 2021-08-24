\name{elk}
\alias{elk}
\encoding{UTF-8}
\docType{data}

\title{
Data from an elk population in USA, 1988-1993
}
\description{
All three data sets from an elk (\emph{Cervus canadensis}) population are from Northern Idaho, 1988 to 1993, and include only females, i.e., the elk cows.

The age-at-harvest data summarize the number of elk cows harvested and reported in each year stratified by age. All animals reported are sexed and aged based on tooth inspection, discarding records of elk older than 18 years. Not all hunted animals are reported.

The reporting rate was assessed by a telephone survey of registered hunters, recording in each year the number who said they had killed an elk cow and the number of hunters interviewed.

Some elk cows were radio-tracked each year, and for these we have data for survival, hunting mortality and other mortality.
}

\usage{data(elk)}

\format{
\code{elk} is a list with 3 matrices:

\describe{
  \item{C }{age-at-harvest data from cow elk, 17 age classes x 6 years.}
  \item{H }{hunter survey results, 2 rows x 6 years.}
  \item{R }{data for radio-tagged elk, 3 categories x 6 years.}
}
}

\source{Idaho Department of Fish and Game, see Gove et al. (2002)}

\references{
Gove, N. E., Skalski, J. R., Zager, P., Townsend, R. L. (2002) Statistical models for population reconstruction using age-at-harvest data. \emph{Journal of Wildlife Management} 66, 310-320.

Schaub, M., Kéry, M. (2022) \emph{Integrated Population Models}, Academic Press, chapter 17.
}

\examples{
data(elk)
str(elk)
}
\keyword{datasets}
