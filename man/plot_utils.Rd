\name{Plotting functions}
\alias{corner.label}
\encoding{UTF-8}

\title{
A patched version of \code{plotrix::corner.label}.
}
\description{
Like \code{plotrix::corner.label}, finds the coordinates in user parameters of a specified corner of the figure region and optionally displays a label there. The \code{plotrix} function has a bug and does not pass on graphical parameters in the '...' argument to the \code{text} function. That bug is fixed here.

This function will be removed when the next version of \code{plotrix} is uploaded to CRAN.
}
\usage{
corner.label(label=NULL, x=-1, y=1, xoff=NA, yoff=NA, figcorner=FALSE,...)
}
\arguments{
  \item{label}{
Text to display. The default is to display nothing.
}
  \item{x}{
an integer value: -1 for the left side of the plot, 1 for the right side.
}
  \item{y}{
an integer value: -1 for the bottom side of the plot, 1 for the top side
}
  \item{xoff, yoff}{
horizontal and vertical text offsets. Defaults to one half of the width and height of "m" respectively.
}
  \item{figcorner}{
whether to find/display at the corner of the plot or figure.
}
  \item{\dots}{
graphical arguments passed to the \code{text} function.
}

}
\value{
A list of the x and y positions of the corner adjusted for the offsets.
}
\author{
Ben Bolker
}

\examples{
plot(1:10,1:10)
corner.label("A")
corner.label(x=1,y=1)
corner.label("B",y=-1,x=1,figcorner=TRUE,col="red")
}

\keyword{
aplot
}
