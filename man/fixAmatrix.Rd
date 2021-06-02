\name{Fix matrices}
\alias{fixAmatrix}
\alias{abbreviated}
\encoding{UTF-8}

\title{ Quick and dirty specification of matrices }

\description{

Many of the arguments to functions in the IPMbook package are in the form of matrices, eg, age classes x time. Entering the full matrix is tedious and difficult to read. In many cases, the parameter will be time-invariant or several age classes will share the same values, when input can be abbreviated.

If all values are equal, a single value can be entered and all elements of the matrix will be set to this.

For a time-invariant parameter, a vector can be entered, and this will be plugged in to all columns of the matrix. The vector can be shorter than the number of age classes, when the last value will be replicated for the remaining classes.

If the parameter varies with time, a matrix must be entered, but it does not need a row for every age class. If the matrix has fewer rows than the number of age classes, the last row will be replicated for the remaining classes.

In most cases, the full matrix is included in the output of the function: check this to make sure the expansion has worked as expected.
}

\keyword{ manip }

