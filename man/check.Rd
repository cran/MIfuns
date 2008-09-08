\name{check}
\alias{maxChar}
\alias{prev}
\alias{runhead}
\alias{sorted}
\title{Check Some Properties of Vectors and Data Frames (beta)}
\description{
maxChar() returns the number of printed characters for the widest element of x.
prev() calculates the previous element for each element in x.
runhead() determines whether each element is the "head" (start) of a run
sorted() checks whether x (data.frame) is sorted on the columns specified.
}
\usage{
maxChar(x)
prev(x)
runhead(x)
sorted(x, on)
}
\arguments{
  \item{x}{a data.frame for sorted(); otherwise a vector.}
  \item{on}{a character vector of column names in x.}
}
\details{
If you forget to round() or signif() a numeric column, you may get more digits
than desired in your output file; maxChar() can warn you.  See examples.

prev() is used by runhead. Note that there is no element previous to the first
element in a vector, so prev() returns NA in that position.

If a 'run' is a sequence (possibly of length one) of identical successive values
in a vector, runhead determines whether an element is the first in such a sequence.
Note that by definition, the first element is the start of a run; Thus runhead()
returns TRUE in that position, even though prev() returns NA.
}
\value{
maxChar(): an atomic integer
prev(): a vector of the same class
runhead(): a vector of logicals
sorted(): an atomic logical
}
\author{Tim Bergsma}
\note{
NAs in the argument to runhead() give surprising but reasonable results. It cannot
be known whether they are the heads of runs, nor can it be known whether values
immediately following them are heads of runs.  To treat NAs deterministically,
convert to some definite value first.
}
\examples{
maxChar(c(1.2,1.234))
prev(c(1,2,NA,3,3,NA,4))
runhead(c(1,2,NA,3,3,NA,4))
sorted(Theoph,on="Subject")#FALSE: note ordered factor.
}

\keyword{manip}

