\name{reapply}
\alias{reapply}
\title{Calculate Cell Aggregates Once per Cell Member}
\description{Calls \code{tapply} but stretches the result to dimensions of \code{x}.}
\usage{
reapply(x, INDEX, FUN, ...,simplify=TRUE)
}
\arguments{
  \item{x}{an atomic object, typically a vector}
  \item{INDEX}{list of factors, each of same length as \code{x}}
  \item{FUN}{the function to be applied. In the case of functions like \code{+}, \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}}
  \item{simplify}{passed to \code{tapply}}
}
\details{
The function \code{tapply} applies \code{FUN} to each cell of a vector, as 
specified by levels of \code{INDEX}. \code{reapply} repeats that result as necessary
to match the number of input elements per cell, and restores the order to that
of the original index.  Regardless of the length of the value of \code{FUN}, the
length of the value of reapply is always identical to that of \code{x}.
}
\value{vector}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\examples{
theoph <- Theoph[c(1,12,2,13,3,14),]
theoph$avg <- with(theoph,reapply(conc,Subject,mean))
theoph$sum <- with(theoph,reapply(conc,Subject,cumsum))
theoph
}
\keyword{manip}

