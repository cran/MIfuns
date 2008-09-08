\name{rep.miTime}
\alias{rep.miTime}
\title{Replicate miTime}
\description{
  Utility to replicate miTime without losing class information
}
\usage{
\method{rep}{miTime}(x, ...)
}
\arguments{
  \item{x}{miTime}
  \item{\dots}{ignored arguments}
}
\value{
miTime
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miTime}},
\code{\link{rep}}}
\examples{
function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
 }
}
\keyword{manip}
