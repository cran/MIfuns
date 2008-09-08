\name{as.miTime.miDateTime}
\alias{as.miTime.miDateTime}
\title{Create miTime object from miDateTime object}
\description{Create miTime object from miDateTime object}
\usage{
\method{as.miTime}{miDateTime}(x, ...)
}
\arguments{
  \item{x}{miDateTime}
  \item{\dots}{extra argmuments, passed to as.numeric}
}
\details{
  This function extracts the 'time' component of miDateTime.
}
\value{
miTime
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miTime.numeric}},
\code{\link{as.miDateTime}}}
\examples{
function(x,...)as.miTime(as.numeric(x \%\% 1,...))
}
\keyword{manip}
