\name{as.miDate.miDateTime}
\alias{as.miDate.miDateTime}
\title{Create miDate object from miDateTime}
\description{
  Create an miDate object from an miDateTime object.
}
\usage{
\method{as.miDate}{miDateTime}(x, ...)
}
\arguments{
  \item{x}{\code{x}}
  \item{\dots}{\code{\dots}}
}
\details{
  Extracts the 'date' component of miDateTime.
}
\value{
miDate
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miDate.numeric}},
\code{\link{as.miDateTime}}}
\examples{
function(x,...)as.miDate(as.numeric(x,...))
}
\keyword{manip}
