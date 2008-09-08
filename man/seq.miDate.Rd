\name{seq.miDate}
\alias{seq.miDate}
\title{Create a sequence of miDate}
\description{
  Utility to use seq on miDate without loss of class information.
}
\usage{
\method{seq}{miDate}(from, to,...)
}
\arguments{
  \item{from}{miDate; atomc}
  \item{to}{coercible to numeric (typ. miDate); atomic}
   \item{\dots}{Extra arguments passed to called functions.}
   }
\value{
vector of miDate
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miDate}},
\code{\link{seq}}}
\examples{
function(from,to)as.miDate(seq(as.numeric(from),as.numeric(to)))
}
\keyword{manip}
