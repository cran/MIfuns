\name{as.miDate}
\alias{as.miDate}
\alias{as.miDate.default}
\alias{as.miDate.numeric}
\title{ Create miDate Objects }
\description{
	\code{as.miDate} is generic.  The default method is just a
	wrapper for \code{as.Date()} that adds 'miDate' to the classpath.
	The method for numeric simply coerces the class to 'Date' and then
	calls the generic.
}
\synopsis{
as.miDate(x, ...)
as.miDate.default(x, ...)
as.miDate.numeric(x, ...)
}
\usage{
as.miDate(x, ...)
}
\arguments{
  \item{x}{ Numeric, character, or any thing accepted by as.Date(). }
  \item{\dots}{ Extra arguments passed to as.Date(), especially 'format'. }
}
\details{
  'Date' is represented as number of days since 1970-01-01, so numerics
  are converted to miDates that are interpreted accordingly. When
  constructing from character, a format argument can be passed, as per 
  \link{as.Date}.
  
  Comparisons are handled specially for class miDate.  Even though stored as a 
  numeric with arbitrary precision, comparisons (<, > , ==, etc) are based only
  on the information visible in the default format (the "day" portion).
}
\value{
  An object of the class miDate, useful for its display behaviors.
}
\seealso{
\code{\link{format.miDate}},
\code{\link{as.miTime}},
\code{\link{as.miDateTime}}, 
\code{\link{strftime}},
\code{\link{Ops.miDate}},
}
\examples{
	as.miDate(1)
	as.miDate("6-7-2007",format="\%m-\%d-\%Y")
	as.miDate(1) == as.miDate(1.5)#TRUE
	as.miDate(1) < as.miDate(Inf)#TRUE
}
\keyword{ manip }

