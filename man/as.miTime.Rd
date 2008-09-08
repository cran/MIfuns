\name{as.miTime}
\alias{as.miTime}
\alias{as.miTime.default}
\alias{as.miTime.numeric}
\alias{as.miTime.miTime}
\title{ Create miTime Objects }
\description{
	\code{as.miTime} is generic, expecting character by default. 
	miTime is stored as numeric, and interpreted as fractional days.
	miTime objects can be constructed from real numbers of arbitrary
	precision, or from character strings with arbitrary formal elements
	(e.g. including seconds) but display of seconds is not currently 
	supported.
}
\usage{
	\method{as.miTime}{default}(x, format = "\%H:\%M", ...)
	\method{as.miTime}{numeric}(x, ...)
	\method{as.miTime}{miTime}(x, ...)
}

\arguments{
  \item{x}{ Typically numeric or character. }
  \item{format}{ A format string using \code{strptime} conventions. }
  \item{\dots}{ Extra arguments, typically ignored. }
}
\details{
	Typical usages are
	\preformatted{
		as.miTime(x, ...)
		as.miTime(x, format = "\%H:\%M", ...)
	}
	
  Comparisons are handled specially for class miTime.  Even though stored as a 
  numeric with arbitrary precision, comparisons (<, > , ==, etc) are based on the 
  information visible in the default format (i.e, nearest minute).  For finer 
  comparisons, coerce to numeric first.
}
\value{
  An object of the class miTime, useful for its display behaviors.
}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miDate}},
\code{\link{format.miTime}},
\code{\link{as.miDateTime}},
\code{\link{strftime}}
\code{\link{Ops.miDate}}
}
\examples{
as.miTime(c(0,0.5,1))
as.miTime("23:15")
as.miTime(0) == as.miTime(0.0001)#TRUE
as.miTime(0) < as.miTime(Inf)#TRUE
}
\keyword{ manip }
