\name{as.miDateTime}
\alias{as.miDateTime}
\alias{as.miDateTime.default}
\alias{as.miDateTime.numeric}
\alias{as.miDateTime.miDate}
\alias{as.miDateTime.miDateTime}
\title{ Create Objects of Class miDateTime }
\description{
	\code{as.miDateTime} is generic.  The class is stored internally
	as numeric, interpreted as decimal days (fractional portion is time).
	Display of seconds is not currently supported.
}

\usage{
	\method{as.miDateTime}{default}(x,format,\dots)
	\method{as.miDateTime}{numeric}(x,\dots)
	\method{as.miDateTime}{miDate}(x,y,\dots)
	\method{as.miDateTime}{miDateTime}(x,\dots)
}
\arguments{
  \item{x}{ character, numeric, or miDate. }
  \item{format}{format string }
  \item{y}{ miTime, when x is miDate. }
  \item{\dots}{Extra arguments, such as 'format' (see \code{strptime}.}
}
\details{
  Typical usages are
  \preformatted{
  as.miDateTime(x, ...)
  as.miDateTime(x,format="\%m/\%d/\%Y \%H:\%M",...)
  as.miDateTime.numeric(x, ...)
  as.miDateTime.miDate(x, y, ...)
  as.miDateTime.miDateTime(x, ...)
  }
  The default method coerces to character, and applies a format string using 
  strftime() syntax.

  Comparisons are handled specially for class miDateTime.  Even though stored as a 
  numeric with arbitrary precision, comparisons (<, > , ==, etc) are based on the 
  information visible in the default format (i.e, nearest minute).  For finer 
  comparisons, coerce to numeric first.
}
\value{
  Object of class miDateTime, useful for its display behaviors.
}
\author{Tim Bergsma}
\seealso{
	\code{\link{as.miDate}},
	\code{\link{as.miTime}},
	\code{\link{format.miDateTime}},
	\code{\link{strftime}}
	\code{\link{Ops.miDate}}
}
\examples{
as.miDateTime("06/07/2007 23:15")
as.miDateTime("07-06-07 23:15:00",format="\%y-\%m-\%d \%H:\%M:\%S")
as.miDateTime(1.5)
as.miDateTime(as.miDate(1), as.miTime(0.5))
as.miDateTime(0) == as.miDateTime(0.0001)#TRUE
as.miDateTime(0) < as.miDateTime(Inf)#TRUE
}
\keyword{ manip }

