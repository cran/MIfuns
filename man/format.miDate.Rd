\name{format.miDate}
\alias{format.miDate}
\alias{format.miTime}
\alias{format.miDateTime}
\alias{print.miTime}
\alias{print.miDateTime}
\alias{as.character.miTime}
\alias{as.character.miDateTime}
\title{ Visualization of mi Objects}
\description{
  Display miDate, miTime, and miDateTime objects appropriately.
}
\usage{
	\method{format}{miDate}(x, ...)
	\method{format}{miTime}(x, ...)
	\method{format}{miDateTime}(x, ...)
	\method{print}{miTime}(x, ...)
	\method{print}{miDateTime}(x, ...)
	\method{as.character}{miTime}(x, ...)
	\method{as.character}{miDateTime}(x, ...)
}

\arguments{
  \item{x}{ Object to be displayed. }
  \item{\dots}{ Extra arguments, ignored. }
}
\details{
  These functions implement the character representations of dates and times
  that NMTRAN expects.  \code{as.character} and \code{print} just call \code{format},
  the latter passing quote=FALSE.  \code{format.miDateTime()} just calls 
  \code{format.miDate} and \code{format.miTime()}, concatenating their results.  
  \code{format.miDate}calls \code{format.Date()}, passing format="\%m/\%d/\%Y".  
  \code{format.miTime()} prints two-digit hours and minutes separated by a colon.
}
\value{
  Character.
}
\author{ Tim Bergsma}

\seealso{  \code{\link{as.miDate}},\code{\link{c.miTime}}}
\examples{
function(miDate,...){
as.character(as.miDateTime(as.miDate(1),as.miTime(.5)))
print(as.miDateTime(as.miDate(1),as.miTime(.5)))
  }
}
\keyword{ manip }

