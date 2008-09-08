\name{c.miTime}
\alias{c.miTime}
\alias{c.miDateTime}
\alias{[.miTime}
\alias{[.miDateTime}
\alias{[[.miTime}
\alias{[[.miDateTime}
\alias{[<-.miTime}
\alias{[<-.miDateTime}
\title{ Manipulate miTime or miDateTime }
\description{
  Combine, subset, select from, or assign to subsets of miTime or miDateTime.
}
\usage{
	\method{c}{miTime}(..., recursive = FALSE)
	\method{c}{miDateTime}(..., recursive = FALSE)
	\method{[}{miTime}(x, ..., drop = TRUE)
	\method{[}{miDateTime}(x, ..., drop = TRUE)
	\method{[[}{miTime}(x, ..., drop = TRUE)
	\method{[}{miDateTime}(x, \dots) <- value
	\method{[}{miTime}(x, \dots) <- value
	\method{[[}{miDateTime}(x, ..., drop = TRUE)
}
\arguments{
  \item{\dots}{ Extra arguments, typically ignored.}
  \item{x}{ Object to be manipulated }
  \item{recursive}{ Conventional meaning. }
  \item{drop}{ Conventional meaning.}
  \item{value}{ Conventional meaning. }
}
\details{
  These functions appear to be necessary to prevent miTime and miDateTime
  objects from losing class information during manipulation.  Apparently miDate
  is handled by 'Date' methods.  In most cases, the user will not call these directly.
}
\value{
  Objects of class miTime or miDateTime.
}
\author{ Tim Bergsma}

\seealso{  \code{\link{as.miDate}},\code{\link{format.miDate}}, }
\examples{
t <- as.miTime(c(0,0.33, 0.66, 1))
dt <- as.miDateTime(c(1,2,3,4))
c(t,t)
c(dt,dt)
t[1]
dt[1]
t[[1]]
dt[[1]]
t[2] <- 1
dt[2] <- 1
}
\keyword{ manip }

