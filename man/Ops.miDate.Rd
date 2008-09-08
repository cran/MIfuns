\name{Ops.miDate}
\alias{Ops.miDate}
\alias{Ops.miDateTime}
\alias{Ops.miTime}
\title{ Limit temporal comparisons to displayed precision. }
\description{
 miDate, miDateTime, and miTime store values with arbitrary precision, yet typically
 display fixed precision (days or minutes). The 'Ops' functions ensure that 
 comparisons (<, >, ==, etc.) are based on the displayed precision so that 
 behavior is consistent with appearance.  Finer comparisons are possible by converting
 to numeric.
 
 The 'Ops' functions are not normally called by the user.
}
\usage{
	\method{Ops}{miDate}(e1, e2)
	\method{Ops}{miDateTime}(e1, e2)
	\method{Ops}{miTime}(e1, e2)
}
\arguments{
  \item{e1}{ first argument }
  \item{e2}{ second argument }
}
\details{
  When a comparison is made, the arguments are literally converted to their character
  representations and then back to temporal classes.  Elements that can't be 
  represented in the default formats (NA, Inf, -Inf, others?) are left untouched.
}
\value{
  logical
}
\author{Tim Bergsma }
\seealso{
\code{\link{as.miDate}},
\code{\link{as.miDateTime}},
\code{\link{as.miTime}},
}
\examples{
## Ops.miDate is currently defined as
function(e1,e2){
	f1 <- as.miDate(as.character(e1),format="\%m/\%d/\%Y")
	f2 <- as.miDate(as.character(e2),format="\%m/\%d/\%Y")
	e1[!is.na(f1)] <- f1[!is.na(f1)]
	e2[!is.na(f2)] <- f2[!is.na(f2)]
	NextMethod()
  }
## The other two are similar.
}
\keyword{ manip }

