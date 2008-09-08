\name{panel.covplot}
\alias{as.double.immutable}
\alias{panel.covplot}
\alias{prepanel.covplot}

\title{Control Plotting of Default "Forest Plots"}
\description{
prepanel.covplot() calls the default stats function to set up 'xlim'.
panel.covplot() lays out the basic form of a "Forest Plot", and calls panel.bwplot().
as.double.immutable() (called by panel.covplot()) prevents as.double() from stripping
names from the values of x.
}
\usage{
	\method{as.double}{immutable}(x, ...)
	panel.covplot(x, ref, rlim = NULL, rcol = "grey90", pcol = "green", ...)
	prepanel.covplot(x,...)
}
\arguments{
  \item{x}{An immutable numeric vector to be plotted.}
  \item{ref}{Where to put the reference line in native units of \code{x}.}
  \item{rlim}{The lower and upper limit of the shaded region in native units,
  or a fraction of \code{ref} by which these limits differ from \code{ref}.}
  \item{rcol}{The shading color.}
  \item{pcol}{Box/whisker color for the primary element (left side of formula).}
  \item{\dots}{Extra arguments, passed to panel.bwplot().}
}
\details{
panel.covplot() stacks box/whisker elements top-down in the order specified in
the right side of the formula passed to bwplot().  The element matching the 
left side of the formula (if present) is plotted in a different color. Elements
representing categorical covariates (z is atomic) have an invisble box (with
zero width, for covplot.stats()). A suitable default will have been passed for ref,
and a vertical line will be drawn there.  A reference region will be shaded, with
limits (rlim) specified either in absolute terms, or as a fraction (+/-) of ref.
Boxes and whiskers are controlled by the stats function, typically covplot.stats().
}
\value{
as.double.immutable() returns its argment.  prepanel.covplot() returns xlim.  
panel.covplot() is used for its side effects.
}
\author{Tim Bergsma}
\seealso{
	\code{\link{as.distribution}},
	\code{\link{bwplot.distribution}},
	\code{\link{covplot.stats}}
	\code{\link{bwplot}}
}

\keyword{manip}

