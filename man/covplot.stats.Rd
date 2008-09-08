\name{covplot.stats}
\alias{covplot.stats}


\title{Calculate Statistics for "Forest Plots"}
\description{
covplot.stats() is a replacement for boxplot.stats(), and serves as the default
'stats' argument for bwplot.distribution(), called via boxplot.prestats().
}
\usage{
covplot.stats(x,y,z,influence,coef,do.out=TRUE,transform="x*1",file=NULL,...)
}
\arguments{
  \item{x}{Numeric vector of bootstrap estimates of a single parameter.}
  \item{y}{A numeric value, possibly named, representing an estimate of a primary
  model parameter.}
  \item{z}{Values of a covariate influencing y, on which x was estimated.}
  \item{influence}{A vector of expressions (or character equivalents) stating the
  relationships among x,y,z, and possibly "...".}
  \item{coef}{Like coef in boxplot.stats, expected but ignored.}
  \item{do.out}{Logical indicating whether to plot outliers.}
  \item{transform}{An expression (or character) used to transform all stats.}
  \item{file}{Path (as necessary) and name of a file in which to append the actual
  values calculated.}
  \item{\dots}{Extra arguments available to influence expressions.}
}
\details{
Any substitute for boxplot.stats() must return 5 values representing a 
lower whisker, a lower box edge, a box center, an upper box edge, and an upper 
whisker.

covplot.stats() accepts the arguments expected by boxplot.stats().  Additionally,
it requires y, z, and influence.  If z is atomic, covplot.stats() treats the 
covariate is categorical; otherwise it treats the covariate as continuous.

For categorical covariates, the box has zero width.  The whiskers are the the 95
percentiles of the influence expression evaluated on all x; box center is the 
corresponding median.

For continuous covariates, x takes on its median value, then the box ends are 
the influence expression evaluated at the 95 percentiles of z; box center is 
evaluated at the median of z.. The whiskers are, again, the 95 percentiles of 
the influence expression evaluated on all x, but repeated for the 95 percentiles
of z and retaining the extremes of the four results (which are not predictably 
ordered in the presence of negative arguments).
}
\value{
Like the return value of boxplot.stats().
}
\author{Tim Bergsma}
\note{
The transform expression must contain at least "x". Nothing will be written to 
file if file==NULL. Like boxplot.stats(), covplot.stats() does not do "conf" 
(notches): these are set to the values of the box ends.
}
\seealso{
	\code{\link{as.distribution}},
	\code{\link{bwplot.distribution}},
	\code{\link{panel.covplot}}
	\code{\link{boxplot.stats}}
	\code{\link{bwplot}}
}
\keyword{manip}

